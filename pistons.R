# pistons.R - core routines for engine.R
#
# 2012 Copyright Sergey Pisarenko (drseergio@gmail.com)
#
# Contains main routines called by engine.R

require(delftfews)


#**
# TRADING ROUTINES
#**

# Volatility indicator
MovInd <- function(x=mktdata, days=days, shift=shift) {
  signal <- shift.vector(ATR(x, days)[, 2], by=shift)
  reclass(signal, x)
}


# Detects price drops and increases in trading volume.
BreakoutSig <- function(label, data,
    day.start, volMult, search.days, atr.max,
    db_config=db_config, symbol=symbol, dodiag=dodiag) {

  n          <- nrow(data)

  dates      <- vector(length=n)
  min.prices <- vector(length=n)
  min.dates  <- vector(length=n)

  in.zone    <- -1
  bo.date    <- NULL
  min.price  <- NULL
  min.date   <- NULL

  smaLong    <- match.names('SMAL', colnames(data))
  smaShort   <- match.names('SMAS', colnames(data))
  vma        <- match.names('VMA', colnames(data))
  closeCol   <- match.names('Adjusted', colnames(data))
  volCol     <- match.names('Volume', colnames(data))
  atrCol     <- match.names('MovInd', colnames(data))

  breakouts <- c(
      (data[, atrCol]   < atr.max) &
      (data[, closeCol] < data[, smaLong]) &
      (data[, volCol]   > data[, vma] * volMult) &
      (data[, smaShort] < data[, smaLong]))
  breakouts[1:day.start] <- FALSE       # skip days where SMA data is not yet available
  breakouts[is.na(breakouts)] <- FALSE  # remove NAs

  # this for-loop already takes advantage of vectorization
  for (i in day.start:length(breakouts)) {
    # breakout detected
    if (breakouts[i]) {
      close      <- data[i, closeCol]
      today_date <- index(close)  # human-readable date

      if (!is.null(db_config)) {
        SaveBreakoutDB(db_config, symbol, today_date, close)
      }
      if (dodiag) {
        print(paste('Breakout', symbol, '@', close, today_date))
      }

      in.zone   <- i
      bo.date   <- as.numeric(today_date)
      min.date  <- bo.date
      min.price <- close
      next
    }

    # create breakout zones
    if (in.zone != -1) {
      close     <- data[i, closeCol]
      close.val <- as.numeric(close)

      if (min.price > close.val) {
        min.price <- close.val
        min.date  <- as.numeric(.indexday(close))
      }

      dates[i]      <- bo.date    # store date of original breakout
      min.prices[i] <- min.price  # store minimal price found so far
      min.dates[i]  <- min.date   # store date when minimal price was found

      if ((i - in.zone) > search.days) {
        in.zone <- -1
      }
    }
  }

  return(xts(cbind(dates, min.prices, min.dates), order.by=index(data)))
}


# Finds buy signals.
BuySig <- function(label, data,
    after.days, close.mult, macd.mult,
    db_config=db_config, symbol=symbol, dodiag=dodiag) {

  n           <- nrow(data)

  smaShort    <- match.names('SMAS', colnames(data))
  macd        <- match.names('MACD.macd', colnames(data))
  macdSig     <- match.names('MACD.signal', colnames(data))
  breakoutCol <- match.names('breakout.sig.min.prices', colnames(data))
  breakoutDa  <- match.names('breakout.sig.min.dates', colnames(data))
  breakoutBd  <- match.names('breakout.sig.dates', colnames(data))
  closeCol    <- match.names('Adjusted', colnames(data))

  buys <- c(
      (as.numeric(.indexday(data)) > (data[, breakoutDa] + after.days)) &
      (data[, breakoutCol]         != 0) &
      (data[, closeCol]            > data[, breakoutCol] * close.mult) &
      (data[, macd]                > data[, macdSig]) &
      (data[, closeCol]            > data[, smaShort] * macd.mult))

  for (i in 1:length(buys)) {
    if (buys[i]) {
      close      <- data[i, closeCol]
      today      <- index(close)  # human-readable date
      min.date   <- as.Date(as.numeric(data[i, breakoutDa]))
      breakout   <- as.Date(as.numeric(data[i, breakoutBd]))

      if (!is.null(db_config)) {
        SaveBuyDB(db_config, symbol, today + 1, min.date, breakout, close)
      }
      if (dodiag) {
        print(paste('Buy', symbol, '@', today, close))
      }
    }
  }

  return(xts(buys, order.by=index(data)))
}


CreateStrategy <- function(day.start,
    volMult, volSMA,
    price.sma.s, price.sma.l,
    search.days, after.days,
    close.mult, macd.mult,
    atr.days, atr.shift, atr.max) {

  s <- strategy('s')

  # add technical indicators
  s <- add.indicator(
      strategy=s, name='SMA', arguments=list(x=quote(Ad(mktdata)), n=price.sma.s), label='SMAS')
  s <- add.indicator(
      strategy=s, name='SMA', arguments=list(x=quote(Ad(mktdata)), n=price.sma.l), label='SMAL')
  s <- add.indicator(
      strategy=s, name='SMA', arguments=list(x=quote(Vo(mktdata)), n=volSMA), label='VMA')
  s <- add.indicator(
      strategy=s, name='MACD', arguments=list(x=quote(Ad(mktdata))), label='MACD')
  s <- add.indicator(
      strategy=s, name='MovInd', arguments=list(
          x=quote(HLC(mktdata)), days=atr.days, shift=atr.shift), label='MovInd')

  # add our signals
  s <- add.signal(
      strategy=s,
      name='BreakoutSig',
      arguments=list(
          data=quote(mktdata),
          day.start=day.start,
          volMult=volMult,
          search.days=search.days,
          atr.max=atr.max),
      label='breakout.sig')
  s <- add.signal(
      strategy=s,
      name='BuySig',
      arguments=list(
          data=quote(mktdata),
          after.days=after.days,
          close.mult=close.mult,
          macd.mult=macd.mult),
      label='buy.sig')

  return(s)
}


ExecuteStrategy <- function(strategy, symbols, db_dst, domc, dodiag) {
  run_signals <- function(symbol, db_config, dodiag) {
    mktdata    <- get(symbol, envir=.GlobalEnv)
    indicators <- applyIndicators(strategy=strategy, mktdata=mktdata)
    signals    <- applySignals(
        strategy=strategy,
        db_config=db_config,
        mktdata=indicators,
        indicators,
        symbol=symbol)
  }

  if (domc == TRUE) {
    mclapply(symbols, run_signals, db_config=db_dst, dodiag=dodiag)
  } else {
    lapply(symbols, run_signals, db_config=db_dst, dodiag=dodiag)
  }
}


FilterStocks <- function(watched, stocks.all, stocks.sample, stocks.list) {
  if (!stocks.all) {
    if (stocks.sample != 0) {
      return(watched[sample(length(watched), stocks.sample)])
    } else if (length(stocks.list) > 0) {
      return(stocks.list)
    }
  }
  return(watched)
}


#**
# CATALYST/MC ROUTINES
#**

# creates a list of all buys (list of vectors)
CreateOpportunityList <- function(results) {
  buys <- list()
  for (mktdata in results) {
    symbol <- gsub('\\.Open', '', colnames(mktdata)[1])
    buyCol <- ncol(mktdata)
    buy_col <- mktdata[, buyCol]
    instrument_buys <- which(buy_col == 1)
    buy_dates <- sapply(instrument_buys, function(x) index(buy_col[x]))
    for (date in buy_dates) {
      if (length(buys) < date) {
        buys[[date]] <- c(symbol)
      } else {
        buys[[date]] <- c(buys[[date]], symbol)
      }
    }
  }
  return(buys)
}


GetMarketDays <- function(from, target) {
  abars <- getSymbols(src='yahoo', Symbols=c('SPY'), auto.assign=FALSE, from=from)
  bars  <- abars[index(abars) >= as.Date(from) & index(abars) <= as.Date(target)]
  return(index(bars))
}


CreateParameterSpace <- function(random, params, runs) {
  Shuffle <- function(x) {
    return(x[sample(length(x))])
  }

  if (random) {
    params <- lapply(params, Shuffle)
  }

  dimension <- prod(unlist(lapply(params, length)))
  params.space <- NULL

  y    <- 0
  each <- 0
  for (i in 1:length(params)) {
    N    <- length(params[[i]])
    if (N == 1) {
      params.space <- cbind(params.space, rep(params[[i]], dimension))
    } else {
      if (y == 0) {
        each <- dimension / N
	reps <- 1
      } else {
        each <- each / N
	reps <- (dimension / N) / each
      }

      params.space <- cbind(params.space, rep(rep(params[[i]], each=each), reps))
      y            <- y + 1
    }
  }

  if (random) {
    df <- params.space[sample(dimension, runs), ]
    return(split(df, 1:nrow(df)))
  } else {
    return(split(params.space, 1:nrow(params.space)))
  }
}


#**
# TRADE SIMULATORS
#**

# Fast trade simulator, uses vectorization and should be 5x faster than
# the other simulator.
FastTradeSim <- function(mktdata, market.days, hold.days, profit.mult) {
  TradeSymbol <- function(data) {
    symbol   <- gsub('\\.Open', '', colnames(data)[1]) 
    buyCol   <- ncol(data)

    FindSell <- function(i) {
      prices                  <- as.vector(data[, 4])
      len                     <- length(prices)
      days                    <- length(market.days)
      if (len > days) len <- days  # if there are prices for more days than S&P

      prices.d                <- vector(length=days)
      prices.d[1:len]         <- prices[1:len]
      indexes                 <- vector(length=days)
      indexes[1:len]          <- 1:len
      indexes.d               <- indexes
      indexes.d[(len-1):days] <- 0

      price.buy <- prices[i]

      cond      <- c(
          (prices.d               > price.buy * profit.mult) |  # profit target reached
          (indexes                >= (i + hold.days)) |         # max hold period exceeded
          (1:length(market.days)) != indexes.d)                 # company delisted, buy on last day

      for (y in i:len) {
        if (cond[y]) return(y)
      }
    }

    buys  <- which(data[, buyCol] != 0)
    buys  <- buys[buys < length(market.days)]  # avoid buys on the last trading day
    sells <- sapply(buys, FindSell)

    if (length(buys) == 0) return(c())
    returns <- vector(length=length(buys))
    returns <- -999999

    last.sell <- 0
    for (i in 1:length(buys)) {
      buy <- buys[i]

      if (buy > last.sell && buy < nrow(data)) {
        row.buy    <- data[buy + 1, ]
        price.buy  <- as.numeric(row.buy[, 4])
        day.buy    <- index(row.buy)

        sell       <- sells[i]
        row.sell   <- data[sell + 1, ]
        price.sell <- as.numeric(row.sell[, 4])
        day.sell   <- index(row.sell)
        delta      <- price.sell - price.buy
        ret        <- round(delta / price.buy, 2)
        returns[i] <- ret

        if (dodiag) {
          print(paste('BUY', day.buy, symbol, '@', price.buy))
          print(paste('SELL', day.sell, symbol, '@', price.sell, paste('[', ret, ']', sep='')))
        }

        last.sell <- sells[i]
      }
    }

    return(returns[returns != -999999])
  }

  returns <- unlist(sapply(mktdata, TradeSymbol, simplify=TRUE))
  if (is.null(returns)) {
    return(c())
  } else {
    return(returns[!is.na(returns)])
  }
}


# Much slower trade simulator, does not use vectorization. Has support for accounting and
# cash. If capital is specified the simulator uses cash.
FullTradeSim <- function(symbols, buy_list, market.days, capital=NULL) {
  # initialize portfolio
  portfolio    <- InitPortfolio()

  if (!is.null(capital)) {
    cash.sim <- TRUE
    cash     <- capital
  } else {
    cash.sim <- FALSE
  }

  run_days <- market.days[-((length(market.days)-3):length(market.days))]

  for (i in 1:length(run_days)) {
    date      <- as.Date(market.days[i])
    next_date <- as.Date(market.days[i+1])
    day_num   <- market.days[i]

    # if we have opportunities today
    if (day_num <= length(buy_list) && !is.null(buy_list[[day_num]])) {
      buys     <- buy_list[[day_num]]

      if (cash.sim) {
        rand   <- sample(1:length(buys), 1)
        symbol <- buys[[rand]]
        bars     <- get(symbol, env=.GlobalEnv)

        if (!HasPosition(portfolio, symbol)) {
          print(paste('We currently have', cash))

          # check that we have enough price data
          prices    <- bars[, 6]                         # open prices, don't use Ad()!
          if (date != tail(index(prices), 1)) {
            open_next <- as.numeric(
                prices[prices[date, which.i=TRUE] + 1])      # opening price next market day
            quantity  <- GetTradeSize(                       # how many stocks to buy
                portfolio,
                open_next,
                tradesize=tradesize)

            amount <- (quantity * open_next) + GetFeeSize(quantity, fees)
            if (length(open_next) != 0 && cash > amount) {
              portfolio <- AddBuyOrder(portfolio, symbol, next_date, quantity, open_next, fees)
              cash      <- cash - amount
            }
          }
        }
      } else {
        for (symbol in buys) {
          bars     <- get(symbol, env=.GlobalEnv)

          # check if we own this stock; if not then try to buy it
          if (!HasPosition(portfolio, symbol)) {
            # check that we have enough price data
            prices    <- bars[, 6]                         # open prices, don't use Ad()!
            if (date != tail(index(prices), 1)) {
              open_next <- as.numeric(
                  prices[prices[date, which.i=TRUE] + 1])      # opening price next market day
              quantity  <- GetTradeSize(                       # how many stocks to buy
                  portfolio,
                  open_next,
                  tradesize=tradesize)

              if (length(open_next) != 0) {
                portfolio <- AddBuyOrder(portfolio, symbol, next_date, quantity, open_next, fees)
              }
            }
          }
        }
      }

    }

    # get over existing positions and sell if we meet criteria
    for (symbol in GetPositionSymbols(portfolio)) {
      bars       <- get(symbol, env=.GlobalEnv)
      prices     <- bars[, 6]  # open prices
      open_next  <- tryCatch(as.numeric(prices[prices[date, which.i=TRUE] + 1]), error=function(e) NULL)

      position   <- GetPosition(portfolio, symbol)
      buy_price  <- position[2]
      buy_date   <- as.Date(position[3])
      curr_price <- as.numeric(Ad(bars)[date])

      # company is bought/delisted, use last available date
      if (is.null(open_next)) {
        last_data <- last(bars)
        open_last <- as.numeric(last_data[, 6])
        last_date <- index(last(bars))
        if (!cash.sim) {
          portfolio <- AddSellOrder(portfolio, symbol, next_date, open_last, fees)
        } else {
          portfolio <- AddSellOrder(portfolio, symbol, next_date, open_last, fees)
          quantity  <- position[1]
          cash      <- cash + (open_last * quantity) - GetFeeSize(quantity, fees)
        }
        next
      }

      # sell because of hold time limit
      if ((date-buy_date) > hold.days.c) {
        if (!cash.sim) {
          portfolio <- AddSellOrder(portfolio, symbol, next_date, open_next, fees)
        } else {
          portfolio <- AddSellOrder(portfolio, symbol, next_date, open_next, fees)
          quantity  <- position[1]
          cash      <- cash + (open_next * quantity) - GetFeeSize(quantity, fees)
        }
        next
      }

      # sell because profit reached
      if ((length(curr_price) > 0) && (curr_price >= buy_price * profit)) {
        if (!cash.sim) {
          portfolio <- AddSellOrder(portfolio, symbol, next_date, open_next, fees)
        } else {
          portfolio <- AddSellOrder(portfolio, symbol, next_date, open_next, fees)
          quantity  <- position[1]
          cash      <- cash + (open_next * quantity) - GetFeeSize(quantity, fees)
        }
        next
      }
    }
  }

  # eliminate any positions at current closing prices
  for (symbol in GetPositionSymbols(portfolio)) {
    bars      <- get(symbol, env=.GlobalEnv)
    last_data <- last(bars)
    last_date <- last(market.days)

    if (!cash.sim) {
      portfolio <- AddSellOrder(portfolio, symbol, last_date, as.numeric(last_data[, 4]), fees)
    } else {
      position  <- GetPosition(portfolio, symbol)
      portfolio <- AddSellOrder(portfolio, symbol, last_date, as.numeric(last_data[, 4]), fees)
      quantity  <- position[1]
      cash      <- cash + (as.numeric(last_data[, 4]) * quantity) - GetFeeSize(quantity, fees)
    }
  }

  if (cash.sim) {
    print(paste('We have ended-up with', cash, 'cash'))
  }

  return(portfolio$transactions)
}


# TODO(drseergio): finish implementation
GetTradeSize <- function(portfolio, price, tradesize=NULL, cash=NULL) {
  if (!is.null(tradesize)) {
    return(round(tradesize / price))
  }

  #total <- GetTotalEquity(portfolio) + cash
}


GetFeeSize <- function(quantity, fees) {
  return(quantity * fees)
}


AnalyzeTrades <- function(trades) {
  total_trades <- 0
  total_target <- 0
  total_wins   <- 0
  total_lose   <- 0
  returns      <- c()

  sells <- trades[trades$pl != 0, ]
  if (nrow(sells) == 0) return(NULL)

  for (y in 1:nrow(sells)) {
    trade <- sells[y,]
    pl    <- trade$pl
    val   <- trade$val

    total_trades <- total_trades + 1
    if (pl > 0) {
      total_wins <- total_wins + 1
    } else {
      total_lose <- total_lose + 1
    }
    delta <- round(pl / (abs(pl + val)), 2)
    if (delta > (profit - 1)) total_target <- total_target + 1
    returns <- c(returns, delta)
  }

  return(list(
      trades=total_trades,
      target=total_target,
      wins=total_wins,
      lose=total_lose,
      returns=returns))
}




SaveTradesCSV <- function(trades, filename) {
  FindReturns <- function(trade) {
    pl  <- as.numeric(trade[6])
    val <- as.numeric(trade[5])
    return(round(pl / (abs(pl + val)), 2))
  }

  sells <- trades[trades$pl != 0, ]
  if (nrow(sells) == 0) return(NULL)
  sells <- cbind(sells, apply(sells, 1, FindReturns))
  
  write.csv(sells, filename, row.names=FALSE)
}


SaveSimulationResults <- function(db.config, from, end,
    price.sma.short, price.sma.long,
    volume.sma, volume.multiplier,
    buy.after,
    macd.multiplier, lowest.multiplier,
    atr.days, atr.shift, atr.max,
    search.days, hold.days, profit.mult, score,
    trades, target, wins, lose,
    SD, mn, md, iqr, run.time, returns, symbols) {

  if (dodiag) print('Saving simulation result to DB')

  win_ratio    <- round(wins / trades, 2)
  lose_ratio   <- round(lose / trades, 2)
  target_ratio <- round(target / trades, 2)

  conn      <- dbConnect(
      dbDriver('MySQL'),
      dbname=db.config$name,
      user=db.config$user,
      password=db.config$pass)

  row        <- data.frame(from, end,
      price.sma.short, price.sma.long, volume.sma, volume.multiplier,
      buy.after, macd.multiplier, lowest.multiplier, atr.days, atr.shift, atr.max,
      search.days, hold.days, profit.mult,
      score, trades, target, target_ratio, wins, win_ratio, lose, lose_ratio,
      SD, mn, md, iqr, run.time,
      toString(returns), symbols)
  names(row) <- c('start_date', 'end_date',
      'price_sma_short', 'price_sma_long', 'volume_sma', 'volume_multiplier',
      'buy_after', 'macd_multiplier', 'lowest_multiplier',
      'atr_days', 'atr_shift', 'atr_max', 'search_days', 'hold_days',
      'profit.mult', 'score', 'trades', 'target', 'target_ratio', 'wins', 'win_ratio',
      'lose', 'lose_ratio', 'sd', 'mean', 'median',
      'IQR', 'run.time', 'returns', 'symbols_num')

  dbWriteTable(conn, 'simulations', value=row, row.names=FALSE, append=TRUE)
  dbDisconnect(conn)
}


#**
# PORTFOLIO MANAGEMENT
#**

AddBuyOrder <- function(portfolio, symbol, date, qty, price, fees) {
  if (dodiag) print(paste(date, symbol, qty, '@', price))

  portfolio$transactions <- rbind(
      portfolio$transactions,
      data.frame(
          symbol=symbol, sdate=date, qty=qty, price=price, val=qty * price,
          pl=0, fees=GetFeeSize(qty, fees), bdate=as.Date(0)))

  portfolio$positions[[symbol]] <- c(qty=qty, price=price, date=date)

  return(portfolio)
}


AddSellOrder <- function(portfolio, symbol, date, price, fees) {
  position               <- portfolio$positions[[symbol]]
  qty                    <- position[1]
  bdate                  <- position[3]
  pl                     <- GetPL(qty, position[2], price)

  if (dodiag) print(paste(date, symbol, -qty, '@', price))

  portfolio$transactions <- rbind(
      portfolio$transactions,
      data.frame(
          symbol=symbol, sdate=date, qty=-qty, price=price, val=-qty * price,
          pl=pl, fees=GetFeeSize(qty, fees), bdate=as.Date(bdate)))

  portfolio$positions[[symbol]] <- NULL

  return(portfolio)
}


GetPL <- function(qty, orig.price, curr.price) {
  return((curr.price - orig.price) * qty)
}


HasPosition <- function(portfolio, symbol) {
  return(!is.null(portfolio$positions[[symbol]]))
}


GetPosition <- function(portfolio, symbol) {
  return(portfolio$positions[[symbol]])
}


GetPositionSymbols <- function(portfolio) {
  return(names(portfolio$positions))
}


GetTotalEquity <- function(portfolio) {
  if (ncol(portfolio$positions) == 0) {
    return(0)
  } else {
    return(sum(portfolio$positions[1, ] * portfolio$positions[2, ]))
  }
}


InitPortfolio <- function() {
  transactions        <- data.frame(t(rep(NA, 8)))
  names(transactions) <- c('symbol', 'sdate', 'qty', 'price', 'val', 'pl', 'fees', 'bdate')
  transactions        <- transactions[-1, ]

  positions           <- data.frame(SEMURG=c(1,2,3))
  positions           <- positions[, -1]

  portfolio           <- list(transactions=transactions, positions=positions)
  return(portfolio)
}


#**
# DATABASE OPERATIONS
#**

ClearResultsDB <- function(db_config) {
  conn      <- dbConnect(
      dbDriver('MySQL'),
      dbname=db_config$name,
      user=db_config$user,
      password=db_config$pass)
  dbSendQuery(conn, 'DELETE FROM breakouts')  # delete previous results
  dbSendQuery(conn, 'DELETE FROM buys')       #
  dbDisconnect(conn)
}


LoadScreenedDB <- function(db_config) {
  src       <- dbConnect(
      dbDriver('MySQL'),
      dbname=db_config$name,
      user=db_config$user,
      password=db_config$pass)
  watched   <- dbListTables(src)
  dbDisconnect(src)
  return(watched)
}


LoadSymbols <- function(from, day.start, domc, dodiag, cores) {
  from <- as.Date(from) - day.start  # add extra days for indicator computations

  load_symbol <- function(symbol) {
    ubars    <- tryCatch(
        getSymbols(symbol, src='MySQL', auto.assign=FALSE, from=from),
        error=function(e) NULL)
    # skip if we don't have enough data
    if (is.null(ubars) || nrow(ubars) < day.start) return(NULL)
    bars     <- ubars[index(ubars) >= as.Date(from)]
    # skip if we don't have enough after filtering
    if (nrow(bars) < day.start) return(NULL)

    if (dodiag == TRUE) print(paste('Loaded', symbol))
    return(list(symbol=symbol, bars=bars))
  }

  symbols <- c()
  if (domc == TRUE) {
    to_load <- mclapply(watched, load_symbol, mc.cores=cores)
  } else {
    to_load <- lapply(watched, load_symbol)
  }

  for (data in to_load) {
    if (!is.null(data)) {
      symbols <- c(symbols, data$symbol)
      assign(data$symbol, data$bars, env=.GlobalEnv)
    }
  }

  return(symbols)
}


SaveBreakoutDB <- function(db_config, symbol, date, close) {
  row        <- data.frame(symbol, date, close)
  names(row) <- c('symbol', 'date', 'close')

  conn       <- dbConnect(
      dbDriver('MySQL'),
      dbname=db_config$name,
      user=db_config$user,
      password=db_config$pass)
  dbWriteTable(conn, 'breakouts', value=row, row.names=FALSE, append=TRUE)
  dbDisconnect(conn)
}


SaveBuyDB <- function(db_config, symbol, date, ddate, bdate, price) {
  row        <- data.frame(symbol, date, ddate, bdate, price)
  names(row) <- c('symbol', 'date', 'ddate', 'bdate', 'price')

  conn       <- dbConnect(
      dbDriver('MySQL'),
      dbname=db_config$name,
      user=db_config$user,
      password=db_config$pass)
  dbWriteTable(conn, 'buys', value=row, row.names=FALSE, append=TRUE)
  dbDisconnect(conn)
}
