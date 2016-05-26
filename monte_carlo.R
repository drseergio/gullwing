# monte_carlo.R - Monte Carlo simulation for parameter optimization
#
# 2012 Copyright Sergey Pisarenko (drseergio@gmail.com)
#
# Invoke with following CLI arguments:
# $ Rscript catalyst.R <START> <TARGET> <MC> <DEBUG> <PATH_DB> <PATH_ENGINE> <PATH_CATALYST> <PATH_MC>
#
# <START> - the starting date of the search
# <TARGET> - typically today's date
# <MC> - multi-core TRUE/FALSE
# <DEBUG> - provide output in stdout
# <PATH_DB> - path to file with DB configuration (yaml)
# <PATH_ENGINE> - path to file with engine.R parameters configuration (yaml)
# <PATH_CATALYST> - path to file with catalyst.R parameters configuration (yaml)
# <PATH_MC> - path to file with monte_carlo.R parameters configuration (yaml)
#
# To invoke from R shell:
# > arguments <- c('START_DATE', ... , 'PATH_MC')
# > source('monte_carlo.R')
#
# The program uses pistons.R to carry out tasks.


require(quantmod)
require(TTR)
require(RMySQL)
require(FinancialInstrument)
require(blotter)
require(quantstrat)
require(yaml)

source('pistons.R')


# arguments
if (!exists('arguments')) arguments <- commandArgs(TRUE)
if (length(arguments) > 0) {
  from          <- arguments[1]              # begin date of search
  end           <- as.Date(arguments[2])     # today's date
  domc          <- as.logical(arguments[3])  # use multicore
  dodiag        <- as.logical(arguments[4])  # print-out progress

  # databases configuration
  db_yaml       <- arguments[5]
  db_config     <- yaml.load_file(db_yaml)
  db_src        <- db_config$db_src
  db_dst        <- db_config$db_dst

  # adjustable engine.R parameters
  params_yaml   <- arguments[6]
  params_config <- yaml.load_file(params_yaml)

  stocks.all    <- params_config$stocks_all       # whether to use all screened stocks
  stocks.sample <- params_config$stocks_sample    # whether to use a random selection of screened stocks
  stocks.list   <- params_config$stocks_list      # whether to use an explicit list of stocks

  # adjustable catalyst.R parameters
  catalyst_yaml   <- arguments[7]
  catalyst_config <- yaml.load_file(catalyst_yaml)

  # adjustable monte_carlo.R configuration & search spaces
  mc_yaml    <- arguments[8]
  mc.config  <- yaml.load_file(mc_yaml)

  vol.mults    <- eval(parse(text=mc.config$vol_mults))
  vol.smas     <- eval(parse(text=mc.config$vol_smas))
  hold.days    <- eval(parse(text=mc.config$hold_days))
  price.smas.s <- eval(parse(text=mc.config$price_smas_s))
  price.smas.l <- eval(parse(text=mc.config$price_smas_l))
  search.days  <- eval(parse(text=mc.config$search_days))
  after.days   <- eval(parse(text=mc.config$after_days))
  close.mults  <- eval(parse(text=mc.config$close_mults))
  macd.mults   <- eval(parse(text=mc.config$macd_mults))
  profit.mults <- eval(parse(text=mc.config$profit_mults))
  atr.days     <- eval(parse(text=mc.config$atr_days))
  atr.shifts   <- eval(parse(text=mc.config$atr_shifts))
  atr.maximums <- eval(parse(text=mc.config$atr_maximums))

  mc.order   <- mc.config$random     # randomize search order or not
  runs       <- mc.config$runs       # how many runs to execute
  cores      <- params_config$cores  # cores to be used per simulation
  mc.cores   <- mc.config$cores      # basically how many simulations simultaneously
}


# concurrency configuration
if (domc == TRUE) {
  require(parallel)
  options(mc.cores=cores)
}


# clean-up
try(rm(list=ls(pos=.strategy), pos=.strategy), silent=TRUE)


if (!exists('stock_symbols')) {
  # load list of pre-screened stocks from DB
  watched <- LoadScreenedDB(db_src)
  watched <- FilterStocks(watched, stocks.all, stocks.sample, stocks.list)


  # load market data
  setDefaults(getSymbols.MySQL, user=db_src$user, password=db_src$pass, dbname=db_src$name)
  day.start     <- max(vol.smas, price.smas.s, price.smas.l)
  stock_symbols <- LoadSymbols(from, day.start, domc, dodiag, mc.cores)
}


# create parameter space
params       <- list(
    vol.mults=vol.mults,
    vol.smas=vol.smas,
    hold.days=hold.days,
    price.smas.s=price.smas.s,
    price.smas.l=price.smas.l,
    search.days=search.days,
    after.days=after.days,
    close.mults=close.mults,
    macd.mults=macd.mults,
    profit.mults=profit.mults,
    atr.days=atr.days,
    atr.shifts=atr.shifts,
    atr.maximums=atr.maximums)

params.space  <- CreateParameterSpace(mc.order, params, runs)
market.days   <- GetMarketDays(from, end)

# run monte-carlo simulation
RunSimulation <- function(x) {
  time.start  <- proc.time()
 
  vol.mult    <- as.numeric(x[1])
  vol.sma     <- as.numeric(x[2])
  hold.days   <- as.numeric(x[3])
  price.sma.s <- as.numeric(x[4])
  price.sma.l <- as.numeric(x[5])
  search.days <- as.numeric(x[6])
  after.days  <- as.numeric(x[7])
  close.mult  <- as.numeric(x[8])
  macd.mult   <- as.numeric(x[9])
  profit.mult <- as.numeric(x[10])
  atr.days    <- as.numeric(x[11])
  atr.shift   <- as.numeric(x[12])
  atr.max     <- as.numeric(x[13])

  s           <- CreateStrategy(day.start,
      vol.mult, vol.sma,
      price.sma.s, price.sma.l,
      search.days, after.days,
      close.mult, macd.mult,
      atr.days, atr.shift, atr.max)
  results     <- ExecuteStrategy(s, stock_symbols, NULL, domc, FALSE)
  returns     <- FastTradeSim(results, market.days, hold.days, profit.mult)

  trades      <- length(returns)
  wins        <- length(returns[returns > 0])
  lose        <- length(returns[returns <= 0])
  target      <- length(returns[returns > (profit.mult - 1)])

  run.time    <- (proc.time() - time.start)[3]

  if (trades > 0) {
    score <- sum(returns)
    SD    <- sd(returns)
    mn    <- mean(returns)
    md    <- median(returns)
    iqr   <- IQR(returns)
  } else {
    score <- NA
    SD    <- NA
    mn    <- NA
    md    <- NA
    iqr   <- NA
  }

  SaveSimulationResults(db_dst, from, end,
      price.sma.s, price.sma.l, vol.sma, vol.mult, after.days, macd.mult, close.mult,
      atr.days, atr.shift, atr.max,
      search.days, hold.days, profit.mult, score, trades, target, wins, lose,
      SD, md, md, iqr, run.time, returns, length(stock_symbols))

  data.frame(
      trades=trades,
      target=target,
      wins=wins,
      lose=lose,
      score=score,
      SD=SD,
      mean=mn,
      median=md,
      IQR=iqr)}

if (domc == TRUE) {
  mc_result <- mclapply(params.space, RunSimulation, mc.cores=mc.cores)
} else {
  mc_result <- lapply(params.space, RunSimulation)
}

# FOR DEBUG (WHEN PARALLEL SHOOTS YOU IN THE FOOT)
#for (i in 1:length(params.space)) {
#  RunSimulation(params.space[[i]])
#}
