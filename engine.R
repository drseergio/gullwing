# engine.R - discover buy situations in the market
#
# 2012 Copyright Sergey Pisarenko (drseergio@gmail.com)
#
# Invoke with following CLI arguments:
# $ Rscript engine.R <START_DATE> <TARGET_DATE> <DO_MC> <DEBUG> <PATH_DB> <PATH_PARAMS>
#
# <START_DATE> - the starting date of the search
# <TARGET_DATE> - typically today's date
# <DO_MC> - multi-core TRUE/FALSE
# <DEBUG> - provide output in stdout
# <PATH_DB> - path to file with DB configuration (yaml)
# <PATH_PARAMS> - path to file with parameters configuration (yaml)
#
# To invoke from R shell:
# > arguments <- c('START_DATE', ... , 'PATH_PARAMS')
# > source('engine.R')
#
# The program makes certain assumptions about DB schema


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
  target        <- as.Date(arguments[2])     # today's date
  domc          <- as.logical(arguments[3])  # use multicore
  dodiag        <- as.logical(arguments[4])  # print-out progress

  # databases configuration
  db_yaml       <- arguments[5]
  db_config     <- yaml.load_file(db_yaml)
  db_src        <- db_config$db_src
  db_dst        <- db_config$db_dst

  # adjustable parameters
  params_yaml   <- arguments[6]
  params_config <- yaml.load_file(params_yaml)

  shortSMA      <- params_config$short_sma        # short SMA
  longSMA       <- params_config$long_sma         # long SMA
  volSMA        <- params_config$volume_sma       # volume SMA
  volMult       <- params_config$volume_mult      # expect at least X fold increase in volume
  buyAfter      <- params_config$buy_after        # only buy after X days since breakout occurred
  macdMult      <- params_config$macd_mult        # expect MACD to be X-fold higher than signal MACD
  closeMult     <- params_config$close_mult       # close price must be X-fold higher than lowest low
  search.days   <- params_config$search_days      # maximum number of days to look for buy after drop
  atr.days      <- params_config$atr_days         # number of days for ATR calculation
  atr.shift     <- params_config$atr_shift        # ATR of X days will be considered
  atr.max       <- params_config$atr_max          # maximum volatility to tolerate (in ATR)
  day.start     <- max(shortSMA, longSMA, volSMA,
      atr.days + atr.shift)                       # minimum amount of days data we need for calculations

  stocks.all    <- params_config$stocks_all       # whether to use all screened stocks
  stocks.sample <- params_config$stocks_sample    # whether to use a random selection of screened stocks
  stocks.list   <- params_config$stocks_list      # whether to use an explicit list of stocks

  # adjustable configuration
  cores         <- params_config$cores
}


# load list of pre-screened stocks from DB
watched <- LoadScreenedDB(db_src)
watched <- FilterStocks(watched, stocks.all, stocks.sample, stocks.list)


# concurrency configuration
if (domc == TRUE) {
  require(parallel)
  options(mc.cores=cores) # number of cores on CPU
}


# clear results database
if (!exists('in_memory') || !in_memory) ClearResultsDB(db_dst)


# unset any existing strategies
try(rm(list=ls(pos=.blotter), pos=.blotter), silent=TRUE)
try(rm(list=ls(pos=.strategy), pos=.strategy), silent=TRUE)


# load market data
setDefaults(getSymbols.MySQL, user=db_src$user, password=db_src$pass, dbname=db_src$name)
symbols <- LoadSymbols(from, day.start, domc, dodiag, cores)


# create overall trading strategy
s <- CreateStrategy(day.start,
    volMult, volSMA,
    shortSMA, longSMA,
    search.days, buyAfter,
    closeMult, macdMult,
    atr.days, atr.shift, atr.max)


# execute strategy
if (exists('in_memory') && in_memory) db_dst <- NULL
results <- ExecuteStrategy(s, symbols, db_dst, domc, dodiag)
TRUE
