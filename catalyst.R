# catalyst.R - create orders for back-test with buy signals
#
# 2012 Copyright Sergey Pisarenko (drseergio@gmail.com)
#
# Invoke with following CLI arguments:
# $ Rscript catalyst.R <START_DATE> <TARGET_DATE> <DO_MC> <DEBUG> <PATH_DB> <PATH_ENGINE> <PATH_CATALYST>
#
# <START_DATE> - the starting date of the search
# <TARGET_DATE> - typically today's date
# <DO_MC> - multi-core TRUE/FALSE
# <DEBUG> - provide output in stdout
# <PATH_DB> - path to file with DB configuration (yaml)
# <PATH_ENGINE> - path to file with engine.R parameters configuration (yaml)
# <PATH_CATALYST> - path to file with catalyst.R parameters configuration (yaml)
#
# To invoke from R shell:
# > arguments <- c('START_DATE', ... , 'PATH_CATALYST')
# > source('catalyst.R')
#
# The program executes engine.R as part of execution.

in_memory <- TRUE  # don't save results to DB
source('engine.R')


# load additional catalyst configuration
if (exists('arguments') && length(arguments) > 0) {
  catalyst_yaml   <- arguments[7]
  catalyst_config <- yaml.load_file(catalyst_yaml)

  tradesize       <- catalyst_config$tradesize    # buy this much stock per trade
  capital         <- catalyst_config$capital      # available trading capital
  profit          <- catalyst_config$profit       # do not sell until we reach X% profit
  fees            <- catalyst_config$fees         # transaction costs

  hold.days.c     <- catalyst_config$hold_cdays   # maximum calendar days to hold stock
  hold.days.m     <- catalyst_config$hold_mdays   # maximum market days to hold stock
}


# get market open days
market.days  <- GetMarketDays(from, target)


# run simulation
buy.list     <- CreateOpportunityList(results)
trades       <- FullTradeSim(symbols, buy.list, market.days, capital=capital)
stats        <- AnalyzeTrades(trades)

print(paste('Total trades:', stats$trades))
print(paste('Win trades:', stats$wins))
print(paste('Lose trades:', stats$lose))
print(paste('Target trades', stats$target))
print(stats$returns)
