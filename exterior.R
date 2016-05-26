# exterior.R - generates a chart for a given date and stock
#
# 2012 Copyright Sergey Pisarenko (drseergio@gmail.com)
#
# Invoke with following CLI arguments:
# $ Rscript exterior.R <DATE> <SYMBOL> <FILENAME> <PATH_TO_CONFIG>
#
# <DATE> - the date (which is going to be the center of chart)
# <SYMBOL> - the instrument we are working with
# <FILENAME> - where to save the chart
# <PATH_TO_CONFIG> - path to file with DB configuration (yaml)
#
# The program will include +/- 90 days around the specified date.


require(quantmod)
require(RMySQL)
require(yaml)


# command line parameters
if (!exists('arguments')) arguments <- commandArgs(TRUE)
if (length(arguments) > 0) {
  today     <- as.Date(arguments[1])
  symbol    <- arguments[2]
  filename  <- arguments[3]
  db_yaml   <- arguments[4]
  
  db_config <- yaml.load_file(db_yaml)
  db_dst    <- db_config$db_dst
}


# load market instrument data
bars <- getSymbols(src='yahoo', Symbols=c(symbol), auto.assign=FALSE)
assign(symbol, bars)


before   <- today - 90
finish   <- today + 90
print(paste(before, '::', finish, sep=''))


chartSeries(bars, subset=paste(before, '::', finish, sep=''), TA=paste(sep='',
    'addSMA(10);',
    'addSMA(5, col="blue");',
    'addVo();',
    'addATR(n=50);',
    'addVolatility();',
    'addMACD();',
    'addTA(xts(TRUE, today), on=-(1:2), col="555555")'))

saveChart('png', file=filename, width=1000, height=1000)
dev.off()
