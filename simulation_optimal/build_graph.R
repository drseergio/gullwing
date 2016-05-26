# build_graph.R - create a graph to find the optimal investment path
#
# 2012 Copyright Michael Blume (blume.michael@gmx.de)
#
# Invoke with following CLI arguments:
# $ Rscript build_graph.R <DATE> <CASH> <DOMC> <PATH_TO_CONFIG>
#
# <DATE> - the start date of the simulation
# <CASH> - starting capital
# <DOMC> - TRUE/FALSE use multi-core [currently NOT USED]
# <PATH_TO_CONFIG> - path to file with DB configuration (yaml)
#
# This program expects that engine.R has done its work


require(RMySQL)
require(parallel)
require(yaml)
require(graph)
require(RBGL)

# arguments
args      <- commandArgs(TRUE)
if (length(args) > 0) {
  from      <- args[1]
  cash      <- as.numeric(args[2])
  domc      <- as.logical(args[3])
  config    <- args[4]
}


db_config = yaml.load_file(config)

DB_SRC_NAME = db_config$db$q_db
DB_SRC_USER = db_config$db$user
DB_SRC_PASS = db_config$db$pass

DB_NAME = db_config$db$s_db
DB_USER = db_config$db$user
DB_PASS = db_config$db$pass

DAYS_PER_YEAR = 365
DEFAULT_INTEREST = 0.01

annualizedInterestRate <- function(returnPercentage, numberOfDays) {
  # Investopedia explains the 'Effective Annualized Interest Rate' the following way:
  #   ear = (1 + (i/n) )^n  - 1
  # where 
  #    i  = the interest rate gained (i.e. 6% = 0.06) 
  #    n  = the number of periods per year (i.e. for quartlery n=4)
  # As the parameter to this function is numberOfDays the money stayed in the investment
  # we first need to convert this into number of periods per year
  if (numberOfDays > DAYS_PER_YEAR) {
    return(returnPercentage)
  } else {
    n = DAYS_PER_YEAR / numberOfDays # i.e. 360/180 = 2 periods per year
    return (( (1+ (returnPercentage/n))^n) - 1)
  }
}           

edgeWeightTransformation <- function(ear, numberOfDays) {
  # the ear (effective annualized interest rate) is usually element of [-1 <= ear <= infinity)
  # we want to map it in such a way, that
  #  (1) 2 consecutive edges of 0.01% ear are worse than 1 edge of 0.02%
  #        [reason for this are the expected transaction cost]
  #  (2) [0 <= weight <= infity] (just on positive double numbers)
  #  
  #  (3) the higher the ear, the lower the weight
  #
  #  (4) the higher the number of days, the higher the weight
  #
  #  (5) any amount of DEFAULT_INTEREST investments is better than an edge with negative ear
  #
  if (ear>=0) {
    return((1 / (1+ear))*numberOfDays)
  } else {
    return(abs(ear) * (1000000000/numberOfDays) )
  }
}


if (domc == TRUE) {
  options(mc.cores=12)
}

annualizedDefaultInterest = annualizedInterestRate(DEFAULT_INTEREST/DAYS_PER_YEAR, 1)
# start a empty graph
g1 <- new("graphNEL", edgemode="directed")
edgeDataDefaults(g1, "weight") <- edgeWeightTransformation(annualizedDefaultInterest,1)
edgeDataDefaults(g1, "inBank") <- paste('1', DEFAULT_INTEREST, sep="_") # first whether in the Bank (0/1) and after the "_" the interest rate


# initialize it with nodes for every day
previousDay = ''
print("Setting up graph with days")
for (d in as.Date(from):Sys.Date()) { 
	g1 <- addNode(as.character(as.Date(d, origin='1970-01-01')), g1)
	if (previousDay!='') {
		g1 <- addEdge(as.character(as.Date(previousDay, origin='1970-01-01')),
				      as.character(as.Date(d, origin='1970-01-01')),
			          g1, edgeWeightTransformation(annualizedDefaultInterest,1))
		edgeData(g1, as.character(as.Date(previousDay, origin='1970-01-01')),
				      as.character(as.Date(d, origin='1970-01-01')),
				attr='inBank') <- paste('1', DEFAULT_INTEREST, sep="_") # first whether in the Bank (0/1) and after the "_" the interest rate

	}
	previousDay <- d
}


# initialize DB access
conn       <- dbConnect(dbDriver('MySQL'),
  dbname=DB_NAME,
  user=DB_USER,
  password=DB_PASS)

print("Adding transactions")
# introduce edges for all transactions
transactions <- dbGetQuery(conn,paste( 
  'SELECT *,DATEDIFF(selldate,buydate) AS Duration ',
  'FROM', 
  '  (SELECT symbol AS buysymbol,date AS buydate, price as buyprice, quantity AS buyquantity FROM `transactions` WHERE type="buy") AS buys ',
  '  RIGHT JOIN ',
  '  (SELECT symbol, date AS selldate, price AS sellprice, quantity AS sellquantity FROM `transactions` WHERE type!="buy" ) AS sells ',
  '  ON buys.buysymbol=sells.symbol AND selldate>buydate AND buyquantity=sellquantity ',
  'WHERE buydate>=\'', from, '\'' ,sep=''))

t <- transactions
for (i in 1:nrow(t)) {
  returnPercentage <- ((as.numeric(t[i,"sellquantity"]) * as.numeric(t[i,"sellprice"])) - 
                       (as.numeric(t[i,"buyquantity"]) * as.numeric(t[i,"buyprice"])) ) / 
                        (as.numeric(t[i,"buyquantity"]) * as.numeric(t[i,"buyprice"]))
  effectiveAnnualInterest <- annualizedInterestRate(returnPercentage, as.numeric(t[i, "Duration"]))
  g1 <- addEdge(t[i,"buydate"], t[i,"selldate"], g1, edgeWeightTransformation(effectiveAnnualInterest, as.numeric(t[i, "Duration"])))
  edgeData(g1, t[i,"buydate"], t[i,"selldate"], attr='inBank') <- paste(0,returnPercentage,sep="_")
}

investment <- cash
lotsize <- 10000
result <- 0

while (investment >= lotsize-2) {
  investment <- investment - lotsize
  currentRun <- lotsize
  # calculate the shortest path
  allSp1<- bellman.ford.sp(g1)
  firstNode <- match(from, nodes(g1))
  lastNode <-  match(as.character(Sys.Date()), nodes(g1))

  # extract pathFromTo (will be a index reference to the nodes array
  pft <- RBGL::extractPath(firstNode, lastNode, allSp1$penult)
  #print(pft)
  #print(nodes(g1)[pft])
  lastNode <- NULL
  for (node in nodes(g1)[pft]) {

	if (!is.null(lastNode) && !is.na(node)) {
	  edge_data <- data.frame(strsplit(as.character(edgeData(g1, from=lastNode, to=node, attr='inBank')),'_'))
          percent <- as.numeric(as.matrix(edge_data[2,1]))
	  #print( as.numeric(as.matrix(edge_data[1,1])) )
	  currentRun <- currentRun * (1+percent)
	  if (as.numeric(as.matrix(edge_data[1,1]))==0) {
                print( paste("Trade: ", percent, sep=" ") )
		g1 <- removeEdge(from=lastNode, to=node, g1)
                # ALTERNATIVE: make it incredibbly hard to use this one
	 	#edgeData(g1, from=lastNode, to=node) <- 100000
	  }
        }
	lastNode <- node
  }
  print('---------------------------')
  print(currentRun)
  print('---------------------------')
  result <- result + currentRun
}
print(result)

# clean-up
dbDisconnect(conn)
