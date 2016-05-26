# build_graph_tests.R - create unit tests for graph setup functions
#
# 2012 Copyright Michael Blume (blume.michael@gmx.com)
#
# Invoke *indirectly* with following CLI arguments:
# $ Rscript ../build_graph_testsuite.R
#

test.annualizedInterestRate.moreThan365Days <- function() {
  # for time periods longer than a year, the function should return the interest rate itself
  checkEqualsNumeric( annualizedInterestRate( 1.05, 700 ), 1.05 )
}

test.annualizedInterestRate.lessThan365Days <- function() {
  # for less it should be (1+i)^n -i
  checkEqualsNumeric( annualizedInterestRate( 0.05, DAYS_PER_YEAR ), 0.05)
  checkTrue( annualizedInterestRate( 0.05, 180) > 0.05 )
}

test.edgeWeightTransformation.ordinallity <- function() {
 # ensure that the logic: 
 #    the higher the return and the less days needed, the lower the value
 # holds true for positive and negative returns

 # for positive returns: the shorter the investment period, the shorter the path
 checkTrue(
    edgeWeightTransformation(annualizedInterestRate(0.40, 90),90) < 
    edgeWeightTransformation(annualizedInterestRate(0.40, 180),180)
 )
 # for positive returns: the higher the return, the shorter the path
 checkTrue(
    edgeWeightTransformation(annualizedInterestRate(0.40, 90),90) < 
    edgeWeightTransformation(annualizedInterestRate(0.20, 90),90)
 )
 # for negative returns: the lower the number of days, the longer the path
 checkTrue(
    edgeWeightTransformation(annualizedInterestRate(-0.40, 180),180) <
    edgeWeightTransformation(annualizedInterestRate(-0.40, 90),90)
 )
 # the higher the negative return at same amount of time, the longer the path
 checkTrue(
    edgeWeightTransformation(annualizedInterestRate(-0.20, 90),90) <
    edgeWeightTransformation(annualizedInterestRate(-0.40, 90),90)
 )
 # TODO(mblume) there should a break even point at certain combiations of investment time and return
 #              it would be interesting to see, if the implemented function represents them both as equal
 #              OR (preferable, the favors the shorter investment period
}
