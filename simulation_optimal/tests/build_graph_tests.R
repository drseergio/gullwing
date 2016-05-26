# build_graph_tests.R - create unit tests for graph setup functions
#
# 2012 Copyright Michael Blume (blume.michael@gmx.com)
#
# Invoke with following CLI arguments:
# $ Rscript build_graph_test.R
#

#require(RUnit)

#source('../build_graph.R')

test.annualizedInterestRate.moreThan365Days <- function() {
  # for time periods longer than a year, the function should return the interest rate itself
  checkEqualsNumeric( annualizedInterestRate( 1.05, 700 ), 1.05 )
}

test.annualizedInterestRate.lessThan365Days <- function() {
  # for less it should be (1+i)^n -i
  checkEqualsNumeric( annualizedInterestRate( 0.05, 180 ), 2.5 )
}

test.edgeWeightTransformation <- function() {
  DEACTIVATED('this function is de-activated for the moment')
  checkTrue(is.numeric("a"))
}


test.edgeWeightRansformation.consecutivity <- function() {
 checkTrue(
    edgeWeightTransformation(annualizedInterestRate(0.40, 180),180) < 
    edgeWeightTransformation(annualizedInterestRate(0.40, 90),90)
 )
}
