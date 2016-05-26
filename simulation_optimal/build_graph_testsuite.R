# build_graph_tests.R - create unit tests for graph setup functions
#
# 2012 Copyright Michael Blume (blume.michael@gmx.com)
#
# Invoke with following CLI arguments:
# $ Rscript build_graph_test.R
#

library('RUnit')
 
# setup the parameters which are else parsed via CMD line
config <- "db.yaml"
from   <- "2010-01-01"
cash   <- 100000
domc   <- FALSE

# now source in the source code
source('build_graph.R')
 
# load the test suite from the sub directory
test.suite <- defineTestSuite("build_graph",
                              dirs = file.path("tests"),
                              testFileRegexp = '^\\d+\\.R')
 
# execute the tests
test.result <- runTestSuite(test.suite)
 
printTextProtocol(test.result)
