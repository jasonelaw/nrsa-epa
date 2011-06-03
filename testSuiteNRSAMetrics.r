# testSuiteNRSAMetrics.r
#
# Defines and runs the test suite for individual NRSA metrics calculation functions
#
# 02/18/10 cws Created.
#

require(RUnit)

testSuiteNRSAMetrics <- function()
# Define and run the test suite for the db*() functions
#
# Results are saved to timestamped HTML file
{
  testSuite <- defineTestSuite('stVal*'
                              ,dirs='l:/Priv/CORFiles/IM/Rwork/nrsa/code'
                              ,testFileRegexp="^mets.*\\.r$"
                              ,testFuncRegexp="^.+Test$"
                              )

  testResult <- runTestSuite(testSuite)

  testResultFile <- 'l:/Priv/CORFiles/IM/Rwork/nrsa/docs/testResults_NRSAMetrics.html'
  printHTMLProtocol(testResult, fileName=testResultFile)

}
