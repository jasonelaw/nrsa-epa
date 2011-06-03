# testSuiteNRSAValidation.r
#
# Defines and runs the test suite for individual NRSA table validation functions
#
# 10/20/09 cws Created.
# 10/28/09 cws Added files *NRSAValidation*.r
#

require(RUnit)

testSuiteNRSAValidation <- function()
# Define and run the test suite for the db*() functions
#
# Results are saved to timestamped HTML file
{
  testSuite <- defineTestSuite('stVal*'
                              ,dirs='l:/Priv/CORFiles/IM/Rwork/nrsa/code'
                              ,testFileRegexp="^val.*\\.r$|^.*NRSAValidation.*\\.r$"
                              ,testFuncRegexp="^.+Test$"
                              )

  testResult <- runTestSuite(testSuite)

  testResultFile <- 'l:/Priv/CORFiles/IM/Rwork/nrsa/docs/testResults_NRSAValidation.html'
  printHTMLProtocol(testResult, fileName=testResultFile)

}
