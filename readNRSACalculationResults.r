# readNRSACalculationResults.r
#
# 12/10/09 cws Created
#  3/11/10 cws separating fileName and path with /.
#

NRSAMetricsLocation <- 'l:/Priv/CORFiles/IM/Rwork/nrsa/results'

readNRSACalculationResults <- function(fileName)
# Reads data validation results from a file in the directory specified by
# NRSAMetricsLocation which contains previously calculated metrics.
#
# Returns the contents of the file as a dataframe if successful, or an
# error message if unsuccessful.
#
# ARGUMENTS:
# filePath   string with full path and filename containing validation results.
#
{

  if(substr(fileName, nchar(fileName)-3, nchar(fileName)) == '.csv') {
      rr <- read.csv(paste(NRSAMetricsLocation, fileName, sep='/')
                    ,na=''
                    ,header=TRUE
                    ,stringsAsFactors=FALSE
                    )
#      rr <- rr[!is.na(rr[1]),]
  } else  {
      cat(sprintf("Error: readNRSACalculationResults() currently only reads csv files.\n"))
      rr <- "Error: readNRSACalculationResults() currently only reads csv files.\n"
  }

  return(rr)
}
