# valLegalUnitsCheck.r
#
# 11/30/09 cws Created to check for illegal units
# 12/04/09 cws Corrected name of output file and notice of completion to tell
#              user that the check is for legal units, not values.
# 12/07/09 cws Check for illegal values now takes SAMPLE_TYPE into account when
#          determining expectedUnits.
# 12/08/09 SSR Created TESTDESCRIPTION in valLegalUnitsCheck.1 and fill with
#          units expected for given parameter  
# 12/23/09 cws valLegalUnitsCheckTest victim of cut&paste error, apparently
#          overwritten by unit test for valLegalCheck.  Recreated unit test.
#          Changed check to flag missing UNITS when nonMissing UNITS is
#          expected.
#  6/11/10 cws Unit test now including UID in expected return value using
#          imperfect/real data, due to changes in constructNRSAValidationResults().
#

valLegalUnitsCheck <- function(tableName,since=NULL)
# Performs legal value checks on a data table, creating a spreadsheet file of
# issues for review in folder determined by NRSAvalidationLocation if any
# issues are discovered.
#
# Returns NULL on success, or character string describing error or if there
# are no illegal values.
#
# ARGUMENTS:
# tableName Character string with name of table to do legal validation on.
#
# ASSUMPTIONS:
# 
{
  # Retrieve data to be validated, form metadata and site information
  chan <- odbcConnect('NRSA2')
  on.exit(valLegalUnitsCheck.cleanup(chan))

    if(!is.null(since)) {
      since <- paste("INSERTION > CAST('", since, "' AS DATETIME)", sep='')
  }

  df <- fetchNRSATable(chan, tableName,where=since)
  if(!is.data.frame(df)) return(df)
  
  meta.df <- fetchNRSATable(chan, 'tblPARAMETERDESCRIPTIONS2',filterSideChannels=FALSE)
  if(!is.data.frame(df)) return(df)

  siteInfo <- fetchNRSATable(chan, 'tblVISITS2',filterSideChannels=FALSE)
  if(!is.data.frame(df)) return(df)

  # Create validation results
  validationResults <- valLegalUnitsCheck.1(df, meta.df, siteInfo)
  if(!is.data.frame(validationResults)) return(validationResults)
  
  print(sprintf("Detected %d illegal units", nrow(validationResults)))

  # Write validation results to a spreadsheet for review.
  rc <- writeNRSAValidationResults(validationResults
                                  ,paste(NRSAvalidationLocation
                                        ,'valLegalUnits'
                                        ,tableName
                                        ,'.csv'
                                        ,sep=''
                                        )
                                  )
  return(NULL)
}

valLegalUnitsCheck.1 <- function(df, meta.df, siteInfo)
# Do all the work once the tables are in hand.  Separated for unit testing.
# Return dataframe of issues on success, or character string describing the
# error if one occurs.
#
# ARGUMENTS:
# df       dataframe of data to be checked
# meta.df  dataframe of metadata containing expected units for parameters that
#            use them.
# siteInfo dataframe of site visit information - site_id, visit_no, date_col.
#
# ASSUMPTIONS:
#  meta.df has columns PARAMETER, UNITS, SAMPLE_TYPE & FORMABBR.
{
  # Keep only metdata of use, and column names upper case so they are the same
  # names as in the data.
  units.meta <- subset(meta.df
                      ,!(UNITS=='' | is.na(UNITS))
                      ,select=c(PARAMETER, UNITS, SAMPLE_TYPE, FORMABBR)
                      )
  if(nrow(units.meta) == 0) {
      return("Warning: No units information occur in the metadata.")
  }

  # Comb through data for unexpected parameter-unit combinations
  expectedUnits <- unique(paste(units.meta$PARAMETER
                               ,units.meta$UNITS
                               ,units.meta$SAMPLE_TYPE
                               )
                         )
                         
  rr <- subset(df
              , #!(UNITS=='' | is.na(UNITS)) &
                !(paste(PARAMETER, UNITS, SAMPLE_TYPE) %in% expectedUnits)
              )

  if(nrow(rr) == 0) {
      return("No illegal units were detected.")
  }

  # Create list of expected parameter/unit combinations
  ss<-units.meta[!duplicated(units.meta[c('PARAMETER','UNITS','SAMPLE_TYPE')]),]
  tt<-aggregate(ss['UNITS']
             ,list(PARAMETER  =ss$PARAMETER
                  ,SAMPLE_TYPE=ss$SAMPLE_TYPE
                  )
             ,function(x) { paste(x, collapse=', ') }
             )

  #Rename UNITS to ALL_UNITS for the merge
  colnames(tt) <- c('PARAMETER', 'SAMPLE_TYPE', 'ALL_UNITS')
  xx <- merge(rr,tt, all.x=T, all.y=F)
  
  # add this column, and fill with expected units
  xx$TESTDESCRIPTION <- paste('UNITS value must be ', xx$ALL_UNITS)
  # its work now done, ALL_UNITS can be deleted
  xx$ALL_UNITS <- NULL

  # Construct validation results
  vv <- constructNRSAValidationResults(xx, meta.df, siteInfo)

  return(vv)
}

valLegalUnitsCheck.cleanup <- function(chan)
# Clean up if something causes an early termination
{
  odbcClose(chan)
}

valLegalUnitsCheckTest <- function()
# tests valLegalUnitsCheck.1
{
  baseData <- expand.grid(UID=1:5
                         ,TRANSECT=LETTERS[1:10]
                         ,STATION=0:9
                         ,PARAMETER=c('BARWIDTH','WETWIDTH'
                                     ,'DEP_SONR','INCREMNT','REACHLENGTH'
                                     )
                         )
  baseData$UID <- as.character(baseData$UID)
  baseData$TRANSECT <- as.character(baseData$TRANSECT)
  baseData$PARAMETER <- as.character(baseData$PARAMETER)
  baseData <- subset(baseData
                    ,PARAMETER %in% c('DEP_SONR','INCREMNT','REACHLENGTH') |
                     (PARAMETER %in% c('BARWIDTH','WETWIDTH') &
                      STATION %in% c(0,5)
                     )
                    )
  baseData$RESULT <- ''
  baseData[baseData$PARAMETER=='INCREMNT',]$RESULT <- c(rep(1.5, times=100)
                                                       ,rep(2.5, times=100)
                                                       ,rep(3.5, times=100)
                                                       ,rep(4.5, times=100)
                                                       ,rep(5.5, times=100)
                                                       )
  baseData[baseData$PARAMETER=='REACHLENGTH',]$RESULT <- c(rep(150, times=100)
                                                          ,rep(250, times=100)
                                                          ,rep(350, times=100)
                                                          ,rep(450, times=100)
                                                          ,rep(550, times=100)
                                                          )
  baseData[baseData$PARAMETER=='BARWIDTH',]$RESULT <- rep(c(0,2), times=50)
  baseData[baseData$PARAMETER=='WETWIDTH',]$RESULT <- rep(3, times=100)
  baseData$UNITS<-''
  baseData[baseData$PARAMETER=='DEP_SONR' & baseData$TRANSECT %in% c('A','C'),]$UNITS <- 'M'
  baseData[baseData$PARAMETER=='DEP_SONR' & !(baseData$TRANSECT %in% c('A','C')),]$UNITS <- 'FT'
  baseData[baseData$PARAMETER=='INCREMNT',]$UNITS <- 'M'
  baseData[baseData$PARAMETER=='REACHLENGTH',]$UNITS <- 'M'
  baseData[baseData$PARAMETER=='BARWIDTH',]$UNITS <- 'M'
  baseData[baseData$PARAMETER=='WETWIDTH',]$UNITS <- 'M'
  baseData$SAMPLE_TYPE<-''
  baseData[baseData$PARAMETER %in%
           c('BARWIDTH','WETWIDTH','INCREMNT','REACHLENGTH')
          ,]$SAMPLE_TYPE <- 'PHAB_THALW'
  baseData[baseData$PARAMETER=='DEP_SONR',]$SAMPLE_TYPE <- 'PHAB_THAL'
  
  meta.df <- data.frame(PARAMETER  =c('BARWIDTH','WETWIDTH','DEP_SONR','DEP_SONR','INCREMNT','REACHLENGTH')
                       ,LEGALVALUES=c(NA,        NA,        NA,        NA,        NA,        NA)
                       ,FORMABBR   =c('Thal',    'Thal',    'Thal',   'Thal',   'Thal',   'Thal')
                       ,SAMPLE_TYPE=c('PHAB_THALW','PHAB_THALW','PHAB_THAL','PHAB_THAL','PHAB_THALW','PHAB_THALW')
                       ,UNITS      =c('M', 'M', 'M', 'FT', 'M', 'M')
                       ,stringsAsFactors=FALSE
                       )

  siteInfo <- data.frame(UID=as.character(1:50)
                        ,SITE_ID=paste('site'
                                      ,as.character(rep(1:25, each=2))
                                      ,sep=''
                                      )
                        ,VISIT_NO=rep(1:2, times=25)
                        ,DATE_COL=as.character(as.POSIXct(paste('2008', 4:5, rep(1:25, each=2), sep='-')))
                        ,SAMPLE_TYPE=paste('type', rep(1:3, length.out=50))
                        ,OTHERJUNK='other junk'
                        ,stringsAsFactors=FALSE
                        )

  # Perform legal value check with perfect data
  rr <- valLegalUnitsCheck.1(baseData, meta.df, siteInfo)
  checkEquals("No illegal units were detected.", rr
             ,paste("Error: Problem detecting illegal values in perfect data: "
                   ,rr
                   )
             )
  
  # Perform legal value check with imperfect data
  realData <- baseData
  realData[realData$UID=='1' & realData$TRANSECT=='A' & realData$STATION==0 &
           realData$PARAMETER=='BARWIDTH',]$UNITS <- 'Furlongs'
  realData[realData$UID=='2' & realData$TRANSECT=='B' & realData$STATION==2 &
           realData$PARAMETER=='DEP_SONR',]$UNITS <- 'Fathoms'
  realData[realData$UID=='3' & realData$TRANSECT=='C' & realData$STATION==0 &
           realData$PARAMETER=='REACHLENGTH',]$UNITS <- as.character(NA)

  rr <- valLegalUnitsCheck.1(realData, meta.df, siteInfo)
  expected <- data.frame(UID=c('1','2','3')
                        ,SITE_ID=c('site1', 'site1', 'site2')
                        ,VISIT_NO=as.integer(c(1,2,1))
                        ,DATE_COL=c("2008-04-01", "2008-05-01", "2008-04-02")
                        ,SAMPLE_TYPE=c('PHAB_THALW','PHAB_THAL','PHAB_THALW')
                        ,TRANSECT=c('A', 'B', 'C')
                        ,STATION=as.integer(c(0,2,0))
                        ,PARAMETER=c("BARWIDTH", "DEP_SONR", "REACHLENGTH")
                        ,TESTDESCRIPTION=c("UNITS value must be  M"
                                          ,"UNITS value must be  M, FT"
                                          ,"UNITS value must be  M"
                                          )
                        ,RESULT=c('0', '', '150')
                        ,UNITS=c('Furlongs', 'Fathoms', NA)
                        ,FORMIMAGE=c('=HYPERLINK(\"file:///L:/Apps/Scantron/Images/2008/Flowing Waters/NRSA_Thal_site1_V1_A.tif\" , \"Thal\")'
                                    ,'=HYPERLINK(\"file:///L:/Apps/Scantron/Images/2008/Flowing Waters/NRSA_Thal_site1_V2_B.tif\" , \"Thal\")'
                                    ,'=HYPERLINK(\"file:///L:/Apps/Scantron/Images/2008/Flowing Waters/NRSA_Thal_site2_V1_C.tif\" , \"Thal\")'
                                    )
                        ,COMMENTS=rep("                                              ", 3)
                        ,stringsAsFactors=FALSE
                        )
#  rr$UID <- NULL
  checkEquals(expected, rr
             ,"Error: Did not correctly identify illegal values"
             )
  
}
# end of file
