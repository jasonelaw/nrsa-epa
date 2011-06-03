# valMissingCheck.r
#
# 10/30/09 cws Created
# 11/30/09 cws Filtering out missing values associated with comment flags.
#          Updated unit test.
# 12/02/09 cws Filtering based on FLAG only if that column exists.
# 12/23/09 cws Updating unit test to correct FORMIMAGE hyperlink arguments.
#  6/11/10 cws Unit test now including UID in expected return value using
#          imperfect/real data, due to changes in constructNRSAValidationResults().
#

valMissingCheck <- function(tableName,since=NULL)
# Performs missing value checks on a data table, creating a spreadsheet file of
# issues for review in folder determined by NRSAvalidationLocation
#
# Returns NULL on success, or character string describing error.
#
# ARGUMENTS:
# tableName Character string with name of table to do missing data validation on.
#
# ASSUMPTIONS:
# 
{
  # Retrieve data to be validated, form metadata and site information
  chan <- odbcConnect('NRSA2')
  on.exit(valMissingCheck.cleanup(chan))

  if(!is.null(since)) {
      since <- paste("INSERTION > CAST('", since, "' AS DATETIME)", sep='')
  }

  df <- fetchNRSATable(chan, tableName,where=since)
  if(!is.data.frame(df)) return(df)
  
  meta.df <- fetchNRSATable(chan, 'tblPARAMETERDESCRIPTIONS2', filterSideChannels=FALSE)
  if(!is.data.frame(meta.df)) return(meta.df)

  siteInfo <- fetchNRSATable(chan, 'tblVISITS2', filterSideChannels=FALSE)
  if(!is.data.frame(siteInfo)) return(siteInfo)
  
  # Create missing check results
  missingResults <- valMissingCheck.1(df, meta.df, siteInfo)
  if(!is.data.frame(missingResults)) return(missingResults)

  print(sprintf("Detected %d rows with missing values", nrow(missingResults)))

  # Write validation results to a spreadsheet for review.
  rc <- writeNRSAValidationResults(missingResults
                                  ,paste(NRSAvalidationLocation
                                        ,'valMissing'
                                        ,tableName
                                        ,'.csv'
                                        ,sep=''
                                        )
                                  )

  return(NULL)
}

valMissingCheck.cleanup <- function(chan)
# Clean up if something causes an early termination
{
  odbcClose(chan)
}


valMissingCheck.1 <- function(df, meta.df, siteInfo)
# Do all the work once the tables are in hand.  Separated for unit testing.
# Return dataframe of issues on success, or character string describing the
# error if one occurs.
#
# ARGUMENTS:
# df       dataframe of data to be checked
# meta.df  dataframe of metadata containing legal values and form imformation
# siteInfo dataframe of site visit information - site_id, visit_no, date_col.
#
{

  # Filter out missing values that have a flag associated with them.  We are
  # assuming that a missing value that has a comment flag attached to it is
  # missing for a reason.
  if('FLAG' %in% names(df)) {
      df <- subset(df, is.na(FLAG) | trimws(FLAG)=='')
  }
  
  # Determine whether UNITS is present and should be included in checks.
  # Use metadata to determine whether UNITS, if it exists, should have a value
  # and exclude rows with missing UNITS that should have missing UNITS unless
  # RESULTS is missing, as well as rows in which missing is noted as a legal
  # value.
  if('UNITS' %in% names(df)) {
      cols <- c('RESULT','UNITS')
      unitsRequired <- unique(subset(meta.df, UNITS!='')$PARAMETER)
      valueOptional <- subset(meta.df, substr(LEGALVALUES,1,1) == '|')$PARAMETER
      checkThese <- subset(df
                          ,(RESULT=='' & !(PARAMETER %in% valueOptional)) |
                           (UNITS=='' & PARAMETER %in% unitsRequired)
                          )
  } else {
      cols <- 'RESULT'
      checkThese <- df
  }

  # Comb through data for missing values and units
  rr <- validationMissing(checkThese, cols, otherTests=NULL)
  if(!is.data.frame(rr)) return(rr)
  if(nrow(rr) == 0) return("No missing values were detected.")

  # Construct validation results
  vv <- constructNRSAValidationResults(rr, meta.df, siteInfo)

  return(vv)
}

valMissingCheckTest <- function()
# Tests valMissingCheckTest.1()
{
  baseData <- expand.grid(UID=1:5
                         ,TRANSECT=LETTERS[1:10]
                         ,STATION=1:10
                         ,PARAMETER=c('BAR_PRES','BACKWATER','CHANUNCD'
                                     ,'BARWIDTH' ,'WETWIDTH','DEP_POLE'
                                     ,'DEP_SONR'
                                     )
                         )
  baseData$UID <- as.character(baseData$UID)
  baseData$TRANSECT <- as.character(baseData$TRANSECT)
  baseData$PARAMETER <- as.character(baseData$PARAMETER)
  baseData$RESULT <- ''
  baseData$UNITS <- ''
  baseData$FLAG <- as.character(NA)
  baseData[baseData$PARAMETER=='BAR_PRES'
          ,]$RESULT <- rep(c('','Y','N')
                          ,length.out=length(baseData[baseData$PARAMETER=='BAR_PRES',]$RESULT)
                          )
  baseData[baseData$PARAMETER=='BACKWATER'
          ,]$RESULT <- rep(c('','Y','N')
                          ,length.out=length(baseData[baseData$PARAMETER=='BACKWATER',]$RESULT)
                          )
  baseData[baseData$PARAMETER=='CHANUNCD'
          ,]$RESULT <- rep(c('','CA','DR','FA','GL','PP','PB','PI','PD','PL'
                            ,'PT','PO','RA','RI'
                            )
                          ,length.out=length(baseData[baseData$PARAMETER=='CHANUNCD',]$RESULT)
                          )
  baseData[baseData$PARAMETER=='BARWIDTH'
          ,]$RESULT <- rep(c(0,1,2,5,10)
                          ,length.out=length(baseData[baseData$PARAMETER=='BARWIDTH',]$RESULT)
                          )
  baseData[baseData$PARAMETER=='BARWIDTH',]$UNITS <- 'm'
  baseData[baseData$PARAMETER=='WETWIDTH'
          ,]$RESULT <- rep(c(1,5,10,20,50,100)
                          ,length.out=length(baseData[baseData$PARAMETER=='WETWIDTH',]$RESULT)
                          )
  baseData[baseData$PARAMETER=='WETWIDTH',]$UNITS <- ''
  baseData[baseData$PARAMETER=='DEP_POLE'
          ,]$RESULT <- rep(c(2,5,10)
                          ,length.out=length(baseData[baseData$PARAMETER=='DEP_POLE',]$RESULT)
                          )
  baseData[baseData$PARAMETER=='DEP_POLE'
          ,]$UNITS <- rep(c('ft','m')
                          ,length.out=length(baseData[baseData$PARAMETER=='DEP_POLE',]$UNITS)
                          )
  baseData[baseData$PARAMETER=='DEP_SONR'
          ,]$RESULT <- rep(c(2,5,10)
                          ,length.out=length(baseData[baseData$PARAMETER=='DEP_SONR',]$RESULT)
                          )
  baseData[baseData$PARAMETER=='DEP_SONR',]$UNITS <- 'm'
  baseData[is.na(baseData$RESULT) | baseData$RESULT=='',]$FLAG <-'F1'

  baseData$SAMPLE_TYPE <- 'PHAB_THAL'

  meta.legal <- data.frame(PARAMETER=  c('BAR_PRES' , 'BACKWATER' , 'CHANUNCD'                               )
                          ,LEGALVALUES=c('|Y|N'     , '|Y|N'      , '|CA|DR|FA|GL|PP|PB|PI|PD|PL|PT|PO|RA|RI')
                          ,FORMABBR  = c('Thal'    , 'Thal'    , 'Thal'                                      )
                          ,SAMPLE_TYPE  = c('PHAB_THAL'    , 'PHAB_THAL'    , 'PHAB_THAL'                    )
                          ,stringsAsFactors=FALSE
                          )
  meta.range <- data.frame(PARAMETER  =c('BARWIDTH' , 'WETWIDTH' , 'DEP_POLE' , 'DEP_POLE', 'DEP_SONR')
                          ,RANGETYPE  =c(''         , ''         , ''         , ''        , '')
                          ,RANGELOW   =c(0.0        , 0.0        , 0.0        , 0.0       , 0.0)
                          ,RANGEHIGH  =c(10         , 100        , 7          , 22        , 10)
                          ,UNITS      =c('m'        ,''          ,'m'         ,'ft'       , 'm')
                          ,FORMABBR   =c('Thal'     , 'Thal'     , 'Thal'     , 'Thal', 'Thal')
                          ,SAMPLE_TYPE=c('PHAB_THAL', 'PHAB_THAL', 'PHAB_THAL', 'PHAB_THAL', 'PHAB_THAL')
                          ,stringsAsFactors=FALSE
                          )
  meta.df <- merge(meta.legal, meta.range
                  ,by=c('PARAMETER','FORMABBR','SAMPLE_TYPE')
                  ,all=TRUE, sort=FALSE
                  )
  meta.df[is.na(meta.df$LEGALVALUES),]$LEGALVALUES <- ''
  meta.df[is.na(meta.df$RANGETYPE),]$RANGETYPE <- ''
  meta.df[is.na(meta.df$RANGELOW),]$RANGELOW <- ''
  meta.df[is.na(meta.df$RANGEHIGH),]$RANGEHIGH <- ''
  meta.df[is.na(meta.df$UNITS),]$UNITS <- ''
  
  siteInfo <- data.frame(UID=as.character(1:50)
                        ,SITE_ID=paste('site'
                                      ,as.character(rep(1:25, each=2))
                                      ,sep=''
                                      )
                        ,VISIT_NO=rep(1:2, times=25)
                        ,DATE_COL=paste('2008', 4:5, rep(1:25, each=2), sep='-')
                        ,SAMPLE_TYPE=paste('type', rep(1:3, length.out=50))
                        ,OTHERJUNK='other junk'
                        ,stringsAsFactors=FALSE
                        )

  # Perform missing value check with perfect data
  rr <- valMissingCheck.1(baseData, meta.df, siteInfo)
  checkEquals("No missing values were detected.", rr
             ,paste("Error: Problem detecting missing values in perfect data: "
                   ,rr
                   )
             )

  # Perform missing value check with imperfect data
  realData <- baseData
  realData[realData$UID==1 & realData$TRANSECT=='A' &
           realData$STATION==1 & realData$PARAMETER=='BARWIDTH',]$UNITS <- ''
  realData[realData$UID==2 & realData$TRANSECT=='B' &
           realData$STATION==1 & realData$PARAMETER=='DEP_POLE',]$UNITS <- ''
  realData[realData$UID==2 & realData$TRANSECT=='B' &
           realData$STATION==2 & realData$PARAMETER=='DEP_POLE',]$UNITS <- ''
  realData[realData$UID==3 & realData$TRANSECT=='C' &
           realData$STATION==3 & realData$PARAMETER=='WETWIDTH',]$RESULT <- ''
  realData[realData$UID==4 & realData$TRANSECT=='D' &
           realData$STATION==4 & realData$PARAMETER=='BARWIDTH',]$RESULT <- ''

  ee <- data.frame(UID=c('1','2','2','3','4')
                  ,SITE_ID=c('site1','site1','site1','site2','site2')
                  ,VISIT_NO=as.integer(c(1,2,2,1,2))
                  ,DATE_COL=c('2008-4-1','2008-5-1','2008-5-1','2008-4-2','2008-5-2')
                  ,SAMPLE_TYPE=rep('PHAB_THAL', 5)
                  ,TRANSECT=c('A','B','B','C','D')
                  ,STATION=as.integer(c(1,1,2,3,4))
                  ,PARAMETER=c('BARWIDTH','DEP_POLE','DEP_POLE','WETWIDTH','BARWIDTH')
                  ,TESTDESCRIPTION=rep('Missing value of RESULT, UNITS', 5)
                  ,RESULT=c('0','2','10','','')
                  ,UNITS=c('','','','','m')
                  ,FLAG=rep(as.character(NA), 5)
                  ,FORMIMAGE=c('=HYPERLINK("file:///L:/Apps/Scantron/Images/2008/Flowing Waters/NRSA_Thal_site1_V1_A.tif" , "Thal")'
                              ,'=HYPERLINK("file:///L:/Apps/Scantron/Images/2008/Flowing Waters/NRSA_Thal_site1_V2_B.tif" , "Thal")'
                              ,'=HYPERLINK("file:///L:/Apps/Scantron/Images/2008/Flowing Waters/NRSA_Thal_site1_V2_B.tif" , "Thal")'
                              ,'=HYPERLINK("file:///L:/Apps/Scantron/Images/2008/Flowing Waters/NRSA_Thal_site2_V1_C.tif" , "Thal")'
                              ,'=HYPERLINK("file:///L:/Apps/Scantron/Images/2008/Flowing Waters/NRSA_Thal_site2_V2_D.tif" , "Thal")'
                              )
                  ,stringsAsFactors=FALSE
                  )
  rr <- valMissingCheck.1(realData, meta.df, siteInfo)
  rr$COMMENTS<-NULL
 checkEquals(ee, rr
             ,paste("Error: Problem detecting missing values in imperfect data: "
                   ,rr
                   )
             )


}

# end of file
