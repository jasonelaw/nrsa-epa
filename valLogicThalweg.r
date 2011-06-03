# valLogicThalweg.r
#
# 12/21/09 cws created
#  3/30/10 cws corrected on.exit() function call.
#  6/11/10 cws Unit test now including UID in expected return value using
#          imperfect/real data, due to changes in constructNRSAValidationResults().
#

valLogicThalweg <- function()
# Performs logic checks on the NRSA thalweg data.
#
# ASSUMPTIONS:
# Values for WETWIDTH in tblBANKGEOMETRY2 are accurate.
#
{
  # Retrieve data to be validated, form metadata and site information
  chan <- odbcConnect('NRSA2')
  on.exit(valLogicThalweg.cleanup(chan))

  thal <- fetchNRSATable(chan, 'tblTHALWEG2')
  if(!is.data.frame(thal)) return(thal)

  meta.df <- fetchNRSATable(chan, 'tblPARAMETERDESCRIPTIONS2')
  if(!is.data.frame(meta.df)) return(meta.df)

  siteInfo <- fetchNRSATable(chan, 'tblVISITS2')
  if(!is.data.frame(siteInfo)) return(siteInfo)

  # Create range validation results
  validationResults <- valLogicThalweg.1(thal, meta.df, siteInfo)
  if(!is.data.frame(validationResults)) return(validationResults)

  print(sprintf("Detected %d values with illogical relations", nrow(validationResults)))

  # Write validation results to a spreadsheet for review.
  rc <- writeNRSAValidationResults(validationResults
                                  ,paste(NRSAvalidationLocation
                                        ,'valLogicThalweg.csv'
                                        ,sep=''
                                        )
                                  )
}

valLogicThalweg.1 <- function(df, meta.df, siteInfo)
# Does all the work for valLogicThalweg()
#
# ARGUMENTS:
# df      dataframe with NRSA Thalweg data
# meta.df   dataframe with parameter and form information from
#             tblPARAMETERDESCRIPTIONS2
# siteInfo  dataframe with site information from tblVISITS2
#
# ASSUMPTIONS:
#
{
  intermediateMessage('Thalweg logic checks.', loc='start')
  
  # wadeable: bar width < wetted width by definition
  # Collect both values on the same row and make comparison.  All units are
  # meters.
  bw <- subset(df, PARAMETER=='BARWIDTH', select=c(UID,TRANSECT,STATION,RESULT))
  bw$RESULT <- as.numeric(bw$RESULT)
  bw <- rename(bw, 'RESULT','BARWIDTH')
  intermediateMessage('.1')

  ww <- subset(df, PARAMETER=='WETWIDTH', select=c(UID,TRANSECT,STATION,RESULT))
  ww$RESULT <- as.numeric(ww$RESULT)
  ww <- rename(ww, 'RESULT','WETWIDTH')
  intermediateMessage('.2')

  tt <- merge(bw,ww, by=c('UID','TRANSECT','STATION'), all.x=TRUE, all.y=FALSE)
  tt <- subset(tt, (BARWIDTH >= WETWIDTH) & (WETWIDTH > 0) )

  # Select rows with incorrect values and add test description
  if(nrow(tt) == 0) {
      rr <- NULL
  } else {
      rr <- merge(subset(df, PARAMETER %in% c('BARWIDTH','WETWIDTH'))
                 ,subset(tt, select=c(UID,TRANSECT,STATION))
                 ,by=c('UID','TRANSECT','STATION')
                 ,all.x=FALSE, all.y=TRUE
                 )
      rr$TESTDESCRIPTION <- 'Bar width MUST be less than wetted width'
  }
  logicResults <- rr
  rm(bw,ww,tt, rr)
  intermediateMessage('.3')


  # wadeable: station increment > reach length / 100 (only checked at A 0)
  # Collect both values on the same row and make comparison.  All units are
  # meters.
  incr <- subset(df, PARAMETER=='INCREMNT' & TRANSECT=='A' & STATION=='0'
                ,select=c(UID,RESULT)
                )
  incr$RESULT <- as.numeric(incr$RESULT)
  incr <- rename(incr, 'RESULT','INCREMNT')
  intermediateMessage('.4')

  rl <- subset(df, PARAMETER=='REACHLENGTH' & TRANSECT=='A' & STATION=='0'
              ,select=c(UID,RESULT)
              )
  rl$RESULT <- as.numeric(rl$RESULT)
  rl <- rename(rl, 'RESULT','REACHLENGTH')
  intermediateMessage('.5')

  tt <- merge(incr, rl, by='UID', all.x=TRUE, all.y=TRUE)
  tt <- subset(tt, INCREMNT > REACHLENGTH/100)
  
  # Select rows with incorrect values and add test description
  if(nrow(tt) == 0) {
      rr <- NULL
  } else {
      rr <- merge(subset(df
                        ,PARAMETER %in% c('INCREMNT','REACHLENGTH') &
                         TRANSECT=='A' & STATION=='0'
                        )
                 ,subset(tt, select=UID)
                 ,by='UID'
                 ,all.x=FALSE, all.y=TRUE
                 )
      rr$TESTDESCRIPTION <- 'Station increment value MUST agree with reach length'
  }
  logicResults <- rbind(logicResults, rr)
  intermediateMessage('.6')
  rm(incr,rl,tt,rr)


  # Construct validation results
  if(is.null(logicResults)) {
      vv <- "There were zero logical errors detected in the thalweg data."
  } else {
      vv <- constructNRSAValidationResults(logicResults, meta.df, siteInfo)
  }
  intermediateMessage('. Done.', loc='end')

  return(vv)
}

valLogicThalwegTest <- function()
# Unit test for valLogicThalweg.1()
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
  baseData$SAMPLE_TYPE<-''
  baseData[baseData$PARAMETER %in%
           c('BARWIDTH','WETWIDTH','INCREMNT','REACHLENGTH')
          ,]$SAMPLE_TYPE <- 'PHAB_THALW'

  meta.df <- data.frame(PARAMETER  =c('BARWIDTH','WETWIDTH','INCREMNT','REACHLENGTH')
                       ,LEGALVALUES=c(NA,        NA,        NA,        NA)
                       ,FORMABBR   =c('Thal',    'Thal',    'Thal',   'Thal')
                       ,SAMPLE_TYPE=c('PHAB_THALW','PHAB_THALW','PHAB_THALW','PHAB_THALW')
                       ,stringsAsFactors=FALSE
                       )

  siteInfo <- data.frame(UID=as.character(1:50)
                        ,SITE_ID=paste('site'
                                      ,as.character(rep(1:25, each=2))
                                      ,sep=''
                                      )
                        ,VISIT_NO=rep(1:2, times=25)
                        ,DATE_COL=as.POSIXct(paste('2008', 4:5, rep(1:25, each=2), sep='-'))
                        ,SAMPLE_TYPE=paste('type', rep(1:3, length.out=50))
                        ,OTHERJUNK='other junk'
                        ,stringsAsFactors=FALSE
                        )

  # Test with perfect data
  rr <- valLogicThalweg.1(baseData, meta.df, siteInfo)
  ee <- "There were zero logical errors detected in the thalweg data."
  checkEquals(ee,rr, "Error: Detected logical thalweg errors where there are none")
  
  # Test with imperfect data
  realData <- baseData
  realData[realData$UID=='1' & realData$TRANSECT=='A' & realData$STATION=='5' &
           realData$PARAMETER=='BARWIDTH',]$RESULT <- 20
  realData[realData$UID=='2' & realData$TRANSECT=='A' & realData$STATION=='0' &
           realData$PARAMETER=='INCREMNT',]$RESULT <- 15
  realData[realData$UID=='3' & realData$TRANSECT=='A' & realData$STATION=='0' &
           realData$PARAMETER=='REACHLENGTH',]$RESULT <- 15

  rr <- valLogicThalweg.1(realData, meta.df, siteInfo)
  ee <- data.frame(UID=c('1','1','2','2','3','3')
                  ,SITE_ID=c('site1','site1','site1','site1','site2','site2')
                  ,VISIT_NO=c(1,1,2,2,1,1)
                  ,DATE_COL=c("2008-04-01","2008-04-01","2008-05-01"
                             ,"2008-05-01","2008-04-02","2008-04-02"
                             )
                  ,SAMPLE_TYPE=rep('PHAB_THALW',6)
                  ,TRANSECT=rep("A", 6)
                  ,STATION=c(5,5,0,0,0,0)
                  ,PARAMETER=c('BARWIDTH','WETWIDTH','INCREMNT','REACHLENGTH','INCREMNT','REACHLENGTH')
                  ,TESTDESCRIPTION=c("Bar width MUST be less than wetted width"
                                    ,"Bar width MUST be less than wetted width"
                                    ,"Station increment value MUST agree with reach length"
                                    ,"Station increment value MUST agree with reach length"
                                    ,"Station increment value MUST agree with reach length"
                                    ,"Station increment value MUST agree with reach length"
                                    )
                  ,RESULT=c('20','3','15','150','1.5','15')
                  ,FORMIMAGE=c('=HYPERLINK("file:///L:/Apps/Scantron/Images/2008/Flowing Waters/NRSA_Thal_site1_V1_A.tif" , "Thal")'
                              ,'=HYPERLINK("file:///L:/Apps/Scantron/Images/2008/Flowing Waters/NRSA_Thal_site1_V1_A.tif" , "Thal")'
                              ,'=HYPERLINK("file:///L:/Apps/Scantron/Images/2008/Flowing Waters/NRSA_Thal_site1_V2_A.tif" , "Thal")'
                              ,'=HYPERLINK("file:///L:/Apps/Scantron/Images/2008/Flowing Waters/NRSA_Thal_site1_V2_A.tif" , "Thal")'
                              ,'=HYPERLINK("file:///L:/Apps/Scantron/Images/2008/Flowing Waters/NRSA_Thal_site2_V1_A.tif" , "Thal")'
                              ,'=HYPERLINK("file:///L:/Apps/Scantron/Images/2008/Flowing Waters/NRSA_Thal_site2_V1_A.tif" , "Thal")'
                              )
                  ,COMMENTS=rep(paste(rep(' ',46),collapse=''), 6)
                  ,stringsAsFactors=FALSE
                  )
  ee$VISIT_NO <- as.integer(ee$VISIT_NO)
  ee$DATE_COL <- as.POSIXct(ee$DATE_COL)
  ee$STATION <- as.integer(ee$STATION)

  checkEquals(ee,rr, "Error: Incorrectly detected existing logical thalweg errors")
}


valLogicThalweg.cleanup <- function(chan)
# Clean up if something causes an early termination
{
  odbcClose(chan)
}

# end of file