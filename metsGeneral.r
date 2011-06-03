# metsGeneral.r
#  
#  01/04/10 rch copied, plagerized and made up this code.
#  02/18/10 cws removed source() of NRSAValidation.r,NA_filler.r and summaryby.r
#  03/22/10 cws Added all=TRUE argument to merge() of expected and actual values
#           in unit test.
#  04/02/10 cws Modified unit test and metrics code to handle data with just
#           one protocol.  Added comment about change in sidecnt value in unit
#           test.
#  06/03/10 cws Modified reachlen calculation to work with single incremnt value
#           at A 0 instead of one for each transect.
#

require(RODBC)
require(RUnit)

metsGeneral <- function()
#Calculates General metrics:
# Wadeable Protocal:
#
#
#Boatable Protocal:
#
#
#These metrics are saved to a csv file in the directory specified by
# NRSAMetricsLocation.
#
# Returns NULL on success or a character string describing the problem if one
# occurs.
# ARGUMENTS:
# none
{
  intermediateMessage('General calculations', loc='start')
     
  intermediateMessage('.1 Read in data', loc='end')
  # read in data from database
  indb <- odbcConnect('NRSA2')
  on.exit(metsGeneral.cleanup(indb))

  rawdat <- fetchNRSATable(indb, 'tblThalweg2')
  rawdat <-rawdat[c('UID','TRANSECT','STATION','SAMPLE_TYPE','PARAMETER','RESULT')]

  bdat <- fetchNRSATable(indb, 'tblChannelGeometry2')
  bdat$STATION =0
  bdat <-bdat[c('UID','TRANSECT','STATION','SAMPLE_TYPE','PARAMETER','RESULT')]
  bdat <- subset(bdat,PARAMETER == 'ACTRANSP')

  vdat <- fetchNRSATable(indb, 'tblVISITS2')
  vdat$TRANSECT = 'A'
  vdat$STATION = 0
  vdat$PARAMETER = 'PROTOCOL'
  vdat$SAMPLE_TYPE = 'PHAB_THALW'
  vdat$RESULT = vdat$VALXSITE
  vdat <-vdat[c('UID','TRANSECT','STATION','SAMPLE_TYPE','PARAMETER','RESULT')]

  rawdat<- rbind (rawdat,bdat,vdat)

  intermediateMessage('.2 call function metsGeneral.1', loc='end')

  # calculate the calculations
  mets <- metsGeneral.1(rawdat)
     
  intermediateMessage('.3 Write results', loc='end')
  # write the results
  rc <- writeNRSACalcResults(mets, 'metsGeneral.csv')

  intermediateMessage('  Done.', loc='end')

  return(rc)
}


metsGeneral.1 <- function(indat)
# Does all the real work for metsGeneral.
# Returns a dataframe of calculations if successful
# or a character string describing the problem if
# one was encountered.
#
# ARGUMENTS:
# indat		dataframe of channel data.
# protocols	dataframe relating UID to the
#			  sampling protocol used at the site.
#
{
  intermediateMessage('.1.0General mets', loc='end')
   
  intermediateMessage('.1.1 subset data and check RESULTS for wadeable and boatable', loc='end')
  cdData <- subset(indat
                  ,PARAMETER %in% c('ACTRANSP','INCREMNT','SIDCHN','OFF_CHAN'
                                   ,'REACHLENGTH','PROTOCOL'
                                   )
                  )

  # calculate PCT_SIDE (side channels) for wadeable(SIDCHN) and boatable (OFF_CHAN)
  sidec<-subset(cdData
               ,PARAMETER %in% c('SIDCHN','OFF_CHAN') &
                RESULT %in% c('Y','N',NA) &
                !(TRANSECT %in% c('XA','XB','XC','XD','XE','XF'
                                 ,'XG','XH','XI','XJ','XK'
                                 )
                 )
               )
 
  intermediateMessage('.1.2 sent datasets to summaryby function', loc='end')

  ps <- summaryby(sidec,'count',"pct_side")
  tyout<-aggregate( list(typesum=sidec$RESULT),list(UID=sidec$UID),function(x){sum(x=='Y',na.rm=TRUE)})
  ps<-merge(ps,tyout,by='UID',all.x=TRUE)
  ps$RESULT <- (ps$typesum/ps$RESULT)*100
  ps<-ps[c('UID','METRIC','RESULT')]

  # subset data for wadeable only and count side channel transect types (SIDECNT)
  wdata  <- subset(cdData,SAMPLE_TYPE == 'PHAB_THALW' & PARAMETER == 'INCREMNT')
  sc <- NULL
  if(nrow(wdata)>0) {
      wdata  <-unique(wdata[c('UID','TRANSECT')])
      wdata$RESULT<-ifelse(wdata$TRANSECT %in% c('XA','XB','XC','XD','XE','XF'
                                                ,'XG','XH','XI','XJ','XK'
                                                )
                          ,1
                          ,0
                          )
      sc<-summaryby(wdata,'sum','sidecnt')
   }

  #calculate REACHLEN using parameter values REACHLENGTH and ACTRANSP

  incr  <- subset(cdData
                 ,SAMPLE_TYPE == 'PHAB_THALW' &
                  PARAMETER == 'INCREMNT' &
                  TRANSECT == 'A' & STATION == 0
                 ,select=c(UID,RESULT)
                 )
  rlw <- NULL
  if(nrow(incr)>0) {
      w2<- nWadeableStationsPerTransect(cdData)
      transpc <- merge(incr,w2,by=c('UID'),all.x=TRUE)
      transpc$transpc<-(as.numeric(transpc$RESULT) * transpc$nSta)
      transpc$RESULT<- ifelse(transpc$transpc<=0,NA,transpc$transpc)
      rlw <- summaryby(transpc,'sum',"reachlen")

      # need to subtract one increment from each UID, since the last transect has 1 less increment than the others.
      rlw<-merge(rlw,incr,by='UID',all.x=TRUE, suffix=c('.transpcTot','.incremnt'))
      rlw$RESULT<-rlw$RESULT.transpcTot - as.numeric(rlw$RESULT.incremnt)
      rlw<-rlw[c('UID','METRIC','RESULT')]
  }

  bdata  <- subset(cdData,PARAMETER =='ACTRANSP')
  rl <- NULL
  if(nrow(bdata)>0) {
      bdata$RESULT<-as.numeric( bdata$RESULT)
      bdata$nSta=1
      bdata$RESULT<- ifelse(bdata$RESULT<=0,NA,bdata$RESULT)
      rl <- summaryby(bdata,'sum',"reachlen")
  }

  #Determine if site SAMPLED is 'Y' or 'N' from tblVISIT2 data
  #NOTE: there is one site, UID=12475, that has a NULL value for samp$RESULT and
  #the site has been sampled.

  samp <- subset(cdData,PARAMETER == 'PROTOCOL')
  samp$RESULT <- ifelse(samp$RESULT %in% c('BOATABLE','PARBYBOAT','ALTERED',
     'INTWADE','PARBYWADE','WADEABLE'),'Y','N')
  samp$METRIC='sampled'
  samp <- unique(samp[c('UID','METRIC','RESULT')]);

  intermediateMessage('.1.3 put dataset together and finish calculations', loc='end')

  mets <- rbind(ps,sc,rlw,rl,samp)
  mets$RESULT<-ifelse(mets$RESULT=='NaN',NA,mets$RESULT)

  intermediateMessage('.1.4 Done with function metsGeneral.1 ', loc='end')
   
  return(mets)
}



metsGeneralTest <- function()
# Unit test for metsGeneral.1
# IGNORE THE RESULTS for Boatable sites.  The test data is from WEMAP data and
#has only wadable sites.  The  metsGeneral.1 function needs data for
#both SAMPLE_TYPES, so the data was duplicated and RESULTS for Boatable obs.
#were set to zero.
{
  intermediateMessage('.2.0General test of data', loc='end')

  intermediateMessage('.2.1 Create dataset from WEMAP', loc='end')
  testData <- metsGeneral.testData()

  intermediateMessage('.2.2 Create Expected Data', loc='end')
  metsExpected <- metsGeneral.expectedMets()

  intermediateMessage('.2.3 Test with both protocols', loc='end')
  metsGeneralTest.process(testData, metsExpected)

  intermediateMessage('.2.4 Test with wadeable protocol', loc='end')
  wd <- subset(testData, UID %in% c('WCAP99-0587','WCAP99-0592','WCAP99-0905'))
  wm <- subset(metsExpected, UID %in% c('WCAP99-0587','WCAP99-0592','WCAP99-0905'))
  metsGeneralTest.process(wd, wm)

  intermediateMessage('.2.5 Test with boatable protocol', loc='end')
  bd <- subset(testData, !(UID %in% c('WCAP99-0587','WCAP99-0592','WCAP99-0905')))
  bm <- subset(metsExpected, !(UID %in% c('WCAP99-0587','WCAP99-0592','WCAP99-0905')))
  metsGeneralTest.process(bd, bm)

}


metsGeneralTest.process <- function(testData, metsExpected)
# performs the bulk of the unit test on the given data and expected results
# testData<- bd; metsExpected<-bm
{
  testDataResult<- metsGeneral.1(testData)

  #compare results from baseData (testDataResult) with expectedResults  (metsExpected)

  metsExpected <- rename(metsExpected, 'RESULT','EXPECTED')
  rr <- subset(testDataResult,testDataResult$METRIC != 'sampled')  #remove character based test
  rr$RESULT<-as.numeric( rr$RESULT)

  # Calculated values should be within 10E-7 of expected values, should
  # only be missing where they are supposed to be missing and nonmissing where
  # they are supposed to be nonmissing.
  # Note: the errs dataframe can be printed to show where the errors occur when
  # debugging.
  tt <- merge(rr, metsExpected, by=c('UID','METRIC'), all=TRUE)
  tt$diff <- tt$RESULT - tt$EXPECTED

  errs <- subset(tt, abs(diff) > 10^-7 | is.na(RESULT) != is.na(EXPECTED))
  checkEquals(0, nrow(errs)
             ,"Error: General  metrics are broken"
             )

}


metsGeneral.cleanup <- function(indb)
# Clean up when metsGeneral() terminates
{
  odbcClose(indb)
}


metsGeneral.testData <- function()
# Creates test data for metsGeneral unit test
{
  # Create correctly formated test data, and run data through metsGeneral.1
  testData <- rbind(data.frame(UID ='WCAP99-0585',
                               PARAMETER='ACTRANSP',
                               SAMPLE_TYPE='PHAB_THAL',
                               TRANSECT=c(LETTERS[1:10]),
                               STATION='0',
                               RESULT=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),stringsAsFactors=FALSE
 ),
                    data.frame(UID ='WCAP99-0587',
                               PARAMETER='INCREMNT',
                               SAMPLE_TYPE='PHAB_THALW',
                               TRANSECT=c("A","A","A","A","A","A","A","A","A","A","B","B","B","B","B","B","B",
     "B","B","B","C","C","C","C","C","C","C","C","C","C","D","D","D","D","D","D","D","D","D","D","E",
     "E","E","E","E","E","E","E","E","E","E","E","F","F","F","F","F","F","F","F","F","F","G","G","G","G",
     "G","G","G","G","G","G","H","H","H","H","H","H","H","H","H","H","H","I","I","I","I","I","I","I","I","I",
     "I","J","J","J","J","J","J","J","J","J","J"),
                               STATION=c("0","1","2","3","4","5","6","7","8","9","0","1","2","3","4","5","6","7",
     "8","9","0","1","2","3","4","5","6","7","8","9","0","1","2","3","4","5","6","7","8","9","0","1","2",
     "3","4","5","6","7","8","9","10","11","0","1","2","3","4","5","6","7","8","9","0","1","2","3","4","5",
     "6","7","8","9","0","1","2","3","4","5","6","7","8","9","10","0","1","2","3","4","5","6","7","8","9",
     "0","1","2","3","4","5","6","7","8","9"),
                               RESULT=c("1.5",NA,NA,NA,NA,NA,NA,NA,NA,NA,"1.5",NA,NA,NA,NA,NA,NA,NA,
     NA,NA,"1.5",NA,NA,NA,NA,NA,NA,NA,NA,NA,"1.5",NA,NA,NA,NA,NA,NA,NA,NA,NA,"1.5",NA,NA,NA,
     NA,NA,NA,NA,NA,NA,NA,NA,"1.5",NA,NA,NA,NA,NA,NA,NA,NA,NA,"1.5",NA,NA,NA,NA,NA,NA,NA,
     NA,NA,"1.5",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,"1.5",NA,NA,NA,NA,NA,NA,NA,NA,NA,"1.5",NA,NA,
     NA,NA,NA,NA,NA,NA,NA),stringsAsFactors=FALSE
 ),
                    data.frame(UID ='WCAP99-0591',
                               PARAMETER='ACTRANSP',
                               SAMPLE_TYPE='PHAB_THAL',
                               TRANSECT=c("A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A",
                   "A","A","A","B","B","B","B","B","B","B","B","B","B","B","B","B","B","B","B","B","B","B",
        "B","C","C","C","C","C","C","C","C","C","C","C","C","C","C","C","C","C","C","C","C","D","D","D",
     "D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","E","E","E","E","E","E","E",
     "E","E","E","E","E","E","E","E","E","E","E","E","E","F","F","F","F","F","F","F","F","F","F","F","F",
     "F","F","F","F","F","F","F","F","G","G","G","G","G","G","G","G","G","G","G","G","G","G","G","G",
     "G","G","G","G","H","H","H","H","H","H","H","H","H","H","H","H","H","H","H","H","H","H","H",
     "H","I","I","I","I","I","I","I","I","I","I","I","I","I","I","I","I","I","I","I","I","J","J","J","J","J","J","J","J",
     "J","J","J","J","J","J","J","J","J","J","J","J"),
                               STATION=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15",
     "16","17","18","19","20","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16",
     "17","18","19","20","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17",
     "18","19","20","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18",
     "19","20","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19",
     "20","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20",
     "1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","1",
     "2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","1","2",
     "3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","1","2","3",
     "4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20"),
                               RESULT=c("30","30","30","30","30","30","30","30","30","30","30","30","30",
     "30","30","30","30","30","30","30","30","30","30","30","30","30","30","30","30","30","30","30",
     "30","30","30","30","30","30","30","30","30","30","30","30","30","30","30","30","30","30","30",
     "30","30","30","30","30","30","30","30","30","30","30","30","30","30","30","30","30","30","30",
     "30","30","30","30","30","30","30","30","30","30","30","30","30","30","30","30","30","30","30",
     "30","30","30","30","30","30","30","30","30","30","30","30","30","30","30","30","30","30","30",
     "30","30","30","30","30","30","30","30","30","30","30","30","30","30","30","30","30","30","30",
     "30","30","30","30","30","30","30","30","30","30","30","30","30","30","30","30","30","30","30",
     "30","30","30","30","30","30","30","30","30","30","30","30","30","30","30","30","30","30","30",
     "30","30","30","30","30","30","30","30","30","30","30","30","30","30","30","30","30","30","30",
     "30","30","30","30","30","30","30","30","30","30","30","30","30","30","30","30"),stringsAsFactors=FALSE
 ),
                    data.frame(UID ='WCAP99-0592',
                               PARAMETER='INCREMNT',
                               SAMPLE_TYPE='PHAB_THALW',
                               TRANSECT=c("A","A","A","A","A","A","A","A","A","A","B","B","B","B","B","B","B",
     "B","B","B","C","C","C","C","C","C","C","C","C","C","D","D","D","D","D","D","D","D","D","D","E",
     "E","E","E","E","E","E","E","E","E","F","F","F","F","F","F","F","F","F","F","G","G","G","G","G","G",
     "G","G","G","G","H","H","H","H","H","H","H","H","H","H","I","I","I","I","I","I","I","I","I","I","J","J",
     "J","J","J","J","J","J","J","J","XF","XF","XF","XG","XG","XG","XG","XG","XG","XG","XG","XG",
     "XG"),
                               STATION=c("0","1","2","3","4","5","6","7","8","9","0","1","2","3","4","5","6","7",
     "8","9","0","1","2","3","4","5","6","7","8","9","0","1","2","3","4","5","6","7","8","9","0","1","2",
     "3","4","5","6","7","8","9","0","1","2","3","4","5","6","7","8","9","0","1","2","3","4","5","6","7",
     "8","9","0","1","2","3","4","5","6","7","8","9","0","1","2","3","4","5","6","7","8","9","0","1","2",
     "3","4","5","6","7","8","9","7","8","9","0","1","2","3","4","5","6","7","8","9"),
                               RESULT=c("1.5",NA,NA,NA,NA,NA,NA,NA,NA,NA,"1.5",NA,NA,NA,NA,NA,NA,NA,NA,NA,
     "1.5",NA,NA,NA,NA,NA,NA,NA,NA,NA,"1.5",NA,NA,NA,NA,NA,NA,NA,NA,NA,"1.5",NA,NA,NA,NA,NA,NA,NA,
     NA,NA,"1.5",NA,NA,NA,NA,NA,NA,NA,NA,NA,"1.5",NA,NA,NA,NA,NA,NA,NA,NA,NA,"1.5",NA,NA,NA,NA,NA,
     NA,NA,NA,NA,"1.5",NA,NA,NA,NA,NA,NA,NA,NA,NA,"1.5",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,"1.5",
     NA,NA,NA,NA,NA,NA,NA,NA,NA),stringsAsFactors=FALSE
 ),
                    data.frame(UID ='WCAP99-0905',
                               PARAMETER='INCREMNT',
                               SAMPLE_TYPE='PHAB_THALW',
                               TRANSECT=c("A","A","A","A","A","A","A","A","A","A","B","B","B","B","B","B","B",
     "B","B","B","C","C","C","C","C","C","C","C","C","C","D","D","D","D","D","D","D","D","D","D","E",
     "E","E","E","E","E","E","E","E","E","F","F","F","F","F","F","F","F","F","F","G","G","G","G","G","G",
     "G","G","G","G","H","H","H","H","H","H","H","H","H","H","I","I","I","I","I","I","I","I","I","I","J","J",
     "J","J","J","J","J","J","J","J","XI","XJ"),
                               STATION=c("0","1","2","3","4","5","6","7","8","9","0","1","2","3","4","5","6","7",
     "8","9","0","1","2","3","4","5","6","7","8","9","0","1","2","3","4","5","6","7","8","9","0","1","2",
     "3","4","5","6","7","8","9","0","1","2","3","4","5","6","7","8","9","0","1","2","3","4","5","6","7",
     "8","9","0","1","2","3","4","5","6","7","8","9","0","1","2","3","4","5","6","7","8","9","0","1","2",
     "3","4","5","6","7","8","9","0","0"),
                               RESULT=c("1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6",
     "1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6",
     "1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6",
     "1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6",
     "1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6",
     "1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6",
     "1.6","1.6","1.6","1.6","1.6"),stringsAsFactors=FALSE
 ),
                    data.frame(UID ='WCAP99-0585',
                               PARAMETER='OFF_CHAN',
                               SAMPLE_TYPE='PHAB_THAL',
                               TRANSECT=LETTERS[1:10],
                               STATION='0',
                               RESULT=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
 ),
                    data.frame(UID ='WCAP99-0587',
                               PARAMETER='SIDCHN',
                               SAMPLE_TYPE='PHAB_THALW',
                               TRANSECT=c("A","A","A","A","A","A","A","A","A","A","B","B","B","B","B","B","B",
     "B","B","B","C","C","C","C","C","C","C","C","C","C","D","D","D","D","D","D","D","D","D","D","E",
     "E","E","E","E","E","E","E","E","E","E","E","F","F","F","F","F","F","F","F","F","F","G","G","G","G",
     "G","G","G","G","G","G","H","H","H","H","H","H","H","H","H","H","H","I","I","I","I","I","I","I","I",
     "I","I","J","J","J","J","J","J","J","J","J","J"),
                               STATION=c("0","1","2","3","4","5","6","7","8","9","0","1","2","3","4","5","6","7",
     "8","9","0","1","2","3","4","5","6","7","8","9","0","1","2","3","4","5","6","7","8","9","0","1","2",
     "3","4","5","6","7","8","9","10","11","0","1","2","3","4","5","6","7","8","9","0","1","2","3","4","5",
     "6","7","8","9","0","1","2","3","4","5","6","7","8","9","10","0","1","2","3","4","5","6","7","8","9",
     "0","1","2","3","4","5","6","7","8","9"),
                               RESULT=c("N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N",
     "N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N",
     "N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N",
     "N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N",
     "N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N"),stringsAsFactors=FALSE
 ),
                    data.frame(UID ='WCAP99-0591',
                               PARAMETER='OFF_CHAN',
                               SAMPLE_TYPE='PHAB_THAL',
                               TRANSECT=c("A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A",
                   "A","A","A","B","B","B","B","B","B","B","B","B","B","B","B","B","B","B","B","B","B","B",
        "B","C","C","C","C","C","C","C","C","C","C","C","C","C","C","C","C","C","C","C","C","D","D","D",
     "D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","E","E","E","E","E","E","E",
     "E","E","E","E","E","E","E","E","E","E","E","E","E","F","F","F","F","F","F","F","F","F","F","F","F",
     "F","F","F","F","F","F","F","F","G","G","G","G","G","G","G","G","G","G","G","G","G","G","G","G",
     "G","G","G","G","H","H","H","H","H","H","H","H","H","H","H","H","H","H","H","H","H","H","H",
     "H","I","I","I","I","I","I","I","I","I","I","I","I","I","I","I","I","I","I","I","I","J","J","J","J","J","J","J","J",
     "J","J","J","J","J","J","J","J","J","J","J","J"),
                               STATION=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15",
     "16","17","18","19","20","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16",
     "17","18","19","20","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17",
     "18","19","20","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18",
     "19","20","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19",
     "20","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20",
     "1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","1",
     "2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","1","2",
     "3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","1","2","3",
     "4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20"),
                               RESULT=c("N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N",
     "N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","Y","N","N","N","N","N","Y","Y",
     "N","N","N","Y","Y","N","N","N","N","N","N","N","N","N","N","N","Y","Y","Y","N","N","N","N",
     "N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N",
     "N","N","N","N","N","N","N","N","N","N","Y","Y","N","N","N","Y","N","N","N","N","N","N","N",
     "N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","Y"," ","N","N","N",
     "N","N","N","N","N","N","N","N","Y","N","N","N","N","N","N","N","N","N","N","N","N","N","N",
     "N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","Y","Y","Y",
     "Y","Y","Y","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","Y","Y"),stringsAsFactors=FALSE
 ),
                    data.frame(UID ='WCAP99-0592',
                               PARAMETER='SIDCHN',
                               SAMPLE_TYPE='PHAB_THALW',
                               TRANSECT=c("A","A","A","A","A","A","A","A","A","A","B","B","B","B","B","B","B",
     "B","B","B","C","C","C","C","C","C","C","C","C","C","D","D","D","D","D","D","D","D","D","D","E",
     "E","E","E","E","E","E","E","E","E","F","F","F","F","F","F","F","F","F","F","G","G","G","G","G","G",
     "G","G","G","G","H","H","H","H","H","H","H","H","H","H","I","I","I","I","I","I","I","I","I","I","J","J",
     "J","J","J","J","J","J","J","J","XF","XF","XF","XG","XG","XG","XG","XG","XG","XG","XG","XG",
     "XG"),
                               STATION=c("0","1","2","3","4","5","6","7","8","9","0","1","2","3","4","5","6","7",
     "8","9","0","1","2","3","4","5","6","7","8","9","0","1","2","3","4","5","6","7","8","9","0","1","2",
     "3","4","5","6","7","8","9","0","1","2","3","4","5","6","7","8","9","0","1","2","3","4","5","6","7",
     "8","9","0","1","2","3","4","5","6","7","8","9","0","1","2","3","4","5","6","7","8","9","0","1","2",
     "3","4","5","6","7","8","9","7","8","9","0","1","2","3","4","5","6","7","8","9"),
                               RESULT=c("Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y",
     "Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y",
     "Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y",
     "Y","Y","Y","Y"," ","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y",
     "Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y",
     "Y","Y","Y","Y"),stringsAsFactors=FALSE
 ),
                    data.frame(UID ='WCAP99-0905',
                               PARAMETER='SIDCHN',
                               SAMPLE_TYPE='PHAB_THALW',
                               TRANSECT=c("A","A","A","A","A","A","A","A","A","A","B","B","B","B","B","B","B",
     "B","B","B","C","C","C","C","C","C","C","C","C","C","D","D","D","D","D","D","D","D","D","D","E",
     "E","E","E","E","E","E","E","E","E","F","F","F","F","F","F","F","F","F","F","G","G","G","G","G","G",
     "G","G","G","G","H","H","H","H","H","H","H","H","H","H","I","I","I","I","I","I","I","I","I","I","J","J",
     "J","J","J","J","J","J","J","J","XI","XJ"),
                               STATION=c("0","1","2","3","4","5","6","7","8","9","0","1","2","3","4","5","6","7",
     "8","9","0","1","2","3","4","5","6","7","8","9","0","1","2","3","4","5","6","7","8","9","0","1","2",
     "3","4","5","6","7","8","9","0","1","2","3","4","5","6","7","8","9","0","1","2","3","4","5","6","7",
     "8","9","0","1","2","3","4","5","6","7","8","9","0","1","2","3","4","5","6","7","8","9","0","1","2",
     "3","4","5","6","7","8","9","0","0"),
                               RESULT=c("N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N",
     "N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N",
     "N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N",
     "N","N","N","N","N","N","N","N","N","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y",
     "Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","N","Y"),stringsAsFactors=FALSE
 ),
                    data.frame(UID ='WCAP99-0585',
                               PARAMETER='PROTOCOL',
                               SAMPLE_TYPE='PHAB_THAL',
                               TRANSECT=LETTERS[1:10],
                               STATION='0',
                               RESULT='BOATABLE',stringsAsFactors=FALSE
   ),
                    data.frame(UID ='WCAP99-0587',
                               PARAMETER='PROTOCOL',
                               SAMPLE_TYPE='PHAB_THALW',
                               TRANSECT=LETTERS[1:10],
                               STATION='0',
                               RESULT='WADEABLE',stringsAsFactors=FALSE
   ),
                    data.frame(UID ='WCAP99-0591',
                               PARAMETER='PROTOCOL',
                               SAMPLE_TYPE='PHAB_THAL',
                               TRANSECT=LETTERS[1:10],
                               STATION='0',
                               RESULT='BOATABLE',stringsAsFactors=FALSE
   ),
                    data.frame(UID ='WCAP99-0592',
                               PARAMETER='PROTOCOL',
                               SAMPLE_TYPE='PHAB_THALW',
                               TRANSECT=LETTERS[1:10],
                               STATION='0',
                               RESULT='WADEABLE',stringsAsFactors=FALSE
   ),
                    data.frame(UID ='WCAP99-0905',
                               PARAMETER='PROTOCOL',
                               SAMPLE_TYPE='PHAB_THALW',
                               TRANSECT=LETTERS[1:10],
                               STATION='0',
                               RESULT='WADEABLE',stringsAsFactors=FALSE
   )
)



      testData$UID <- as.character(testData$UID)
      testData$TRANSECT <- as.character(testData$TRANSECT)
      testData$STATION <- as.numeric(testData$STATION)
      testData$SAMPLE_TYPE <- as.character(testData$SAMPLE_TYPE)
      testData$PARAMETER <- as.character(testData$PARAMETER)
      testData$RESULT <- as.character(testData$RESULT)

  return(testData)
}

metsGeneral.expectedMets <- function()
# Create dataframe of expected metrics calculations
# The sidecnt value for WCAP99-0905 was changed from 3 to 2 due to the
# difference in how the value was calculated -- EMAP defines it as the
# number of side channel transects in the sub_bank file, while NRSA defines
# it as the number of side channel transects recorded in the tblTHALWEG2 table.
{
  metsExpected <- rbind(data.frame(UID = c('WCAP99-0585','WCAP99-0587','WCAP99-0591','WCAP99-0592','WCAP99-0905'),
                                   METRIC='pct_side',
                                   RESULT=c( NA,0,10.552763819,100,28 )
                                  )
                       ,data.frame(UID = c('WCAP99-0585','WCAP99-0587','WCAP99-0591','WCAP99-0592','WCAP99-0905'),
                                   METRIC='reachlen',
                                   RESULT=c(NA,153,6000,148.5,158.4 )
                                  )
                       ,data.frame(UID = c('WCAP99-0585','WCAP99-0587','WCAP99-0591','WCAP99-0592','WCAP99-0905'),
                                   METRIC='sidecnt',
                                   RESULT=c(NA,0,NA,2,2 )
                                  )
                       )
  return(metsExpected)
}