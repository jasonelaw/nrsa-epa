#  metsCanopyDensiometer.r
#  
#  01/04/10 rch copied, plagerized and made up this code.
#  02/18/10 cws removed source() of NRSAValidation.r and summaryby.r
#  03/23/10 ssr moved creation of unit test dataframes to separate functions.
#  03/31/10 cws Removing extra print() statements and commented-out code.
#  04/06/10 mrc Modified unit test and metrics code to handle data with just
#           one protocol.  
#

require(RODBC)
require(RUnit)

metsCanopyDensiometer <- function()
#Calculates Canopy Densiometer metrics:
# Wadeable Protocal:
#xcdenmid xcdenbk vcdenmid vcdenbk nbnk nmid.
#
#Boatable Protocal:
#xcdenbk  vcdenbk nbnk
#
#These metrics are saved to a csv file in the directory specified by
# NRSAMetricsLocation.
#
# Returns NULL on success or a character string describing the problem if one
# occurs.
# ARGUMENTS:
# none
{
  intermediateMessage('Canopy Densiometer calculations', loc='start')

  intermediateMessage('.1 Read in data', loc='end')
  # read in densiometer readings from database
  dens <- odbcConnect('NRSA2')
  on.exit(metsCanopyDensiometer.cleanup(dens))
  rawdat <- fetchNRSATable(dens, 'tblCHANCOV2')

  intermediateMessage('.2 call function metsCanopyDensiometer.1', loc='end')

  # calculate the calculations
  mets <- metsCanopyDensiometer.1(rawdat)

  intermediateMessage('.3 Write results', loc='end')

  # write the results
  rc <- writeNRSACalcResults(mets, 'metsCanopyDensiometer.csv')

  intermediateMessage('  Done.', loc='end')
  return(rc)
}



metsCanopyDensiometer.1 <- function(indat)
# Does all the real work for metsCanopyDensiometer.
# Returns a dataframe of calculations if successful
# or a character string describing the problem if
# one was encountered.
#
# ARGUMENTS:
# indat		dataframe of canopy data.
# protocols	dataframe relating UID to the
#			  sampling protocol used at the site.
#
{

  intermediateMessage('.1.0Canopy Densiometer mets', loc='end')
   
  intermediateMessage('.1.1 check data and split into wadeable (mid and bank) and boatable (bank)', loc='end')
  cdData <- subset(indat,PARAMETER == 'DENSIOM')
  # cdData <- merge(cdData, protocols, by='UID', all.x=TRUE, all.y=FALSE)
   
  mdData <- subset(cdData
                  ,TRANSDIR %in% c('CU','CL','CD','CR') &
                   SAMPLE_TYPE == 'PHAB_CHANW'
                  )
  bkData <- subset(cdData
                  ,TRANSDIR %in% c('LF','RT') & SAMPLE_TYPE == 'PHAB_CHANW'
                  )
    
  btData  <- subset(cdData,SAMPLE_TYPE == 'PHAB_CHANB')
   
  intermediateMessage('.1.2 sent datasets to summaryby function', loc='end')
  
   mdx <- NULL
   mds <- NULL
   mdc <- NULL
   bkx <- NULL
   bks <- NULL
   bkc <- NULL
   btx <- NULL
   bts <- NULL
   btc <- NULL
  
  
  if (nrow (mdData) > 0)
    {
  mdx <- summaryby(mdData,'mean',"xcdenmid")
  mds <- summaryby(mdData,'sd',"vcdenmid")
  mdc <- summaryby(mdData,'count',"nmid")
     }
   if (nrow (bkData) > 0)
     { 
     
  bkx <- summaryby(bkData,'mean',"xcdenbk")
  bks <- summaryby(bkData,'sd',"vcdenbk")
  bkc <- summaryby(bkData,'count',"nbnk")
      }
   if (nrow (btData) > 0)
    {  
  btx <- summaryby(btData,'mean',"xcdenbk")
  bts <- summaryby(btData,'sd',"vcdenbk")
  btc <- summaryby(btData,'count',"nbnk")
      }
  intermediateMessage('.1.3 put dataset together and finish calculations', loc='end')
  mets <- rbind(mdx,mds,mdc,bkx,bks,bkc,btx,bts,btc)
  mets$RESULT<-ifelse(mets$RESULT=='NaN',NA,mets$RESULT)
  
  # more calcs
  mets$RESULT <- ifelse(mets$METRIC %in% c("xcdenmid", "vcdenmid", "xcdenbk","vcdenbk")
                       ,100 * mets$RESULT/17
                       ,mets$RESULT
                       )
    
  intermediateMessage('.1.4 Done with function metsCanopyDensiometer.1 ', loc='end')
   
  return(mets)
    
}


metsCanopyDensiometerTest <- function()
# Unit test for metsCanopyDensiometer.1

{
  intermediateMessage('.2.0 Canopy Densiometer test of data', loc='end')
  intermediateMessage('.2.1 Create dataset from WEMAP', loc='end')
   testData <- metsCanopyDensiometer.testData()

  intermediateMessage ('.2.2 Create expected dataset', loc='end')
   metsExpected <- metsCanopyDensiometer.expectedMets ()
   
#in this dataset, the protoccol is determined by SAMPLE_TYPE.  Here where
#SAMPLE_TYPE == PHAB_CHANW, the protocol is WADEABLE
#SAMPLE_TYPE == PHAB_CHANB the protocol is BOATABLE   
#to test for one/both protocols, subset the data by SAMPLE_TYPE as a proxy for protocol

  intermediateMessage ('.2.3 Test with both protocols', loc='end')
  
  metsCanopyDensiometerTest.process (testData, metsExpected)

 intermediateMessage ('.2.4 Test with wadeable protocol', loc='end')
  test.w <- subset(testData, SAMPLE_TYPE == 'PHAB_CHANW')
  expected.w <- subset (metsExpected, UID %in%  c('WAZP99-0545','WAZP99-0551','WCAP99-0534',
                                           'WCAP99-0535','WCAP99-0536','WCAP99-0539',
                                           'WMTP99-0549'))
  metsCanopyDensiometerTest.process (test.w, expected.w)
  
  intermediateMessage ('.2.5 Test with boatable protocol', loc='end')
  test.b <- subset(testData, SAMPLE_TYPE == 'PHAB_CHANB')
  expected.b <- subset (metsExpected, UID %in%  c('WMTP99-0525','WMTP99-0587'))
  metsCanopyDensiometerTest.process (test.b, expected.b)
 } 
 
  metsCanopyDensiometerTest.process <- function (testData, metsExpected)

  {

 
  testDataResult<- metsCanopyDensiometer.1(testData)
  
 

  #compare results from baseData (testDataResult) with expectedResults  (metsExpected)
  metsExpected <- rename(metsExpected, 'RESULT','EXPECTED')
  rr <- testDataResult

  # Calculated values should be within 10E-7 of expected values, should
  # only be missing where they are supposed to be missing and nonmissing where
  # they are supposed to be nonmissing.
  # Note: the errs dataframe can be printed to show where the errors occur when
  # debugging.
  tt <- merge(rr, metsExpected, by=c('UID','METRIC'),all=T)
  tt$diff <- tt$RESULT - tt$EXPECTED

  errs <- subset(tt, abs(diff) > 10^-5 | is.na(RESULT) != is.na(EXPECTED))
  checkEquals(0, nrow(errs)
             ,"Error: Canopy Densiometer  metrics are broken"
             )
}

 
metsCanopyDensiometer.cleanup <- function(indb)
# Clean up when metsCanopyDensiometer() terminates
{
  odbcClose(indb)
}

metsCanopyDensiometer.testData <- function()
# creates dataframe of canopy densiometer data for unit test
{
  # Create correctly formated test data, and run data through metsCanopyDensiometer.1
  testData <- rbind(expand.grid(TRANSDIR =c('CD','CL','CR','CU','LF','RT')
                               ,TRANSECT = LETTERS[1:11]
                               ,UID = c('WAZP99-0545','WAZP99-0551','WCAP99-0534'
                                       ,'WCAP99-0535','WCAP99-0536','WCAP99-0539'
                                       ,'WMTP99-0549'
                                       )
                               ,SAMPLE_TYPE = 'PHAB_CHANW'
                               ,PARAMETER = 'DENSIOM'
                               )
                   )

  testData$RESULT <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
                      ,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
                      ,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,17,17,17,17
                      ,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17
                      ,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17
                      ,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17
                      ,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17
                      ,17,17,17,17,17,17,16,14,17,17,14,14,13,12,17,17
                      ,11,15,16,9,17,17,16,16,11,10,17,17,12,17,14,15
                      ,17,16,8,9,0,0,17,17,0,17,0,5,17,12,17,17,17,17
                      ,17,17,3,0,6,2,7,5,0,0,2,0,12,3,0,3,0,0,10,2,5,13
                      ,0,9,17,13,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA
                      ,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA
                      ,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA
                      ,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA
                      ,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA
                      ,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA
                      ,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA
                      ,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,13,15,15,17,16
                      ,17,17,17,17,17,17,17,17,17,17,17,17,17,17,16,16
                      ,15,1,17,15,16,15,15,17,15,16,16,17,14,17,17,15
                      ,13,12,15,16,14,17,17,16,17,17,11,NA,NA,NA,NA,NA
                      ,NA,NA,NA,NA,NA,NA,NA,17,17,17,17,17,17,0,0,0,0,3
                      ,2,0,0,0,0,2,0,0,0,0,0,2,2,0,0,0,0,5,17,0,0,0,0,7
                      ,17,0,0,0,0,4,12,0,0,0,0,10,14,0,0,0,0,8,0,0,0,0
                      ,0,4,2,0,0,0,0,4,15,0,0,0,0,5,12
                      )
  testData$UID <- as.character(testData$UID)
  testData$TRANSECT <- as.character(testData$TRANSECT)
  testData$TRANSDIR <- as.character(testData$TRANSDIR)
  testData$SAMPLE_TYPE <- as.character(testData$SAMPLE_TYPE)
  testData$PARAMETER <- as.character(testData$PARAMETER)

  testBoat <- rbind(expand.grid(TRANSDIR =c('UP','DN','LF','RT')
                               ,TRANSECT = LETTERS[1:11]
                               ,UID = c('WMTP99-0525','WMTP99-0587')
                               ,SAMPLE_TYPE='PHAB_CHANB'
                               ,PARAMETER = 'DENSIOM'
                               )
                   )

  intermediateMessage('.2.2 Create and add Boat data', loc='end')

  testBoat$RESULT <- c(2,10, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 2, 1, 10, 13
                      ,0, 8, 8,11, 0, 8, 0, 0, 0, 0, 1, 0,14,2, 0, 0, 0, 0, 0
                      ,0, 0, 0, 0, 0, 0, 0, 9,13, 4,15, 0, 0, 0, 0,10, 0,10, 0
                      ,13, 4,10, 2, NA, NA, NA, NA, NA, NA, NA, NA, 0, 0, 0, 0
                      ,0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0
                      )

  testBoat$UID <- as.character(testBoat$UID)
  testBoat$TRANSECT <- as.character(testBoat$TRANSECT)
  testBoat$TRANSDIR <- as.character(testBoat$TRANSDIR)
  testBoat$SAMPLE_TYPE <- as.character(testBoat$SAMPLE_TYPE)
  testBoat$PARAMETER <- as.character(testBoat$PARAMETER)

  testData<-rbind (testData,testBoat)

  return(testData)
}


metsCanopyDensiometer.expectedMets <- function()
# creates dataframe of canopy densiometer metrics calculation results for unit test

{
  metsExpected <- rbind(data.frame(UID = c('WAZP99-0545','WAZP99-0551','WCAP99-0534',
                                           'WCAP99-0535','WCAP99-0536','WCAP99-0539',
                                           'WMTP99-0549'),
                                   METRIC='xcdenmid',
                                   RESULT=c(0,100.000, 62.433155, 22.05882352,
                                            NA ,93.7908496,0)
                                           )
                       ,data.frame(UID = c('WAZP99-0545','WAZP99-0551',
                                           'WCAP99-0534','WCAP99-0535','WCAP99-0536',
                                           'WCAP99-0539','WMTP99-0549'),
                                   METRIC='vcdenmid',
                                   RESULT=c(  0,  0, 38.81681136, 29.11616158, NA ,
                                            7.94748513, 0)
                                           )
                       ,data.frame(UID = c('WAZP99-0545','WAZP99-0551',
                                           'WCAP99-0534','WCAP99-0535','WCAP99-0536',
                                           'WCAP99-0539','WMTP99-0549'),
                                   METRIC='nmid',
                                   RESULT=c(44,44,44, 8, 0,36,44  )
                                  )
                       ,data.frame(UID = c('WAZP99-0545','WAZP99-0551',
                                           'WCAP99-0534','WCAP99-0535','WCAP99-0536'
                                          ,'WCAP99-0539','WMTP99-0549'),
                                   METRIC='xcdenbk',
                                   RESULT=c(  0,100.000, 87.433155, 61.764705,NA ,
                                            90.52287, 39.304812)
                                           )
                       ,data.frame(UID = c('WAZP99-0545','WAZP99-0551',
                                           'WCAP99-0534','WCAP99-0535','WCAP99-0536'
                                          ,'WCAP99-0539','WMTP99-0549'),
                                   METRIC='vcdenbk',
                                   RESULT=c( 0, 0,25.3689502,37.3579585,NA ,
                                            23.016811,32.594532)
                                           )
                       ,data.frame(UID = c('WAZP99-0545','WAZP99-0551',
                                           'WCAP99-0534','WCAP99-0535','WCAP99-0536',
                                           'WCAP99-0539','WMTP99-0549'),
                                   METRIC='nbnk',
                                   RESULT=c( 22,22,22, 4, 0, 18, 22)
                                  )
                       ,data.frame(UID = c('WMTP99-0525','WMTP99-0587'),
                                   METRIC='xcdenbk',
                                   RESULT=c(12.700534,16.176470)
                                  )
                       ,data.frame(UID = c('WMTP99-0525','WMTP99-0587'),
                                   METRIC='vcdenbk',
                                   RESULT=c(23.799516,27.279878)
                                  )
                       ,data.frame(UID = c('WMTP99-0525','WMTP99-0587'),
                                   METRIC='nbnk',
                                   RESULT=c(44,36)
                                  )
                       )
  metsExpected$METRIC <- as.factor(as.character(metsExpected$METRIC))
  metsExpected$UID <- as.factor(as.character(metsExpected$UID))
  return(metsExpected[order(metsExpected$UID, metsExpected$METRIC),])
}
