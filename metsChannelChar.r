#  metsChannelChar.r
#  
#  01/27/10 rch copied, plagerized and made up this code.
#  02/18/10 cws removed source() of NRSAValidation.r and summaryby.r
#  03/23/10 ssr moved creation of unit test dataframes to separate functions.
#  06/10/10 cws removed list of created metrics as incorrect.
#  06/24/10 cws Added mets from tblChannelChar2

require(RODBC)
require(RUnit)

metsChannelChar <- function()
# Calculates Channel Characteristic metrics:
# xshor2vg, mxshor, mnshor, pct_ovrb, pctch_b, pctch_c, pctch_n, pctch_u and
# conbankfull, confeatures, conpattern, conpercent, constraint, convalley,
# convalleybox.
#
# These metrics are saved to a csv file in the directory specified by
# NRSAMetricsLocation.
#
# Returns NULL on success or a character string describing the problem if one
# occurs.
# ARGUMENTS:
# none
{
  intermediateMessage('Channel Characteristic calculations', loc='start')
  intermediateMessage('.1 Read in data', loc='end')
  
  # read in Channel Characteristic data from database
  newtab <- odbcConnect('NRSA2')
  bg <- fetchNRSATable(newtab, 'tblBankGeometry2')
  cc <- fetchNRSATable(newtab, 'tblChannelChar2')

  intermediateMessage('.2 call function metsChannelChar.1', loc='end')

  # calculate the calculations
  mets <- metsChannelChar.1(bg, cc)

  intermediateMessage('.3 Write results', loc='end')

  # write the results
  rc <- writeNRSACalcResults(mets, 'metsChannelChar.csv')
  on.exit(metsChannelChar.cleanup(newtab))
  intermediateMessage('  Done.', loc='end')

  return(rc)
}


metsChannelChar.1 <- function(indat, chanCon)
# Does all the real work for metsChannelChar.
# Returns a dataframe of calculations if successful
# or a character string describing the problem if
# one was encountered.
#
# ARGUMENTS:
# indat		dataframe of bank geometry data taken at each transect.
# cc        dataframe with channel characteristics data taken for entire site.
# protocols	dataframe relating UID to the
#			  sampling protocol used at the site.
#
{
  intermediateMessage('.1.0Channel Characteristic mets', loc='end')
  intermediateMessage('.1.1 check data and split into wadeable (mid and bank) and boatable (bank)', loc='end')

  #cdData <- subset(indat,PARAMETER %in% c('CONSTRT','SHOR2RIP','SEEOVRBK'))
  # cdData <- merge(cdData, protocols, by='UID', all.x=TRUE, all.y=FALSE)

  #Create datasets needed for the calculations
  cdata <- subset(indat,SAMPLE_TYPE =='PHAB_CHANB' & PARAMETER =='CONSTRT' )
  rdata <- subset(indat,SAMPLE_TYPE =='PHAB_CHANB' & PARAMETER =='SHOR2RIP' )
  rdata$RESULT<-as.numeric( rdata$RESULT)
  odata <- subset(indat,SAMPLE_TYPE =='PHAB_CHANB' & PARAMETER =='SEEOVRBK' )

  #Use summaryby for the three metrics of shor2rip distance
  xsh <- summaryby(rdata,'mean',"xshor2vg")
  mxs <- summaryby(rdata,'max',"mxshor")
  mns <- summaryby(rdata,'min',"mnshor")

  intermediateMessage('.1.2 sent datasets to summaryby function', loc='end')

  #Calculate percentages for each of the pct_* metrics and format the results
 
  tco <- summaryby(odata,'count',"pct_ovrb")
  tyout<-aggregate( list(yessum=odata$RESULT),list(UID=odata$UID),function(x){sum(x=='YES',na.rm=TRUE)})
  tco<-merge(tco,tyout,by='UID',all.x=TRUE)
  tco$yessum<- ifelse( is.na(tco$yessum),0,tco$yessum)
  tco$RESULT <- (tco$yessum/tco$RESULT)*100
  tco<-tco[c('UID','METRIC','RESULT')]

  tbb <- summaryby(cdata,'count',"pctch_b")
  bb<-aggregate(list(letsum=cdata$RESULT),list(UID=cdata$UID),function(x){sum(x=='B',na.rm=TRUE)})
  tbb<-merge(tbb,bb,by='UID',all.x=TRUE)
  tbb$RESULT <- (tbb$letsum/tbb$RESULT)*100
  tbb<-tbb[c('UID','METRIC','RESULT')]

  tcc <- summaryby(cdata,'count',"pctch_c")
  cc<-aggregate(list(letsum=cdata$RESULT),list(UID=cdata$UID),function(x){sum(x=='C',na.rm=TRUE)})
  tcc<-merge(tcc,cc,by='UID',all.x=TRUE)
  tcc$RESULT <- (tcc$letsum/tcc$RESULT)*100
  tcc<-tcc[c('UID','METRIC','RESULT')]

  tnn <- summaryby(cdata,'count',"pctch_n")
  nn<-aggregate(list(letsum=cdata$RESULT),list(UID=cdata$UID),function(x){sum(x=='N',na.rm=TRUE)})
  tnn<-merge(tnn,nn,by='UID',all.x=TRUE)
  tnn$RESULT <- (tnn$letsum/tnn$RESULT)*100
  tnn<-tnn[c('UID','METRIC','RESULT')]

  tuu <- summaryby(cdata,'count',"pctch_u")
  uu<-aggregate(list(letsum=cdata$RESULT),list(UID=cdata$UID),function(x){sum(x=='U',na.rm=TRUE)})
  tuu<-merge(tuu,uu,by='UID',all.x=TRUE)
  tuu$RESULT <- (tuu$letsum/tuu$RESULT)*100
  tuu<-tuu[c('UID','METRIC','RESULT')]

  intermediateMessage('.1.3 pctch_* calculated', loc='end')

  # Translate channel characteristics taken for entire site
  ccMets <- rename(chanCon[c('UID','PARAMETER','RESULT')], 'PARAMETER', 'METRIC')
  ccMets$METRIC <- ifelse(ccMets$METRIC=='BANKFULL', 'conbankfull'
                  ,ifelse(ccMets$METRIC=='CONSTRNT', 'constraint'
                  ,ifelse(ccMets$METRIC=='FEATURES', 'confeatures'
                  ,ifelse(ccMets$METRIC=='PATTERN', 'conpattern'
                  ,ifelse(ccMets$METRIC=='PERCENT', 'conpercent'
                  ,ifelse(ccMets$METRIC=='VALLEY', 'convalley'
                  ,ifelse(ccMets$METRIC=='VALLYBOX', 'convalleybox','UNKNOWN!!'
                  )))))))

  intermediateMessage('.1.4 con* mets done', loc='end')

  #add the datasets together and return
  intermediateMessage('.1.5 put dataset together and finish calculations', loc='end')
  mets <- rbind(xsh,mxs,mns,tco,tbb,tcc,tnn,tuu,ccMets)

  intermediateMessage('.1.6 Done with function metsChannelChar.1 ', loc='end')

  return(mets)

}


metsChannelCharTest <- function()
# Unit test for metsChannelChar.1
# IGNORE THE RESULTS for Boatable sites.  The test data is from WEMAP data and
#has only wadable sites.  The  metsChannelChar.1 function needs data for
#both SAMPLE_TYPES, so the data was duplicated and RESULTS for Boatable obs.
#were set to zero.
{

  intermediateMessage('.2.0Channel Characteristic test of data', loc='end')
  intermediateMessage('.2.1 Create dataset from WEMAP', loc='end')
  
  # Create correctly formated test data, and run data through metsChannelChar.1
  testData <- metsChannelChar.inputData()
  testCCData <- metsChannelchar.createChanChar()

  #  testData$RESULT <- rep(as.character(0:4), length.out=nrow(testData))

  intermediateMessage('.2.3 Call metsChannelChar.1', loc='end')

  metsResult <- metsChannelChar.1(testData, testCCData)

  intermediateMessage('.2.4 Create Expected Data', loc='end')

  metsExpected <- metsCanopyDensiometer.testResults()
#  metsExpected <- rename(metsExpected, 'RESULT','EXPECTED')

  intermediateMessage('.2.5 Merge Expected data and results, then compare', loc='end')
  # Check character mets separately from numeric mets to allow zeriFudge to
  # have an effect.
  # Calculated values should be within 10E-7 of expected values, should
  # only be missing where they are supposed to be missing and nonmissing where
  # they are supposed to be nonmissing.
  # Note: the errs dataframe can be printed to show where the errors occur when
  # debugging.

  errC <- dfCompare(subset(metsExpected
                          ,METRIC %in% c('constraint','confeatures','conpattern','convalleybox')
                          )
                   ,subset(metsResult
                          ,METRIC %in% c('constraint','confeatures','conpattern','convalleybox')
                          )
                   ,c('UID','METRIC'), zeroFudge=1e-7
                   )
  ee <- subset(metsExpected
              ,!(METRIC %in% c('constraint','confeatures','conpattern','convalleybox'))
              )
  ee$RESULT <- as.numeric(ee$RESULT)
  rr <- subset(metsResult
              ,!(METRIC %in% c('constraint','confeatures','conpattern','convalleybox'))
              )
  rr$RESULT <- as.numeric(rr$RESULT)
  errN <- dfCompare(ee, rr, c('UID','METRIC'), zeroFudge=1e-7)
  errs <- rbind(errC, errN)
  checkEquals(NULL, errs
             ,"Error: Channel Characteristic metrics are broken"
  )

}


metsChannelChar.cleanup <- function(indb)
# Clean up when metsChannelChar() terminates
{
  odbcClose(indb)
}

metsChannelChar.inputData <- function()
# creates dataframe of channel characteristics data for unit test
{
  # Create correctly formated test data, and run data through metsChannelChar.1
  testData <- rbind(expand.grid(PARAMETER=c('CONSTRT','SEEOVRBK','SHOR2RIP')
                   ,TRANSECT = LETTERS[1:11]
                   ,UID = c('WAZP99-0591','WMTP99-0587','WSDP99-0528','WUTP99-0553','WWYP99-0576')
                   ,SAMPLE_TYPE = 'PHAB_CHANB'
                   )
     )

  testData$RESULT <- c("N","YES",0,"N","YES",0,"N","YES",0,"C",NA,0,"C","YES",0,"N","YES",
              0,"C","YES",0,"N","YES",0,"N",NA,0,"N","YES",0,"C","YES",0,"B","N",0.3,
              "U","N",30,"B","N",2,"U","N",1,"U","N",2,"U","N",10,"B","N",15,"B","N",3,
              "B","N",1,"C","N",5,"B","N",2,"C","YES",0,"B","N",0,"B","N",0,"B","YES",
              0,"B","YES",0,"B","N",0,"B","N",0,"B",NA,0,"U","YES",0,"U","YES",1,NA,
              NA,NA,"C","YES",2.7,"C","YES",1.5,NA,NA,NA,"C","YES",1.3,"C","YES",
              1.1,"C","N",0.5,"C","N",0,"C","N",1.5,"C","YES",22,"C","N",3,"C","N",30,
              "B","YES",15,"U","YES",5,"B","YES",2,"C","N",10,"B","N",1,"U","N",1,
              "B","N",2,"B","N",1,"B","N",1,"B","N",2,"B","N",0.3)

  testData$UID <- as.character(testData$UID)
  testData$TRANSECT <- as.character(testData$TRANSECT)
  testData$SAMPLE_TYPE <- as.character(testData$SAMPLE_TYPE)
  testData$PARAMETER <- as.character(testData$PARAMETER)

  return(testData)
}


metsCanopyDensiometer.testResults <- function()
# creates dataframe of channel characteristics metrics calculation results for unit test
{
  metsExpected <- rbind(data.frame(UID = c('WAZP99-0591','WMTP99-0587','WSDP99-0528','WUTP99-0553','WWYP99-0576'),
                                   METRIC='xshor2vg',
                                   RESULT=c(0,6.4818181818,0.1,6.36,3.6636363636 )
                                  )
                       ,data.frame(UID = c('WAZP99-0591','WMTP99-0587','WSDP99-0528','WUTP99-0553','WWYP99-0576'),
                                   METRIC='mxshor',
                                   RESULT=c(0,30,1,30,15 )
                                  )
                       ,data.frame(UID = c('WAZP99-0591','WMTP99-0587','WSDP99-0528','WUTP99-0553','WWYP99-0576'),
                                   METRIC='mnshor',
                                   RESULT=c(0,0.3,0,0,0.3 )
                                  )
                       ,data.frame(UID = c('WAZP99-0591','WMTP99-0587','WSDP99-0528','WUTP99-0553','WWYP99-0576'),
                                   METRIC='pct_ovrb',
                                   RESULT=c(100,0,55.555555556,50,27.272727273 )
                                  )
                       ,data.frame(UID = c('WAZP99-0591','WMTP99-0587','WSDP99-0528','WUTP99-0553','WWYP99-0576'),
                                   METRIC='pctch_b',
                                   RESULT=c( 0,54.545454545,70,0,72.727272727)
                                  )
                       ,data.frame(UID = c('WAZP99-0591','WMTP99-0587','WSDP99-0528','WUTP99-0553','WWYP99-0576'),
                                   METRIC='pctch_c',
                                   RESULT=c( 36.363636364,9.0909090909,10,100,9.0909090909)
                                  )
                       ,data.frame(UID = c('WAZP99-0591','WMTP99-0587','WSDP99-0528','WUTP99-0553','WWYP99-0576'),
                               METRIC='pctch_n',
                               RESULT=c( 63.636363636,0,0,0,0)
                                  )
                       ,data.frame(UID = c('WAZP99-0591','WMTP99-0587','WSDP99-0528','WUTP99-0553','WWYP99-0576'),
                                   METRIC='pctch_u',
                                   RESULT=c( 0,36.363636364,20,0,18.181818182)
                                  )
                       ,data.frame(UID = c('WAZP99-0591','WMTP99-0587','WSDP99-0528','WUTP99-0553','WWYP99-0576'),
                                   METRIC='conbankfull',
                                   RESULT=rep('5.5', 5)
                                  )
                       ,data.frame(UID = c('WAZP99-0591','WMTP99-0587','WSDP99-0528','WUTP99-0553','WWYP99-0576'),
                                   METRIC='constraint',
                                   RESULT=rep('CON_BROAD', 5)
                                  )
                       ,data.frame(UID = c('WAZP99-0591','WMTP99-0587','WSDP99-0528','WUTP99-0553','WWYP99-0576'),
                                   METRIC='confeatures',
                                   RESULT=rep('HILLSLOPE', 5)
                                  )
                       ,data.frame(UID = c('WAZP99-0591','WMTP99-0587','WSDP99-0528','WUTP99-0553','WWYP99-0576'),
                                   METRIC='conpattern',
                                   RESULT=rep('SINGLE', 5)
                                  )
                       ,data.frame(UID = c('WAZP99-0591','WMTP99-0587','WSDP99-0528','WUTP99-0553','WWYP99-0576'),
                                   METRIC='conpercent',
                                   RESULT=rep('100', 5)
                                  )
                       ,data.frame(UID = c('WAZP99-0591','WMTP99-0587','WSDP99-0528','WUTP99-0553','WWYP99-0576'),
                                   METRIC='convalley',
                                   RESULT=rep('1500', 5)
                                  )
                       ,data.frame(UID = c('WAZP99-0591','WMTP99-0587','WSDP99-0528','WUTP99-0553','WWYP99-0576'),
                                   METRIC='convalleybox',
                                   RESULT=rep('Y', 5)
                                  )
                       )

  return(metsExpected)
}

metsChannelchar.createChanChar <- function()
# Create and return a dataframe of simulated channel constraint data from
# tblCHANNELCHAR2.
{
  testData <- expand.grid(PARAMETER=c('BANKFULL','CONSTRNT','FEATURES'
                                     ,'PATTERN','PERCENT','VALLEY','VALLYBOX'
                                     )
                         ,UID = c('WAZP99-0591','WMTP99-0587','WSDP99-0528'
                                 ,'WUTP99-0553','WWYP99-0576'
                                 )
                         )
  testData$PARAMETER <- as.character(testData$PARAMETER)
  testData$UID <- as.character(testData$UID)
  testData$RESULT <- rep(c('5.5','CON_BROAD','HILLSLOPE','SINGLE','100','1500','Y'),5)
  testData$SAMPLE_TYPE <- 'PHAB_CHCON'
  testData$TRANSECT <- 'NONE'
  testData$FLAG <- ''
  
  return(testData)
}