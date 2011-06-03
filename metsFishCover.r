# metsFishCover.r
#
# Calculates Fish Cover portion of the Physical Habitat metrics from
# validated NRSA data.
#
# 12/15/2009  mrc copied fcMets from NLA project

# 12/18/2009  mrc completed creating mets, now write tests
# 12/30/2009  tests complete
# 03/23/2010  cws moved creation of unit test dataframes to separate functions.
# 04/01/2010  ssr created only-boatable and only-wadeable tests 

#wrapper function

metsFishCover <- function ()

# calculate fish cover metrics and saves results to a file.
# Wadeable and Boatable protocol generate the same results:
#  Metrics: pfc_alg, pfc_rck, pfc_brs, pfc_lvt, pfc_aqm, pfc_ohv, pfc_hum, pfc_ucb, pfc_lwd 
#           xfc_alg, xfc_rck, xfc_brs, xfc_lvt, xfc_aqm, xfc_ohv, xfc_hum, xfc_ucb, xfc_lwd 
#           pfc_all, pfc_big, pfc_nat
#           xfc_all, xfc_big, xfc_nat
#           sdfc_ucb, sdfc_ohv
#           idrucb, idrohv, iqrucb, iqrohv
#
# These metrics are saved to a csv file in the directory specified by
# NRSAMetricsLocation.

# Returns NULL if successful or a character string describing the issue
# ARGUMENTS:
#  none

{
  intermediateMessage ('Fish Cover calculations', loc='start')
  
  #read in the data from fishcover2
  
  chan <- odbcConnect ('NRSA2')
  fishcover <- fetchNRSATable (chan, 'tblFISHCOVER2')

  intermediateMessage ('fetch_data.1')
  #determine protocol used for each site
  protocols <- siteProtocol (unique(fishcover$UID))
  
  intermediateMessage ('set_protocols.2')
  
  #calculate the metrics
  mets <- metsFishCover.1 (fishcover, protocols)
   if(is.character(mets)) return (mets)
   
  #write the results
  rc <- writeNRSACalcResults (mets, 'metsFishCover.csv')
  
  intermediateMessage ( ' Done.', loc='end')
  
  return (rc)
}
  
  
  
 metsFishCover.1 <- function (fishcover, protocols)
 
 #Returns a dataframe of calculations if successful or a character string describing the problem if one was encountered.
 
 #ARGUMENTS:
 #fishcover   dataframe of the fishcover data.
 #protocols   dataframe relating UID to the sampling protocol used at that site
 
 {
 #Did the protocols for fun, but the mets for WADEABLE and BOATABLE are the same for fish cover
 
 #do the calculations
  
#fcMets <- function (df)
# Calculate metrics based on Fish Cover data
#
# ARGUMENTS:
#   None
# 
# Calculates the FishCover metrics


# ASSUMPTIONS:
# The parameter vector has the following values: ALGAE, BOULDR,
# BRUSH, LVTREE, MACPHY, OVRHNG, STRUCT, UNDCUT (Wadeable), UNDERCUT (Boatable), WOODY
# change the value of UNDCUT to UNDERCUT to make them compatible

    intermediateMessage('Fish Cover mets', loc='start')

    fcData <- subset(fishcover
                    ,PARAMETER %in% c('ALGAE', 'BOULDR'
                                      ,'BRUSH', 'LVTREE'
                                      ,'MACPHY', 'OVRHNG'
                                      ,'STRUCT', 'UNDCUT'
                                      ,'UNDERCUT', 'WOODY'
                                     )
                    )
    fcData$PARAMETER <- ifelse(fcData$PARAMETER == 'UNDCUT', 'UNDERCUT' , fcData$PARAMETER)                 

    # Create tables for converting field values to calculation values
    cover04<-data.frame(field=c(NA,'0','1','2','3','4')
                       ,calc=c(NA,0,0.05,0.25,0.575,0.875)
                       ,stringsAsFactors=FALSE
                       )

    presence04<-data.frame(field=c(NA,'0','1','2','3','4')
                          ,calc=c(NA,0,1,1,1,1)
                          ,stringsAsFactors=FALSE
                          )

    fcTypes<-data.frame(PARAMETER=c('ALGAE', 'BOULDR', 'BRUSH'
                                   ,'LVTREE', 'MACPHY', 'OVRHNG'
                                   ,'STRUCT', 'UNDERCUT', 'WOODY'
                                   )
                       ,isBig=c(FALSE, TRUE, FALSE
                               ,FALSE, FALSE, FALSE
                               ,TRUE, TRUE,TRUE
                               )
                       ,isNatural=c(FALSE, TRUE, TRUE
                                   ,TRUE, FALSE, TRUE
                                   ,FALSE ,TRUE, TRUE
                                   )
       
                       )

    intermediateMessage('create tables used in calculations.1', loc='start')

    # Calculate presence mean of each type of fish cover
    # Convert field results to calculable values
    fcPresence<-merge(fcData
                     ,presence04
                     ,by.x='RESULT'
                     ,by.y='field'
                     ,all.x=TRUE
                     ,sort=FALSE
                     )

    fcPMeans<-aggregate(fcPresence$calc
                 ,list('UID'=fcPresence$UID
                      ,"PARAMETER"=fcPresence$PARAMETER
                      )
                 ,mean, na.rm=TRUE
                 )
                 

                          
  meanPresence  <-rename(fcPMeans, 'x','fcfp') 
   
  meanPresence$PARAMETER <- ifelse(meanPresence$PARAMETER == 'ALGAE', 'pfc_alg' , meanPresence$PARAMETER)      
  meanPresence$PARAMETER <- ifelse(meanPresence$PARAMETER == 'BOULDR', 'pfc_rck' , meanPresence$PARAMETER)     
  meanPresence$PARAMETER <- ifelse(meanPresence$PARAMETER == 'BRUSH', 'pfc_brs' , meanPresence$PARAMETER) 
  meanPresence$PARAMETER <- ifelse(meanPresence$PARAMETER == 'LVTREE', 'pfc_lvt' , meanPresence$PARAMETER) 
  meanPresence$PARAMETER <- ifelse(meanPresence$PARAMETER == 'MACPHY', 'pfc_aqm' , meanPresence$PARAMETER)  
  meanPresence$PARAMETER <- ifelse(meanPresence$PARAMETER == 'OVRHNG', 'pfc_ohv' , meanPresence$PARAMETER) 
  meanPresence$PARAMETER <- ifelse(meanPresence$PARAMETER == 'STRUCT', 'pfc_hum' , meanPresence$PARAMETER) 
  meanPresence$PARAMETER <- ifelse(meanPresence$PARAMETER == 'UNDERCUT', 'pfc_ucb' , meanPresence$PARAMETER) 
  meanPresence$PARAMETER <- ifelse(meanPresence$PARAMETER == 'WOODY', 'pfc_lwd' , meanPresence$PARAMETER) 
 
     
     
  meanPresence <- rename(meanPresence, c('PARAMETER','fcfp'),c('METRIC' ,'RESULT'))
                         

    intermediateMessage('complete Mean Presence mets (fp).2')


    # Calculate cover mean of each type of fish cover
    # Convert field results to calculable values.
    fcCover<-merge(fcData, cover04
                   ,by.x='RESULT'
                   ,by.y='field'
                   ,all.x=TRUE, sort=FALSE
                   )

    fcCMeans <-aggregate(fcCover$calc
                      ,list('UID'=fcCover$UID
                           ,"PARAMETER"=fcCover$PARAMETER
                           )
                      ,mean, na.rm=TRUE
                      )
 
      
  meanCover  <- rename(fcCMeans, 'x','fcfc')  
      
  meanCover$PARAMETER <- ifelse(meanCover$PARAMETER == 'ALGAE', 'xfc_alg' , meanCover$PARAMETER)      
  meanCover$PARAMETER <- ifelse(meanCover$PARAMETER == 'BOULDR', 'xfc_rck' , meanCover$PARAMETER)     
  meanCover$PARAMETER <- ifelse(meanCover$PARAMETER == 'BRUSH', 'xfc_brs' , meanCover$PARAMETER) 
  meanCover$PARAMETER <- ifelse(meanCover$PARAMETER == 'LVTREE', 'xfc_lvt' , meanCover$PARAMETER) 
  meanCover$PARAMETER <- ifelse(meanCover$PARAMETER == 'MACPHY', 'xfc_aqm' , meanCover$PARAMETER)  
  meanCover$PARAMETER <- ifelse(meanCover$PARAMETER == 'OVRHNG', 'xfc_ohv' , meanCover$PARAMETER) 
  meanCover$PARAMETER <- ifelse(meanCover$PARAMETER == 'STRUCT', 'xfc_hum' , meanCover$PARAMETER) 
  meanCover$PARAMETER <- ifelse(meanCover$PARAMETER == 'UNDERCUT', 'xfc_ucb' , meanCover$PARAMETER) 
  meanCover$PARAMETER <- ifelse(meanCover$PARAMETER == 'WOODY', 'xfc_lwd' , meanCover$PARAMETER) 
 
     
     
  meanCover <- rename(meanCover, c('PARAMETER','fcfc'),c('METRIC' , 'RESULT'))
                                            

    intermediateMessage('Complete mean value mets (xp).3')


    # Calculate cover indices: total, large, and natural vegetation
    #grouped presence
    fcPMeans <- merge(fcPMeans, fcTypes, by='PARAMETER', all=TRUE, sort=FALSE)

    fciPAll <- aggregate(list('fciPAll'=fcPMeans$x)
                       ,list('uid'=fcPMeans$UID)
                       ,sum, na.rm=TRUE
                       )
    
    fciPAll$METRIC <- 'pfc_all'
    fciPAll  <- rename(fciPAll, 'fciPAll','RESULT')  
    fciPAll  <- rename(fciPAll, 'uid','UID')     
    
    ttPBig <- subset(fcPMeans, isBig)
    fciPBig <- aggregate(list('fciPBig'=ttPBig$x)
                       ,list('uid'=ttPBig$UID)
                       ,sum, na.rm=TRUE
                       )
    fciPBig$METRIC <- 'pfc_big'
    fciPBig  <- rename(fciPBig,'fciPBig','RESULT')                    
    fciPBig <- rename(fciPBig, 'uid','UID')                        

    ttPNatural <- subset(fcPMeans, isNatural)
    fciPNatural <- aggregate(list('fciPNatural'=ttPNatural$x)
                           ,list('uid'=ttPNatural$UID)
                           ,sum, na.rm=TRUE
                           )
    fciPNatural$METRIC <- 'pfc_nat'
    fciPNatural  <- rename(fciPNatural, 'fciPNatural','RESULT') 
    fciPNatural  <- rename(fciPNatural,'uid','UID')                      
                       
  
 #   fciP <- merge(fciPAll, fciPBig,  by='uid', all=TRUE, sort=FALSE)
 #   fciP <- merge(fciP, fciPNatural, by='uid', all=TRUE, sort=FALSE)
  
    # Calculate cover indices: total, large, and natural vegetation
    # grouped cover
    fcCMeans <- merge(fcCMeans, fcTypes, by='PARAMETER', all=TRUE, sort=FALSE)

    fciCAll <- aggregate(list('fciCAll'=fcCMeans$x)
                       ,list('uid'=fcCMeans$UID)
                       ,sum, na.rm=TRUE
                       )
    
    fciCAll$METRIC <- 'xfc_all'
    fciCAll  <- rename(fciCAll, 'fciCAll','RESULT')  
    fciCAll  <- rename(fciCAll, 'uid','UID')
      
    ttCBig <- subset(fcCMeans, isBig)
    fciCBig <- aggregate(list('fciCBig'=ttCBig$x)
                       ,list('uid'=ttCBig$UID)
                       ,sum, na.rm=TRUE
                       )
    fciCBig$METRIC <- 'xfc_big'
    fciCBig  <- rename(fciCBig, 'fciCBig','RESULT')                    
    fciCBig  <- rename(fciCBig, 'uid','UID')                 

    ttCNatural <- subset(fcCMeans, isNatural)
    fciCNatural <- aggregate(list('fciCNatural'=ttCNatural$x)
                           ,list('uid'=ttCNatural$UID)
                           ,sum, na.rm=TRUE
                           )
    fciCNatural$METRIC <- 'xfc_nat'
    fciCNatural  <- rename(fciCNatural, 'fciCNatural','RESULT')
    fciCNatural  <- rename(fciCNatural, 'uid','UID')                 
                       
  intermediateMessage('complete type metrics.4', loc='start')

    # Calculate cover standard deviation of OHV and UCB
    
  
  
    tt<-aggregate(fcCover$calc
                 ,list('uid'=fcCover$UID
                      ,"parameter"=fcCover$PARAMETER
                      )
                 ,sd, na.rm=TRUE
                 )
  
    sd1 <- subset(tt, parameter %in% c('UNDERCUT', 'OVRHNG'))
    
  sd1$parameter <- ifelse(sd1$parameter == 'UNDERCUT', 'sdfc_ucb' , sd1$parameter) 
  sd1$parameter <- ifelse(sd1$parameter == 'OVRHNG', 'sdfc_ohv' , sd1$parameter) 
  sd1  <- rename(sd1, 'uid','UID')
  sd1  <- rename(sd1, 'x','RESULT') 
  sd1  <- rename(sd1, 'parameter','METRIC') 
     

    intermediateMessage('complete sd calculations.5', loc='start')


    # Calculate iqr, idr for OVRHNG and UNDERCUT midpoints
    
     tt<-aggregate(fcCover$calc
                 ,list('uid'=fcCover$UID
                      ,"parameter"=fcCover$PARAMETER
                      )
                 ,iqr
                 )
  
    idr1 <- subset(tt, parameter %in% c('UNDERCUT', 'OVRHNG'))
  
  idr1$parameter <- ifelse(idr1$parameter == 'UNDERCUT', 'idrucb' , idr1$parameter) 
  idr1$parameter <- ifelse(idr1$parameter == 'OVRHNG', 'idrohv' , idr1$parameter) 
  idr1  <- rename(idr1, 'uid','UID')
  idr1  <- rename(idr1, 'x','RESULT') 
  idr1  <- rename(idr1, 'parameter','METRIC') 
  
  #iqr
  
       tt<-aggregate(fcCover$calc
                 ,list('uid'=fcCover$UID
                      ,"parameter"=fcCover$PARAMETER
                      )
                 ,iqr
                 )
  
    iqr1 <- subset(tt, parameter %in% c('UNDERCUT', 'OVRHNG'))
  
  iqr1$parameter <- ifelse(iqr1$parameter == 'UNDERCUT', 'iqrucb' , iqr1$parameter) 
  iqr1$parameter <- ifelse(iqr1$parameter == 'OVRHNG', 'iqrohv' , iqr1$parameter) 
  iqr1  <- rename(iqr1, 'uid','UID')
  iqr1  <- rename(iqr1, 'x','RESULT') 
  iqr1  <- rename(iqr1, 'parameter','METRIC') 
  
  
  
    intermediateMessage('complete id and iq calculations.6', loc='start')

    # combine results into a dataframe
    
    
  mets <- rbind (meanPresence
                         , meanCover                  
                         ,fciPNatural
                         ,fciPBig
                         ,fciPAll
                         ,fciCNatural
                         ,fciCBig
                         ,fciCAll
                         ,sd1
                         ,idr1
                         ,iqr1)
    
  
    intermediateMessage(' Done.', loc='end')

    return(mets)
}


#unit tests for metsFishCover

metsFishCoverTest <- function ()

{
  # Create correctly formated test data, and run data through metsFishCover.1
  testData <- metsFishCover.createData()
  testData.River <- subset(testData, UID %in% c('1','2','3','4','5'))
  testData.Stream <- subset(testData, UID %in% c('6','7','8','9','10'))

  testDataResult <- metsFishCover.1(testData)
  testDataResult.River <- metsFishCover.1(testData.River)
  testDataResult.Stream <- metsFishCover.1(testData.Stream)
  
  #create the expected results (mets) for the test data using outside calculations
  metsExpected <- metsFishCover.createResults()
  
  #compare results from baseData (testDataResult) with expectedResults  (metsExpected)
  # Calculated values should be within 10E-7 of expected values, should
  # only be missing where they are supposed to be missing and nonmissing where
  # they are supposed to be nonmissing.
  # Note: the errs dataframe can be printed to show where the errors occur when
  # debugging.
  metsExpected <- rename(metsExpected, 'RESULT','EXPECTED')
  rr <- testDataResult

  tt <- merge(rr, metsExpected, by=c('UID','METRIC'),all=T)
  tt$diff <- tt$RESULT - tt$EXPECTED
  errs <- subset(tt, abs(diff) > 10^-7 | is.na(RESULT) != is.na(EXPECTED))
  checkEquals(0, nrow(errs)
             ,"Error: Fish Cover metrics (stream and river) are broken"
             )
             
  metsExpected.River <- subset(metsExpected, UID %in% c('1','2','3','4','5'))
  rr <- testDataResult.River

  tt <- merge(rr, metsExpected.River, by=c('UID','METRIC'),all=T)
  tt$diff <- tt$RESULT - tt$EXPECTED
  errs <- subset(tt, abs(diff) > 10^-7 | is.na(RESULT) != is.na(EXPECTED))
  checkEquals(0, nrow(errs)
             ,"Error: Fish Cover metrics (river) are broken"
             )

  metsExpected.Stream <- subset(metsExpected, UID %in% c('6','7','8','9','10'))
  rr <- testDataResult.Stream

  tt <- merge(rr, metsExpected.Stream, by=c('UID','METRIC'),all=T)
  tt$diff <- tt$RESULT - tt$EXPECTED
  errs <- subset(tt, abs(diff) > 10^-7 | is.na(RESULT) != is.na(EXPECTED))
  checkEquals(0, nrow(errs)
             ,"Error: Fish Cover metrics (stream) are broken"
             )
}


metsFishCover.createData <- function()
#
{
  boatData <- rbind(expand.grid(UID = 1:5
                               ,TRANSECT = LETTERS[1:11]
                               ,SAMPLE_TYPE = 'PHAB_CHANB'
                               ,PARAMETER = c('ALGAE', 'BOULDR', 'BRUSH'
                                             ,'LVTREE', 'MACPHY', 'OVRHNG'
                                             ,'STRUCT', 'UNDERCUT', 'WOODY'
                                             )
                               )
                    )
  boatData$UID <- as.character(boatData$UID)
  boatData$TRANSECT <- as.character(boatData$TRANSECT)
  boatData$SAMPLE_TYPE <- as.character(boatData$SAMPLE_TYPE)
  boatData$PARAMETER <- as.character(boatData$PARAMETER)
  boatData$RESULT <- rep(as.character(0:4), length.out=nrow(boatData))
  boatData$FLAG <- as.character(NA)

    strmData <- rbind(expand.grid(UID = 6:10
                               ,TRANSECT = LETTERS[1:11]
                               ,SAMPLE_TYPE = 'PHAB_CHANB'
                               ,PARAMETER = c('ALGAE', 'BOULDR', 'BRUSH'
                                             ,'LVTREE', 'MACPHY', 'OVRHNG'
                                             ,'STRUCT', 'UNDCUT', 'WOODY'
                                             )
                               )
                    )
  strmData$UID <- as.character(strmData$UID)
  strmData$TRANSECT <- as.character(strmData$TRANSECT)
  strmData$SAMPLE_TYPE <- as.character(strmData$SAMPLE_TYPE)
  strmData$PARAMETER <- as.character(strmData$PARAMETER)
  strmData$RESULT <- rep(as.character(0:4), length.out=nrow(strmData))
  strmData$FLAG <- as.character(NA)

  testData <- rbind(boatData,strmData)

  return(testData)
}


metsFishCover.createResults <- function()
#
{
  mets<- rbind(data.frame(UID=rep('1',30)
                         ,METRIC=c('pfc_alg', 'pfc_rck', 'pfc_brs', 'pfc_lvt'
                                  ,'pfc_aqm', 'pfc_ohv', 'pfc_hum', 'pfc_ucb', 'pfc_lwd'
                                  ,'xfc_alg', 'xfc_rck', 'xfc_brs', 'xfc_lvt'
                                  ,'xfc_aqm','xfc_ohv', 'xfc_hum', 'xfc_ucb', 'xfc_lwd'
                                  ,'pfc_all', 'pfc_big', 'pfc_nat'
                                  ,'xfc_all', 'xfc_big', 'xfc_nat'
                                  ,'sdfc_ucb', 'sdfc_ohv'
                                  ,'idrucb', 'idrohv', 'iqrucb', 'iqrohv'
                                   )
                         ,RESULT=c(0,         0,          0,         0
                                  ,0,         0,          0,         0,          0
                                  ,0,         0,          0,         0
                                  ,0,         0,          0,         0,          0
                                  ,0,         0,          0
                                  ,0,         0,          0
                                  ,0,         0
                                  ,0,         0,          0,         0
                                  )
                         )
              ,data.frame(UID=rep('10',30)
                         ,METRIC=c('pfc_alg', 'pfc_rck', 'pfc_brs', 'pfc_lvt'
                                  ,'pfc_aqm', 'pfc_ohv', 'pfc_hum', 'pfc_ucb', 'pfc_lwd'
                                  ,'xfc_alg', 'xfc_rck', 'xfc_brs', 'xfc_lvt'
                                  ,'xfc_aqm','xfc_ohv', 'xfc_hum', 'xfc_ucb', 'xfc_lwd'
                                  ,'pfc_all', 'pfc_big', 'pfc_nat'
                                  ,'xfc_all', 'xfc_big', 'xfc_nat'
                                  ,'sdfc_ucb', 'sdfc_ohv'
                                  ,'idrucb', 'idrohv', 'iqrucb', 'iqrohv'
                                   )
                         ,RESULT=c(1,         1,          1,         1
                                  ,1,         1,          1,         1,          1
                                  ,0.875,     0.875,      0.875,     0.875
                                  ,0.875,     0.875,      0.875,     0.875,      0.875
                                  ,9,         4,          6
                                  ,7.875,     3.5,        5.25
                                  ,0,         0
                                  ,0,         0,          0,         0
                                  )
                         )
              ,data.frame(UID=rep('2',30)
                         ,METRIC=c('pfc_alg', 'pfc_rck', 'pfc_brs', 'pfc_lvt'
                                  ,'pfc_aqm', 'pfc_ohv', 'pfc_hum', 'pfc_ucb', 'pfc_lwd'
                                  ,'xfc_alg', 'xfc_rck', 'xfc_brs', 'xfc_lvt'
                                  ,'xfc_aqm','xfc_ohv', 'xfc_hum', 'xfc_ucb', 'xfc_lwd'
                                  ,'pfc_all', 'pfc_big', 'pfc_nat'
                                  ,'xfc_all', 'xfc_big', 'xfc_nat'
                                  ,'sdfc_ucb', 'sdfc_ohv'
                                  ,'idrucb', 'idrohv', 'iqrucb', 'iqrohv'
                                   )
                         ,RESULT=c(1,         1,          1,         1
                                  ,1,         1,          1,         1,          1
                                  ,0.05,      0.05,       0.05,      0.05
                                  ,0.05,      0.05,       0.05,      0.05,      0.05
                                  ,9,         4,          6
                                  ,0.45,      0.2,        0.3
                                  ,0,         0
                                  ,0,         0,          0,         0
                                  )
                         )
              ,data.frame(UID=rep('3',30)
                         ,METRIC=c('pfc_alg', 'pfc_rck', 'pfc_brs', 'pfc_lvt'
                                  ,'pfc_aqm', 'pfc_ohv', 'pfc_hum', 'pfc_ucb', 'pfc_lwd'
                                  ,'xfc_alg', 'xfc_rck', 'xfc_brs', 'xfc_lvt'
                                  ,'xfc_aqm','xfc_ohv', 'xfc_hum', 'xfc_ucb', 'xfc_lwd'
                                  ,'pfc_all', 'pfc_big', 'pfc_nat'
                                  ,'xfc_all', 'xfc_big', 'xfc_nat'
                                  ,'sdfc_ucb', 'sdfc_ohv'
                                  ,'idrucb', 'idrohv', 'iqrucb', 'iqrohv'
                                   )
                         ,RESULT=c(1,         1,          1,         1
                                  ,1,         1,          1,         1,          1
                                  ,0.25,      0.25,       0.25,      0.25
                                  ,0.25,      0.25,       0.25,      0.25,      0.25
                                  ,9,         4,          6
                                  ,2.25,      1,          1.5
                                  ,0,         0
                                  ,0,         0,          0,         0
                                  )
                         )
              ,data.frame(UID=rep('4',30)
                         ,METRIC=c('pfc_alg', 'pfc_rck', 'pfc_brs', 'pfc_lvt'
                                  ,'pfc_aqm', 'pfc_ohv', 'pfc_hum', 'pfc_ucb', 'pfc_lwd'
                                  ,'xfc_alg', 'xfc_rck', 'xfc_brs', 'xfc_lvt'
                                  ,'xfc_aqm','xfc_ohv', 'xfc_hum', 'xfc_ucb', 'xfc_lwd'
                                  ,'pfc_all', 'pfc_big', 'pfc_nat'
                                  ,'xfc_all', 'xfc_big', 'xfc_nat'
                                  ,'sdfc_ucb', 'sdfc_ohv'
                                  ,'idrucb', 'idrohv', 'iqrucb', 'iqrohv'
                                   )
                         ,RESULT=c(1,         1,          1,         1
                                  ,1,         1,          1,         1,          1
                                  ,0.575,     0.575,      0.575,     0.575
                                  ,0.575,     0.575,      0.575,     0.575,      0.575
                                  ,9,         4,          6
                                  ,5.175,     2.3,        3.45
                                  ,0,         0
                                  ,0,         0,          0,         0
                                  )
                         )


              ,data.frame(UID=rep('5',30)
                         ,METRIC=c('pfc_alg', 'pfc_rck', 'pfc_brs', 'pfc_lvt'
                                  ,'pfc_aqm', 'pfc_ohv', 'pfc_hum', 'pfc_ucb', 'pfc_lwd'
                                  ,'xfc_alg', 'xfc_rck', 'xfc_brs', 'xfc_lvt'
                                  ,'xfc_aqm','xfc_ohv', 'xfc_hum', 'xfc_ucb', 'xfc_lwd'
                                  ,'pfc_all', 'pfc_big', 'pfc_nat'
                                  ,'xfc_all', 'xfc_big', 'xfc_nat'
                                  ,'sdfc_ucb', 'sdfc_ohv'
                                  ,'idrucb', 'idrohv', 'iqrucb', 'iqrohv'
                                   )
                         ,RESULT=c(1,         1,          1,         1
                                  ,1,         1,          1,         1,          1
                                  ,0.875,     0.875,      0.875,     0.875
                                  ,0.875,     0.875,      0.875,     0.875,      0.875
                                  ,9,         4,          6
                                  ,7.875,     3.5,        5.25
                                  ,0,         0
                                  ,0,         0,          0,         0
                                  )
                         )

              ,data.frame(UID=rep('6',30)
                         ,METRIC=c('pfc_alg', 'pfc_rck', 'pfc_brs', 'pfc_lvt'
                                  ,'pfc_aqm', 'pfc_ohv', 'pfc_hum', 'pfc_ucb', 'pfc_lwd'
                                  ,'xfc_alg', 'xfc_rck', 'xfc_brs', 'xfc_lvt'
                                  ,'xfc_aqm','xfc_ohv', 'xfc_hum', 'xfc_ucb', 'xfc_lwd'
                                  ,'pfc_all', 'pfc_big', 'pfc_nat'
                                  ,'xfc_all', 'xfc_big', 'xfc_nat'
                                  ,'sdfc_ucb', 'sdfc_ohv'
                                  ,'idrucb', 'idrohv', 'iqrucb', 'iqrohv'
                                   )
                         ,RESULT=c(0,         0,          0,         0
                                  ,0,         0,          0,         0,          0
                                  ,0,         0,          0,         0
                                  ,0,         0,          0,         0,          0
                                  ,0,         0,          0
                                  ,0,         0,          0
                                  ,0,         0
                                  ,0,         0,          0,         0
                                  )
                         )

              ,data.frame(UID=rep('7',30)
                         ,METRIC=c('pfc_alg', 'pfc_rck', 'pfc_brs', 'pfc_lvt'
                                  ,'pfc_aqm', 'pfc_ohv', 'pfc_hum', 'pfc_ucb', 'pfc_lwd'
                                  ,'xfc_alg', 'xfc_rck', 'xfc_brs', 'xfc_lvt'
                                  ,'xfc_aqm','xfc_ohv', 'xfc_hum', 'xfc_ucb', 'xfc_lwd'
                                  ,'pfc_all', 'pfc_big', 'pfc_nat'
                                  ,'xfc_all', 'xfc_big', 'xfc_nat'
                                  ,'sdfc_ucb', 'sdfc_ohv'
                                  ,'idrucb', 'idrohv', 'iqrucb', 'iqrohv'
                                   )
                         ,RESULT=c(1,         1,          1,         1
                                  ,1,         1,          1,         1,          1
                                  ,0.05,      0.05,       0.05,      0.05
                                  ,0.05,      0.05,       0.05,      0.05,      0.05
                                  ,9,         4,          6
                                  ,0.45,      0.2,        0.3
                                  ,0,         0
                                  ,0,         0,          0,         0
                                  )
                         )
              ,data.frame(UID=rep('8',30)
                         ,METRIC=c('pfc_alg', 'pfc_rck', 'pfc_brs', 'pfc_lvt'
                                  ,'pfc_aqm', 'pfc_ohv', 'pfc_hum', 'pfc_ucb', 'pfc_lwd'
                                  ,'xfc_alg', 'xfc_rck', 'xfc_brs', 'xfc_lvt'
                                  ,'xfc_aqm','xfc_ohv', 'xfc_hum', 'xfc_ucb', 'xfc_lwd'
                                  ,'pfc_all', 'pfc_big', 'pfc_nat'
                                  ,'xfc_all', 'xfc_big', 'xfc_nat'
                                  ,'sdfc_ucb', 'sdfc_ohv'
                                  ,'idrucb', 'idrohv', 'iqrucb', 'iqrohv'
                                   )
                         ,RESULT=c(1,         1,          1,         1
                                  ,1,         1,          1,         1,          1
                                  ,0.25,      0.25,       0.25,      0.25
                                  ,0.25,      0.25,       0.25,      0.25,      0.25
                                  ,9,         4,          6
                                  ,2.25,      1,          1.5
                                  ,0,         0
                                  ,0,         0,          0,         0
                                  )
                         )
              ,data.frame(UID=rep('9',30)
                         ,METRIC=c('pfc_alg', 'pfc_rck', 'pfc_brs', 'pfc_lvt'
                                  ,'pfc_aqm', 'pfc_ohv', 'pfc_hum', 'pfc_ucb', 'pfc_lwd'
                                  ,'xfc_alg', 'xfc_rck', 'xfc_brs', 'xfc_lvt'
                                  ,'xfc_aqm','xfc_ohv', 'xfc_hum', 'xfc_ucb', 'xfc_lwd'
                                  ,'pfc_all', 'pfc_big', 'pfc_nat'
                                  ,'xfc_all', 'xfc_big', 'xfc_nat'
                                  ,'sdfc_ucb', 'sdfc_ohv'
                                  ,'idrucb', 'idrohv', 'iqrucb', 'iqrohv'
                                   )
                         ,RESULT=c(1,         1,          1,         1
                                  ,1,         1,          1,         1,          1
                                  ,0.575,     0.575,      0.575,     0.575
                                  ,0.575,     0.575,      0.575,     0.575,      0.575
                                  ,9,         4,          6
                                  ,5.175,     2.3,        3.45
                                  ,0,         0
                                  ,0,         0,          0,         0
                                  )
                         )
              )

  return(mets)
}

# end of file


                                  
