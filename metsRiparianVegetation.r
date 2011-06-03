# metsRiparianVegetation.r
#
#  2/17/10 cws Created
#  3/22/10 cws Added missing call to checkEquals!
#  3/25/10 cws Changed diff() calls to dfCompare(), nlaLengthen() to dfLengthen().
#  3/31/10 cws Added on.exit() call.

require(RODBC)
require(RUnit)

metsRiparianVegetation <- function()
# Calculate riparian vegetation metrics
#
# These metrics are saved to a csv file in the directory specified by
# NRSAMetricsLocation.
#
# Returns NULL on success, or a character string describing the problem if one
# occurs.
#
# ARGUMENTS:
# none
{
  # Read in thalweg data and retain only required parameters
  chan <- odbcConnect('NRSA2')
  visrip <- fetchNRSATable(chan, 'tblVISRIP2')
  on.exit(odbcClose(chan))
  if(is.character(visrip)) return(visrip)
  
  visrip <- subset(visrip
                  ,PARAMETER %in% c('CANBTRE','CANSTRE','CANVEG'
                                   ,'UNDWDY','UNDNWDY','UNDERVEG'
                                   ,'GCWDY','GCNWDY','BARE'
                                   )
                  )

  # Calculate rp mets and write them to file.
  mets <- metsRiparianVegetation.1(visrip)
  if(is.character(mets)) return(mets)

  rc <- writeNRSACalcResults(mets, 'metsRiparianVegetation.csv')
  return(rc)
}

metsRiparianVegetation.1 <- function(visrip)
# Calculates visible riparian vegetation metrics.
# Returns a dataframe of the metrics upon completion, or a character string
# describing the problem if one occurs.
#
# ARGUMENTS:
# visrip    dataframe with visible riparian vegetation data
#
{
  intermediateMessage('Starting riparian vegetation metrics', loc='start')
  
  # Canopy type fractions
  tt <- subset(visrip, PARAMETER=='CANVEG')
  pcan_c <- aggregate(list('RESULT' = tt$RESULT)
                     ,list('UID' = tt$UID)
                     ,function(x) { mean(x=='C', na.rm=TRUE) }
                     )
  pcan_c$METRIC<-'pcan_c'
  pcan_d <- aggregate(list('RESULT' = tt$RESULT)
                     ,list('UID' = tt$UID)
                     ,function(x) { mean(x=='D', na.rm=TRUE) }
                     )
  pcan_d$METRIC<-'pcan_d'
  pcan_e <- aggregate(list('RESULT' = tt$RESULT)
                     ,list('UID' = tt$UID)
                     ,function(x) { mean(x=='E', na.rm=TRUE) }
                     )
  pcan_e$METRIC<-'pcan_e'
  pcan_m <- aggregate(list('RESULT' = tt$RESULT)
                     ,list('UID' = tt$UID)
                     ,function(x) { mean(x=='M', na.rm=TRUE) }
                     )
  pcan_m$METRIC<-'pcan_m'
  pcan_n <- aggregate(list('RESULT' = tt$RESULT)
                     ,list('UID' = tt$UID)
                     ,function(x) { mean(x=='N', na.rm=TRUE) }
                     )
  pcan_n$METRIC<-'pcan_n'

  canopyTypes <- rbind(pcan_c, pcan_d, pcan_e, pcan_m, pcan_n)
  intermediateMessage('.1')
  
  # Mid-layer type fractions
  tt <- subset(visrip, PARAMETER=='UNDERVEG')
  pmid_c <- aggregate(list('RESULT' = tt$RESULT)
                     ,list('UID' = tt$UID)
                     ,function(x) { mean(x=='C', na.rm=TRUE) }
                     )
  pmid_c$METRIC<-'pmid_c'
  pmid_d <- aggregate(list('RESULT' = tt$RESULT)
                     ,list('UID' = tt$UID)
                     ,function(x) { mean(x=='D', na.rm=TRUE) }
                     )
  pmid_d$METRIC<-'pmid_d'
  pmid_e <- aggregate(list('RESULT' = tt$RESULT)
                     ,list('UID' = tt$UID)
                     ,function(x) { mean(x=='E', na.rm=TRUE) }
                     )
  pmid_e$METRIC<-'pmid_e'
  pmid_m <- aggregate(list('RESULT' = tt$RESULT)
                     ,list('UID' = tt$UID)
                     ,function(x) { mean(x=='M', na.rm=TRUE) }
                     )
  pmid_m$METRIC<-'pmid_m'
  pmid_n <- aggregate(list('RESULT' = tt$RESULT)
                     ,list('UID' = tt$UID)
                     ,function(x) { mean(x=='N', na.rm=TRUE) }
                     )
  pmid_n$METRIC<-'pmid_n'

  midlayerTypes <- rbind(pmid_c, pmid_d, pmid_e, pmid_m, pmid_n)
  intermediateMessage('.2')


  # Vegetation class area cover characterizations -- individual classes
  # Use arithmetic means of end points to numerically characterize each cover
  # class.
  cover04<-data.frame(field=c(NA,'0','1','2','3','4')
                     ,calc=c(NA,0,0.05,0.25,0.575,0.875)
                     ,stringsAsFactors=FALSE
                     )
            
  tt <- merge(subset(visrip, PARAMETER=='CANBTRE')
            ,cover04
            ,by.x='RESULT', by.y='field'
            ,all.x=TRUE
            )
  xcl <- aggregate(list('RESULT'=tt$calc)
                  ,list('UID'=tt$UID)
                  ,mean, na.rm=TRUE
                  )
  xcl$METRIC <- 'xcl'

  tt <- merge(subset(visrip, PARAMETER=='CANSTRE')
            ,cover04
            ,by.x='RESULT', by.y='field'
            ,all.x=TRUE
            )
  xcs <- aggregate(list('RESULT'=tt$calc)
                  ,list('UID'=tt$UID)
                  ,mean, na.rm=TRUE
                  )
  xcs$METRIC <- 'xcs'

  tt <- merge(subset(visrip, PARAMETER=='UNDWDY')
            ,cover04
            ,by.x='RESULT', by.y='field'
            ,all.x=TRUE
            )
  xmw <- aggregate(list('RESULT'=tt$calc)
                  ,list('UID'=tt$UID)
                  ,mean, na.rm=TRUE
                  )
  xmw$METRIC <- 'xmw'

  tt <- merge(subset(visrip, PARAMETER=='UNDNWDY')
            ,cover04
            ,by.x='RESULT', by.y='field'
            ,all.x=TRUE
            )
  xmh <- aggregate(list('RESULT'=tt$calc)
                  ,list('UID'=tt$UID)
                  ,mean, na.rm=TRUE
                  )
  xmh$METRIC <- 'xmh'

  tt <- merge(subset(visrip, PARAMETER=='GCWDY')
            ,cover04
            ,by.x='RESULT', by.y='field'
            ,all.x=TRUE
            )
  xgw <- aggregate(list('RESULT'=tt$calc)
                  ,list('UID'=tt$UID)
                  ,mean, na.rm=TRUE
                  )
  xgw$METRIC <- 'xgw'

  tt <- merge(subset(visrip, PARAMETER=='GCNWDY')
            ,cover04
            ,by.x='RESULT', by.y='field'
            ,all.x=TRUE
            )
  xgh <- aggregate(list('RESULT'=tt$calc)
                  ,list('UID'=tt$UID)
                  ,mean, na.rm=TRUE
                  )
  xgh$METRIC <- 'xgh'

  tt <- merge(subset(visrip, PARAMETER=='BARE')
            ,cover04
            ,by.x='RESULT', by.y='field'
            ,all.x=TRUE
            )
  xgb <- aggregate(list('RESULT'=tt$calc)
                  ,list('UID'=tt$UID)
                  ,mean, na.rm=TRUE
                  )
  xgb$METRIC <- 'xgb'
  
  individualCovers <- rbind(xcl, xcs, xmw, xmh, xgw, xgh, xgb)
  intermediateMessage('.3')

  
  # Vegetation class area cover characterizations -- combinations of classes
  tt <- merge(subset(visrip, PARAMETER %in% c('CANBTRE','CANSTRE'))
             ,cover04
             ,by.x='RESULT', by.y='field'
             ,all.x=TRUE
             )
  ss <- aggregate(tt$calc
                 ,list('UID'=tt$UID, 'TRANSECT'=tt$TRANSECT, 'TRANSDIR'=tt$TRANSDIR)
                 ,function(x) {if(all(is.na(x))) { NA } else {sum(x, na.rm=TRUE) } }
                 )
  xc <- aggregate(list('RESULT'=ss$x)
                 ,list('UID'=ss$UID)
                 ,mean, na.rm=TRUE
                 )
  xc$METRIC <- 'xc'

  tt <- merge(subset(visrip, PARAMETER %in% c('UNDWDY','UNDNWDY'))
             ,cover04
             ,by.x='RESULT', by.y='field'
             ,all.x=TRUE
             )
  ss <- aggregate(tt$calc
                 ,list('UID'=tt$UID, 'TRANSECT'=tt$TRANSECT, 'TRANSDIR'=tt$TRANSDIR)
                 ,function(x) {if(all(is.na(x))) { NA } else {sum(x, na.rm=TRUE) } }
                 )
  xm <- aggregate(list('RESULT'=ss$x)
                 ,list('UID'=ss$UID)
                 ,mean, na.rm=TRUE
                 )
  xm$METRIC <- 'xm'

  tt <- merge(subset(visrip, PARAMETER %in% c('CANBTRE','CANSTRE','UNDWDY'))
             ,cover04
             ,by.x='RESULT', by.y='field'
             ,all.x=TRUE
             )
  ss <- aggregate(tt$calc
                 ,list('UID'=tt$UID, 'TRANSECT'=tt$TRANSECT, 'TRANSDIR'=tt$TRANSDIR)
                 ,function(x) {if(all(is.na(x))) { NA } else {sum(x, na.rm=TRUE) } }
                 )
  xcmw <- aggregate(list('RESULT'=ss$x)
                 ,list('UID'=ss$UID)
                 ,mean, na.rm=TRUE
                 )
  xcmw$METRIC <- 'xcmw'

  tt <- merge(subset(visrip, PARAMETER %in% c('CANBTRE','CANSTRE','UNDWDY','UNDNWDY'))
             ,cover04
             ,by.x='RESULT', by.y='field'
             ,all.x=TRUE
             )
  ss <- aggregate(tt$calc
                 ,list('UID'=tt$UID, 'TRANSECT'=tt$TRANSECT, 'TRANSDIR'=tt$TRANSDIR)
                 ,function(x) {if(all(is.na(x))) { NA } else {sum(x, na.rm=TRUE) } }
                 )
  xcm <- aggregate(list('RESULT'=ss$x)
                 ,list('UID'=ss$UID)
                 ,mean, na.rm=TRUE
                 )
  xcm$METRIC <- 'xcm'

  tt <- merge(subset(visrip, PARAMETER %in% c('GCWDY','GCNWDY'))
             ,cover04
             ,by.x='RESULT', by.y='field'
             ,all.x=TRUE
             )
  ss <- aggregate(tt$calc
                 ,list('UID'=tt$UID, 'TRANSECT'=tt$TRANSECT, 'TRANSDIR'=tt$TRANSDIR)
                 ,function(x) {if(all(is.na(x))) { NA } else {sum(x, na.rm=TRUE) } }
                 )
  xg <- aggregate(list('RESULT'=ss$x)
                 ,list('UID'=ss$UID)
                 ,mean, na.rm=TRUE
                 )
  xg$METRIC <- 'xg'

  tt <- merge(subset(visrip, PARAMETER %in% c('CANBTRE','CANSTRE','UNDWDY','GCWDY'))
             ,cover04
             ,by.x='RESULT', by.y='field'
             ,all.x=TRUE
             )
  ss <- aggregate(tt$calc
                 ,list('UID'=tt$UID, 'TRANSECT'=tt$TRANSECT, 'TRANSDIR'=tt$TRANSDIR)
                 ,function(x) {if(all(is.na(x))) { NA } else {sum(x, na.rm=TRUE) } }
                 )
  xcmgw <- aggregate(list('RESULT'=ss$x)
                 ,list('UID'=ss$UID)
                 ,mean, na.rm=TRUE
                 )
  xcmgw$METRIC <- 'xcmgw'


  tt <- merge(subset(visrip, PARAMETER %in% c('CANBTRE','CANSTRE'
                                             ,'UNDWDY','UNDNWDY'
                                             ,'GCWDY','GCNWDY'
                                             )
                    )
             ,cover04
             ,by.x='RESULT', by.y='field'
             ,all.x=TRUE
             )
  ss <- aggregate(tt$calc
                 ,list('UID'=tt$UID, 'TRANSECT'=tt$TRANSECT, 'TRANSDIR'=tt$TRANSDIR)
                 ,function(x) {if(all(is.na(x))) { NA } else {sum(x, na.rm=TRUE) } }
                 )
  xcmg <- aggregate(list('RESULT'=ss$x)
                 ,list('UID'=ss$UID)
                 ,mean, na.rm=TRUE
                 )
  xcmg$METRIC <- 'xcmg'

  combinedCovers <- rbind(xc, xm, xcmw, xcm, xg, xcmgw, xcmg)
  intermediateMessage('.4')

  
  # Vegetation class presence characterizations
  visrip$presence <- ifelse(is.na(visrip$RESULT), NA
                    ,ifelse(visrip$RESULT=='0', FALSE, TRUE
                     ))

  tt <- subset(visrip, PARAMETER %in% c('CANBTRE','CANSTRE'))
  ss <- aggregate(tt$presence
                 ,list('UID'=tt$UID, 'TRANSECT'=tt$TRANSECT, 'TRANSDIR'=tt$TRANSDIR)
                 ,any
                 )
  xpcan <- aggregate(list('RESULT'=ss$x)
                    ,list('UID'=ss$UID)
                    ,mean, na.rm=TRUE
                    )
  xpcan$METRIC <- 'xpcan'

  tt <- subset(visrip, PARAMETER %in% c('UNDWDY','UNDNWDY'))
  ss <- aggregate(tt$presence
                 ,list('UID'=tt$UID, 'TRANSECT'=tt$TRANSECT, 'TRANSDIR'=tt$TRANSDIR)
                 ,any
                 )
  xpmid <- aggregate(list('RESULT'=ss$x)
                    ,list('UID'=ss$UID)
                    ,mean, na.rm=TRUE
                    )
  xpmid$METRIC <- 'xpmid'

  tt <- subset(visrip, PARAMETER %in% c('GCWDY','GCNWDY'))
  ss <- aggregate(tt$presence
                 ,list('UID'=tt$UID, 'TRANSECT'=tt$TRANSECT, 'TRANSDIR'=tt$TRANSDIR)
                 ,any
                 )
  xpgveg <- aggregate(list('RESULT'=ss$x)
                    ,list('UID'=ss$UID)
                    ,mean, na.rm=TRUE
                    )
  xpgveg$METRIC <- 'xpgveg'

  tt <- subset(visrip, PARAMETER %in% c('UNDWDY','GCWDY'))
  ss <- aggregate(tt$presence
                 ,list('UID'=tt$UID, 'TRANSECT'=tt$TRANSECT, 'TRANSDIR'=tt$TRANSDIR)
                 ,function(x) {if(all(is.na(x))) { NA } else {all(x, na.rm=TRUE) } }
                 )
  xpmgw <- aggregate(list('RESULT'=ss$x)
                    ,list('UID'=ss$UID)
                    ,mean, na.rm=TRUE
                    )
  xpmgw$METRIC <- 'xpmgw'

  tt <- subset(visrip, PARAMETER %in% c('CANBTRE','CANSTRE'))
  cc <- aggregate(list('can'=tt$presence)
                 ,list('UID'=tt$UID, 'TRANSECT'=tt$TRANSECT, 'TRANSDIR'=tt$TRANSDIR)
                 ,any
                 )
  tt <- subset(visrip, PARAMETER %in% c('UNDWDY','UNDNWDY'))
  uu <- aggregate(list('und'=tt$presence)
                 ,list('UID'=tt$UID, 'TRANSECT'=tt$TRANSECT, 'TRANSDIR'=tt$TRANSDIR)
                 ,any
                 )
  ss <- merge(cc, uu, by=c('UID','TRANSECT','TRANSDIR'), all=TRUE)
  ss$both <- ss$can & ss$und
  xpcm <- aggregate(list('RESULT'=ss$both)
                    ,list('UID'=ss$UID)
                    ,mean, na.rm=TRUE
                    )
  xpcm$METRIC <- 'xpcm'

  tt <- subset(visrip, PARAMETER %in% c('UNDWDY','UNDNWDY'))
  uu <- aggregate(list('und'=tt$presence)
                 ,list('UID'=tt$UID, 'TRANSECT'=tt$TRANSECT, 'TRANSDIR'=tt$TRANSDIR)
                 ,any
                 )
  tt <- subset(visrip, PARAMETER %in% c('GCWDY','GCNWDY'))
  gg <- aggregate(list('gnd'=tt$presence)
                 ,list('UID'=tt$UID, 'TRANSECT'=tt$TRANSECT, 'TRANSDIR'=tt$TRANSDIR)
                 ,any
                 )
  ss <- merge(uu, gg, by=c('UID','TRANSECT','TRANSDIR'), all=TRUE)
  ss$both <- ss$und & ss$gnd
  xpmg <- aggregate(list('RESULT'=ss$both)
                    ,list('UID'=ss$UID)
                    ,mean, na.rm=TRUE
                    )
  xpmg$METRIC <- 'xpmg'

  tt <- subset(visrip, PARAMETER %in% c('CANBTRE','CANSTRE'))
  cc <- aggregate(list('can'=tt$presence)
                 ,list('UID'=tt$UID, 'TRANSECT'=tt$TRANSECT, 'TRANSDIR'=tt$TRANSDIR)
                 ,any
                 )
  tt <- subset(visrip, PARAMETER %in% c('UNDWDY','UNDNWDY'))
  uu <- aggregate(list('und'=tt$presence)
                 ,list('UID'=tt$UID, 'TRANSECT'=tt$TRANSECT, 'TRANSDIR'=tt$TRANSDIR)
                 ,any
                 )
  tt <- subset(visrip, PARAMETER %in% c('GCWDY','GCNWDY'))
  gg <- aggregate(list('gnd'=tt$presence)
                 ,list('UID'=tt$UID, 'TRANSECT'=tt$TRANSECT, 'TRANSDIR'=tt$TRANSDIR)
                 ,any
                 )
  ss <- merge(cc, uu, by=c('UID','TRANSECT','TRANSDIR'), all=TRUE)
  ss <- merge(ss, gg, by=c('UID','TRANSECT','TRANSDIR'), all=TRUE)
  ss$all3 <- ss$can & ss$und & ss$gnd
  xpcmg <- aggregate(list('RESULT'=ss$all3)
                    ,list('UID'=ss$UID)
                    ,mean, na.rm=TRUE
                    )
  xpcmg$METRIC <- 'xpcmg'

  presences <- rbind(xpcan, xpmid, xpgveg, xpmgw, xpcm, xpmg, xpcmg)
  intermediateMessage('.5')

                           
  # Combine groups of metrics and convert NaN calculations to NA
  mets <- rbind(canopyTypes, midlayerTypes, individualCovers, combinedCovers
               ,presences
               )
  mets$RESULT <- as.character(ifelse(is.nan(as.numeric(mets$RESULT)), NA, mets$RESULT))
  intermediateMessage('.  Done.', loc='end')

  return(mets)
}


metsRiparianVegetationTest <- function()
# Unit test for metsRiparianVegetation().
{
  ripData <- metsRiparianVegetation.makeData()

  # Create dataframe with expected results
  expected <- metsRiparianVegetation.makeExpected()

  # Compare expected and calculated results
  results <- metsRiparianVegetation.1(ripData)
  
  expected$RESULT <- as.numeric(expected$RESULT)
  results$RESULT <- as.numeric(results$RESULT)
  errs <- dfCompare(expected, results, c('UID','METRIC'), zeroFudge=1e-4)
#  return(errs)
  checkEquals(NULL, errs, "Error: metsRiparianVegetation is broken.")
}

metsRiparianVegetation.makeData <- function()
# Create dataframe of fake riparian vegetation data. Uses data from the
# following WEMAP sites:
# 2000 WAZP99-0512 1 - site with no missing values
# 2000 WAZP99-0545 1 - site with many missing values, and all missing values
#                      for CANVEG, UNDERVEG
# 2000 WCAP99-0592 1 - site with side channels, one of which is entirely missing
#                      data.
# 2002 WUTP02-R003 1   site with all missing values for BARE
#
# Expected values for metrics were obtained from the wemap calculations and
# were changed only as follows:
#   2000 WAZP99-0545 1 pcan_c, pcan_d, pcan_e, pcan_m, pcan_n, pmid_c, pmid_d,
#                      pmid_e, pmid_m, pmid_n were changed from 0 to NA since
#                      all the values are missing, and NA is thus more correct.
#   2000 WCAP99-0592 1 pcan_c, pcan_d, pcan_e, pcan_m, pcan_n , pmid_c, pmid_d,
#                      pmid_e, pmid_m, pmid_n were not handling cases with some
#                      missing values correctly (the missing values were
#                      included in the sample size).  pcan_c changed from
#                      0.76923077 to 0.83333333333, pcan_m changed from
#                      0.11538462 to 0.125, pcan_n from 0.03846154 to
#                      0.0416666666666667, and pmind_m from 0.92307692 to 1.
{
  #4567890123456789012345678901234567890
  wemapRip <- data.frame(matrix(
                  c('2000 WAZP99-0512 1', 'A', 'LF', '0', '0', 'N', '0', '1', 'D', '0', '3', 0
                   ,'2000 WAZP99-0512 1', 'A', 'RT', '1', '0', 'D', '1', '0', 'D', '1', '1', 1
                   ,'2000 WAZP99-0512 1', 'B', 'LF', '0', '0', 'N', '0', '0', 'N', '1', '2', 1
                   ,'2000 WAZP99-0512 1', 'B', 'RT', '1', '1', 'D', '1', '0', 'D', '1', '2', 1
                   ,'2000 WAZP99-0512 1', 'C', 'LF', '1', '0', 'D', '1', '0', 'D', '0', '2', 1
                   ,'2000 WAZP99-0512 1', 'C', 'RT', '1', '1', 'D', '1', '2', 'D', '0', '2', 0
                   ,'2000 WAZP99-0512 1', 'D', 'LF', '0', '2', 'D', '1', '0', 'D', '1', '2', 0
                   ,'2000 WAZP99-0512 1', 'D', 'RT', '1', '1', 'D', '1', '2', 'D', '1', '2', 0
                   ,'2000 WAZP99-0512 1', 'E', 'LF', '1', '1', 'D', '1', '0', 'D', '0', '1', 0
                   ,'2000 WAZP99-0512 1', 'E', 'RT', '1', '0', 'D', '2', '0', 'D', '0', '1', 0
                   ,'2000 WAZP99-0512 1', 'F', 'LF', '0', '0', 'N', '1', '0', 'D', '1', '1', 1
                   ,'2000 WAZP99-0512 1', 'F', 'RT', '0', '0', 'N', '1', '0', 'D', '1', '1', 0
                   ,'2000 WAZP99-0512 1', 'G', 'LF', '0', '0', 'N', '0', '0', 'N', '1', '1', 2
                   ,'2000 WAZP99-0512 1', 'G', 'RT', '0', '0', 'N', '1', '0', 'D', '1', '2', 0
                   ,'2000 WAZP99-0512 1', 'H', 'LF', '0', '0', 'N', '1', '0', 'D', '0', '1', 2
                   ,'2000 WAZP99-0512 1', 'H', 'RT', '1', '0', 'D', '1', '0', 'D', '0', '1', 0
                   ,'2000 WAZP99-0512 1', 'I', 'LF', '0', '0', 'N', '0', '0', 'N', '1', '1', 3
                   ,'2000 WAZP99-0512 1', 'I', 'RT', '0', '0', 'N', '1', '0', 'D', '2', '1', 0
                   ,'2000 WAZP99-0512 1', 'J', 'LF', '0', '0', 'N', '1', '0', 'D', '1', '1', 1
                   ,'2000 WAZP99-0512 1', 'J', 'RT', '1', '1', 'D', '1', '0', 'D', '2', '1', 1
                   ,'2000 WAZP99-0512 1', 'K', 'LF', '0', '0', 'N', '1', '0', 'D', '1', '1', 3
                   ,'2000 WAZP99-0512 1', 'K', 'RT', '0', '0', 'N', '1', '0', 'D', '1', '2', 1
                   ,'2000 WAZP99-0545 1', 'A', 'LF', NA, NA, NA, NA, NA, NA, '0', '3', 0
                   ,'2000 WAZP99-0545 1', 'A', 'RT', NA, NA, NA, NA, NA, NA, '0', '3', 0
                   ,'2000 WAZP99-0545 1', 'B', 'LF', NA, NA, NA, NA, NA, NA, '0', '3', 0
                   ,'2000 WAZP99-0545 1', 'B', 'RT', NA, NA, NA, NA, NA, NA, '0', '3', 0
                   ,'2000 WAZP99-0545 1', 'C', 'LF', NA, NA, NA, NA, NA, NA, '0', '3', 0
                   ,'2000 WAZP99-0545 1', 'C', 'RT', NA, NA, NA, NA, NA, NA, '0', '3', 0
                   ,'2000 WAZP99-0545 1', 'D', 'LF', NA, NA, NA, NA, NA, NA, '0', '4', 0
                   ,'2000 WAZP99-0545 1', 'D', 'RT', NA, NA, NA, NA, NA, NA, '0', '4', 0
                   ,'2000 WAZP99-0545 1', 'E', 'LF', NA, NA, NA, NA, NA, NA, '0', '3', 0
                   ,'2000 WAZP99-0545 1', 'E', 'RT', NA, NA, NA, NA, NA, NA, '0', '4', 0
                   ,'2000 WAZP99-0545 1', 'F', 'LF', NA, NA, NA, NA, NA, NA, '0', '3', 0
                   ,'2000 WAZP99-0545 1', 'F', 'RT', NA, NA, NA, NA, NA, NA, '0', '3', 0
                   ,'2000 WAZP99-0545 1', 'G', 'LF', NA, NA, NA, NA, NA, NA, '0', '2', 0
                   ,'2000 WAZP99-0545 1', 'G', 'RT', NA, NA, NA, NA, NA, NA, '0', '3', 0
                   ,'2000 WAZP99-0545 1', 'H', 'LF', NA, NA, NA, NA, NA, NA, '0', '4', 0
                   ,'2000 WAZP99-0545 1', 'H', 'RT', NA, NA, NA, NA, NA, NA, '0', '3', 0
                   ,'2000 WAZP99-0545 1', 'I', 'LF', '0', '0', NA, '0', '0', NA, '0', '3', 0
                   ,'2000 WAZP99-0545 1', 'I', 'RT', '0', '0', NA, '0', '0', NA, '0', '3', 0
                   ,'2000 WAZP99-0545 1', 'J', 'LF', '0', '0', NA, '0', '0', NA, '0', '3', 0
                   ,'2000 WAZP99-0545 1', 'J', 'RT', '0', '0', NA, '0', '0', NA, '0', '4', 0
                   ,'2000 WAZP99-0545 1', 'K', 'LF', '0', '0', NA, '0', '0', NA, '0', '3', 0
                   ,'2000 WAZP99-0545 1', 'K', 'RT', '0', '0', NA, '0', '0', NA, '0', '3', 0
                   ,'2000 WCAP99-0592 1', 'A', 'LF', '0', '0', 'N', '1', '4', 'M', '0', '4', 0
                   ,'2000 WCAP99-0592 1', 'A', 'RT', '1', '2', 'M', '2', '2', 'M', '1', '3', 1
                   ,'2000 WCAP99-0592 1', 'B', 'LF', '3', '1', 'M', '2', '2', 'M', '2', '3', 1
                   ,'2000 WCAP99-0592 1', 'B', 'RT', '1', '3', 'M', '2', '2', 'M', '2', '3', 2
                   ,'2000 WCAP99-0592 1', 'C', 'LF', '1', '3', 'C', '2', '2', 'M', '2', '2', 2
                   ,'2000 WCAP99-0592 1', 'C', 'RT', '1', '3', 'C', '2', '2', 'M', '2', '2', 2
                   ,'2000 WCAP99-0592 1', 'D', 'LF', '2', '2', 'C', '3', '1', 'M', '2', '1', 2
                   ,'2000 WCAP99-0592 1', 'D', 'RT', '2', '2', 'C', '2', '1', 'M', '1', '1', 3
                   ,'2000 WCAP99-0592 1', 'E', 'LF', '1', '2', 'C', '2', '2', 'M', '2', '2', 2
                   ,'2000 WCAP99-0592 1', 'E', 'RT', '0', '1', 'C', '2', '2', 'M', '2', '2', 2
                   ,'2000 WCAP99-0592 1', 'F', 'LF', '2', '1', 'C', '2', '2', 'M', '2', '2', 2
                   ,'2000 WCAP99-0592 1', 'F', 'RT', '2', '0', 'C', '2', '2', 'M', '2', '3', 0
                   ,'2000 WCAP99-0592 1', 'G', 'LF', '2', '2', 'C', '2', '2', 'M', '2', '3', 0
                   ,'2000 WCAP99-0592 1', 'G', 'RT', '2', '2', 'C', '2', '1', 'M', '3', '2', 0
                   ,'2000 WCAP99-0592 1', 'H', 'LF', '1', '1', 'C', '3', '2', 'M', '2', '3', 0
                   ,'2000 WCAP99-0592 1', 'H', 'RT', '2', '1', 'C', '2', '2', 'M', '2', '3', 0
                   ,'2000 WCAP99-0592 1', 'I', 'LF', '1', '2', 'C', '1', '2', 'M', '1', '4', 0
                   ,'2000 WCAP99-0592 1', 'I', 'RT', '1', '2', 'C', '3', '2', 'M', '3', '2', 0
                   ,'2000 WCAP99-0592 1', 'J', 'LF', '1', '0', 'C', '1', '1', 'M', '1', '4', 0
                   ,'2000 WCAP99-0592 1', 'J', 'RT', '0', '2', 'C', '3', '1', 'M', '3', '2', 0
                   ,'2000 WCAP99-0592 1', 'K', 'LF', '3', '1', 'C', '3', '1', 'M', '3', '2', 0
                   ,'2000 WCAP99-0592 1', 'K', 'RT', '2', '1', 'C', '2', '1', 'M', '2', '3', 0
                   ,'2000 WCAP99-0592 1', 'XF', 'LF', NA, NA, NA, NA, NA, NA, NA, NA, NA
                   ,'2000 WCAP99-0592 1', 'XF', 'RT', NA, NA, NA, NA, NA, NA, NA, NA, NA
                   ,'2000 WCAP99-0592 1', 'XG', 'LF', '2', '2', 'C', '1', '2', 'M', '3', '2', 0
                   ,'2000 WCAP99-0592 1', 'XG', 'RT', '2', '2', 'C', '2', '2', 'M', '1', '3', 0
                   ,'2002 WUTP02-R003 1', 'A', 'LF', '2', '2', 'C', '3', '3', 'D', '3', '2', NA
                   ,'2002 WUTP02-R003 1', 'A', 'RT', '2', '2', 'C', '3', '3', 'D', '3', '2', NA
                   ,'2002 WUTP02-R003 1', 'B', 'LF', '2', '2', 'C', '3', '3', 'D', '3', '2', NA
                   ,'2002 WUTP02-R003 1', 'B', 'RT', '2', '2', 'C', '3', '3', 'D', '3', '2', NA
                   ,'2002 WUTP02-R003 1', 'C', 'LF', '2', '2', 'C', '3', '3', 'D', '3', '2', NA
                   ,'2002 WUTP02-R003 1', 'C', 'RT', NA, NA, 'C', '3', '3', 'D', NA, NA, NA
                   ,'2002 WUTP02-R003 1', 'D', 'LF', '2', '2', 'C', '3', '3', 'D', '3', '3', NA
                   ,'2002 WUTP02-R003 1', 'D', 'RT', '2', '2', 'C', '3', '3', 'D', '3', '2', NA
                   ,'2002 WUTP02-R003 1', 'E', 'LF', '2', '2', 'C', '3', '3', 'D', '3', '3', NA
                   ,'2002 WUTP02-R003 1', 'E', 'RT', '2', '2', 'C', '3', '3', 'D', '3', '3', NA
                   ,'2002 WUTP02-R003 1', 'F', 'LF', '2', '2', 'D', '3', '3', 'D', '3', '3', NA
                   ,'2002 WUTP02-R003 1', 'F', 'RT', '2', '2', 'C', '3', '3', 'D', '3', '2', NA
                   ,'2002 WUTP02-R003 1', 'G', 'LF', '3', '3', 'C', '2', '3', 'D', '3', '3', NA
                   ,'2002 WUTP02-R003 1', 'G', 'RT', '3', '3', 'C', '3', '3', 'D', '3', '3', NA
                   ,'2002 WUTP02-R003 1', 'H', 'LF', '2', '2', 'D', '3', '3', 'D', '3', '3', NA
                   ,'2002 WUTP02-R003 1', 'H', 'RT', '2', '2', 'D', '3', '3', 'D', '3', '3', NA
                   ,'2002 WUTP02-R003 1', 'I', 'LF', '2', '2', 'D', '3', '3', 'D', '3', '3', NA
                   ,'2002 WUTP02-R003 1', 'I', 'RT', '2', '2', 'D', '3', '3', 'D', '3', '3', NA
                   ,'2002 WUTP02-R003 1', 'J', 'LF', '2', '2', 'D', '3', '3', 'D', '3', '3', NA
                   ,'2002 WUTP02-R003 1', 'J', 'RT', '2', '2', 'D', '3', '3', 'D', '3', '3', NA
                   ,'2002 WUTP02-R003 1', 'K', 'LF', '2', '2', 'C', '3', '3', 'D', '3', '2', NA
                   ,'2002 WUTP02-R003 1', 'K', 'RT', '2', '2', 'C', '3', '3', 'D', '3', '2', NA
                   )
                   ,ncol=12, byrow=TRUE
                 ) # end of matrix() call
                 ,stringsAsFactors=FALSE
             ) # end of data.frame() call

  names(wemapRip) <- c('UID','TRANSECT','TRANSDIR','CANBTRE','CANSTRE','CANVEG'
                      ,'UNDWDY','UNDNWDY','UNDERVEG','GCWDY','GCNWDY','BARE'
                      )

  fakeVisRip <- dfLengthen(wemapRip
                          ,c('UID','TRANSECT','TRANSDIR')
                          ,'PARAMETER'
                          ,'RESULT'
                          ,names(wemapRip)[!(names(wemapRip) %in%
                                               c('UID','TRANSECT','TRANSDIR')
                                            )]
                          )

  return(fakeVisRip)
}


metsRiparianVegetation.makeExpected <- function()
# Create dataframe of expected metrics calculations for unit test
{
  texpected <- data.frame(matrix(
                  c('2000 WAZP99-0512 1', 0, 0.45454545, 0, 0, 0.54545455
                                        , 0, 0.86363636, 0, 0, 0.13636364
                                        , 0.02045455, 0.02272727, 0.05, 0.025, 0.05227273, 0.14659091, 0.09318182
                                        , 0.04318182, 0.075, 0.09318182, 0.11818182, 0.19886364, 0.14545455, 0.31704545
                                        , 0.45454545, 0.86363636, 1, 0.54545455, 0.45454545, 0.86363636, 0.45454545
                   ,'2000 WAZP99-0545 1', NA, NA, NA, NA, NA
                                        , NA, NA, NA, NA, NA
                                        , 0, 0, 0, 0, 0, 0.62840909, 0
                                        , 0, 0, 0, 0, 0.62840909, 0, 0.62840909
                                        , 0, 0, 1, 0, 0, 0, 0
                   ,'2000 WCAP99-0592 1', 0.83333333, 0, 0, 0.125, 0.041666667
                                        , 0, 0, 0, 1, 0
                                        , 0.17083333, 0.20104167, 0.284375, 0.21770833, 0.265625, 0.43333333, 0.10104167
                                        , 0.371875, 0.50208333, 0.65625, 0.87395833, 0.69895833, 0.921875, 1.57291667
                                        , 0.95833333, 1, 1, 0.95833333, 0.95833333, 1, 0.95833333
                   ,'2002 WUTP02-R003 1', 0.68181818, 0.31818182, 0, 0, 0
                                        , 0, 1, 0, 0, 0
                                        , 0.28095238, 0.28095238, 0.56022727, 0.575, 0.575, 0.43571429, NA
                                        , 0.56190476, 1.13522727, 1.09659091, 1.67159091, 1.01071429, 1.64545455,2.63636364
                                        , 1, 1, 1, 1, 1, 1, 1
                   )
                ,ncol=32, byrow=TRUE
                ) #end of matrix() call
              ,stringsAsFactors=FALSE
              ) # end of data.frame() call
  names(texpected) <-c('UID','pcan_c', 'pcan_d', 'pcan_e', 'pcan_m', 'pcan_n'
                      ,'pmid_c', 'pmid_d', 'pmid_e', 'pmid_m', 'pmid_n'
                      ,'xcl', 'xcs', 'xmw', 'xmh', 'xgw', 'xgh', 'xgb'
                      ,'xc', 'xm', 'xcmw', 'xcm', 'xg', 'xcmgw', 'xcmg'
                      ,'xpcan', 'xpmid', 'xpgveg', 'xpmgw', 'xpcm', 'xpmg', 'xpcmg'
                      )

  expected <- dfLengthen(texpected, 'UID', 'METRIC', 'RESULT'
                        ,names(texpected)[!(names(texpected)=='UID')]
                        )
  return(expected)
}

# end of file