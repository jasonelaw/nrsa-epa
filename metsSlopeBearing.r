# metsSlopeBearing.r
#
# 02/01/10 cws created.
# 03/10/10 cws Updated as needed for change in nWadeableStationsPerTransect().
# 03/11/10 cws call to writeNRSACalcResults() corrected.
# 03/22/10 cws Moved unit test dataframes to separate functions
#  3/25/10 cws Changed diff() calls to dfCompare(), nlaLengthen() to dfLengthen().
# 04/09/10 ssr Modified unit test to include stream only and boatable only options.
#              Modified metsSlopeBearing.1 to allow stream or river only options.
#  4/22/10 cws cleaned up unit test and added case for site with slopes measured
#          as changes in elevation.  Updated calculations to handle these sites.
#  5/27/10 cws Updated code to use INCREMNT occuring only at A 0, reflecting
#              the current data organization.  Modified unit test accordingly.
#

require(RODBC)
require(RUnit)

metsSlopeBearing <- function()
# Calculates NRSA channel slope and bearing metrics:
#
# These metrics are saved to a csv file in the directory specified by
# NRSAMetricsLocation.
#
# Returns NULL on success or a character string describing the problem if one
# occurs.
#
# ARGUMENTS:
# none
{

  chan <- odbcConnect('NRSA2')

  thal <- fetchNRSATable(chan, 'tblTHALWEG2')
  if(is.character(thal)) return(thal)

  chanGeom <- fetchNRSATable(chan, 'tblCHANNELGEOMETRY2')
  if(is.character(chanGeom)) return(chanGeom)

  protocols <- siteProtocol(c(unique(chanGeom$UID), unique(thal$UID)))

  mets <- metsSlopeBearing.1(thal, chanGeom, protocols)
  if(is.character(mets)) return(mets)

  rc <- writeNRSACalcResults(mets, 'metsSlopeBearing.csv')
  return(rc)
}

metsSlopeBearing.1 <- function(thal, chanGeom, protocols)
# Does the work for for metsSlopeBearing
#
# ARGUMENTS:
# thal        dataframe with thalweg data table
# chanGeom    dataframe with channel geometry table
# protocols   dataframe with protocol (WADEABLE, BOATABLE) used for each UID
#
# rm(dists,sbWadeable,sbBoatable,sb,nsta,tt,sumDists,tsb,transpc,tranEast, tranNorth, tranSlope, tran,totEast, totNorth, fishDist, nEast, nNorth, reach, xslope, vslope, nslp, reach, xbearing, sinu, mets)
{
  intermediateMessage('Slope and bearing calculations', loc='start')

  # Get expected parameters from each dataframe for main channel only.  Calculate
  # transect spacing in wadeables (like ACTRANSP in boatable reaches) as the
  # expected number of stations in the transect times INCREMNT (the distance
  # between adjacent stations) in that transect.  Calculate backsighting
  # percentages for the boatable reaches based on the backsighted distances.
  # The boatable parameter ACTRANSP is ignored in favour of the distances over
  # which backsightings are made.
  dists <- subset(thal
                 ,PARAMETER=='INCREMNT' & TRANSECT=='A' & STATION==0
                  & UID %in% subset(protocols, PROTOCOL=='WADEABLE')$UID
                 ,select=c(UID,PARAMETER,RESULT)
                 )

  sbWadeable <- subset(chanGeom
                      ,PARAMETER %in% c('BEARING','BEARING2','BEARING3'
                                       ,'PROP','PROP2','PROP3'
                                       ,'SLOPE','SLOPE2','SLOPE3'
                                       )
                       & TRANSECT %in% LETTERS[1:11]
                       & UID %in% subset(protocols, PROTOCOL=='WADEABLE')$UID
                      ,select=c(UID,TRANSECT,PARAMETER,RESULT,UNITS)
                      )

  sbBoatable <- subset(chanGeom
                      ,PARAMETER %in% c('BEAR','SLOPE','DISTANCE')
                       & TRANSECT %in% LETTERS[1:11]
                       & UID %in% subset(protocols, PROTOCOL=='BOATABLE')$UID
                      ,select=c(UID,TRANSECT,LINE,PARAMETER,RESULT,UNITS)
                      )
  sbBoatable$LINE <- as.numeric(as.character(sbBoatable$LINE))
  sbBoatable$RESULT <- as.numeric(as.character(sbBoatable$RESULT))


  intermediateMessage('.1')

  #########################################################################
  # Organize for wadeable and boatable reaches into a single structure with the
  # following columns:
  # UID         Unique Identifier
  # TRANSECT    A-K
  # LINE        a numeric value = 0 for the main reading, 1, 2 or 3 thereafter.
  #               This value will be NA for TRANSPC, which is not associated
  #               with a specific line on the form.
  # PARAMETER   with values BEARING, SLOPE, PROPORTION, TRANSPC
  # RESULT      The numeric value of the measured parameter.
  # UNITS       The measurement units, CM or PERCENT for slopes, NONE for others.
  
  # Calculate transect spacing TRANSPC in wadeable reaches.  Include TRANSPC
  # as a parameter in the wadeable data. Fill in LINE and standardize PARAMETER
  # values.
  dists$RESULT <- as.numeric(as.character(dists$RESULT))
  newDists <- NULL
  if (nrow(dists)>0){
      # Calculate transect spacing for wadeable sites
      nsta <- nWadeableStationsPerTransect(thal)
#      newDists <- merge(subset(dists, STATION==0, select=-STATION)
#                       ,nsta
#                       ,by=c('UID','TRANSECT')
#                       )
      newDists <- merge(dists, nsta, by='UID', all.x=TRUE)

      newDists$TRANSPC <- as.character(newDists$nSta * as.numeric(newDists$RESULT))
      newDists <- newDists[c('UID','TRANSECT','TRANSPC')]
  }
  
  # Create LINE value based on PARAMETER, then standardize PARAMETER values
  sbWadeable$LINE <- ifelse(sbWadeable$PARAMETER %in% c('PROP','SLOPE','BEARING'), 0
                    ,ifelse(sbWadeable$PARAMETER %in% c('PROP2','SLOPE2','BEARING2'), 1
                    ,ifelse(sbWadeable$PARAMETER %in% c('PROP3','SLOPE3','BEARING3'), 2
                    ,NA
                    )))
  sbWadeable$PARAMETER <- ifelse(substr(sbWadeable$PARAMETER,1,4) == 'PROP', 'PROPORTION'
                         ,ifelse(substr(sbWadeable$PARAMETER,1,4) == 'SLOP', 'SLOPE'
                         ,ifelse(substr(sbWadeable$PARAMETER,1,4) == 'BEAR', 'BEARING'
                         ,NA
                         )))
  intermediateMessage('.2')
  

  # Calculate transect spacing TRANSPC from incremental DISTANCE values.
  # Calculate incremental proportion values from DISTANCE and TRANSPC.
  # Handle TRANSPC as a parameter in a separate dataframe.
  tt <- subset(sbBoatable, PARAMETER=='DISTANCE')

  sumDists <- NULL
  if (nrow(tt)>0){
      sumDists <- aggregate(list('transpc'=tt$RESULT)
                           ,list('UID'=tt$UID, 'TRANSECT'=tt$TRANSECT)
                           ,sum, na.rm=TRUE
                           )

      sbBoatable <- merge(sbBoatable, sumDists, c('UID', 'TRANSECT'))

      sbBoatable$RESULT <- ifelse(sbBoatable$PARAMETER=='DISTANCE'
                                 ,100 * sbBoatable$RESULT/sbBoatable$transpc
                                 ,sbBoatable$RESULT
                                 )
      sbBoatable$transpc <- NULL

      sumDists$TRANSPC <- as.character(sumDists$transpc)
      sumDists$transpc <- NULL

      sbBoatable$PARAMETER <- ifelse(sbBoatable$PARAMETER=='DISTANCE'
                                ,'PROPORTION'
                                ,sbBoatable$PARAMETER
                                )
      sbBoatable$PARAMETER <- ifelse(sbBoatable$PARAMETER=='BEAR'
                                    ,'BEARING'
                                    ,sbBoatable$PARAMETER
                                    )
      sbBoatable$LINE <- ifelse(is.na(sbBoatable$LINE), 0, sbBoatable$LINE - 1 )

      sumDists <- sumDists[c('UID','TRANSECT','TRANSPC')]
  }

  tsb <- rbind(sbWadeable[c('UID','TRANSECT','LINE','PARAMETER','RESULT','UNITS')]
              ,sbBoatable[c('UID','TRANSECT','LINE','PARAMETER','RESULT','UNITS')]
              )

  transpc <- rbind(newDists, sumDists)

  intermediateMessage('.3')

  # Transpose to wide format for intermediate calculations at each transect
  # Put TRANSPC values on every row, regardless of LINE value.
  # NOTE: Wadeable slopes (SLOPE_ND at least) have units = 'NONE', but are
  # expressed in percent, and this is reflected in the code below.
  sb <- reshape(tsb
               ,idvar=c('UID','TRANSECT','LINE')
               ,direction='wide'
               ,timevar='PARAMETER'
               )
  sb <- rename(sb, names(sb), sub('RESULT\\.(\\1)', '\\1', names(sb)))
  sb <- merge(sb, transpc, by=c('UID','TRANSECT'), all.x=TRUE)
  sb$BEARING <- as.numeric(sb$BEARING)
  sb$PROPORTION <- as.numeric(sb$PROPORTION)
  sb$TRANSPC <- as.numeric(sb$TRANSPC)
  sb$SLOPE <-  ifelse(sb$UNITS.SLOPE %in% c('PERCENT','NONE')
                     ,as.numeric(sb$SLOPE)
                     ,ifelse(sb$UNITS.SLOPE == 'CM'
                            ,100*as.numeric(sb$SLOPE)/(sb$TRANSPC * sb$PROPORTION)
                            ,NA
                            )
                     )
  sb$UNITS.SLOPE <- ifelse(sb$UNITS.SLOPE == 'CM', 'PERCENT', sb$UNITS.SLOPE)
  intermediateMessage('.4')

  #########################################################################
  # Intermediate calculations over single transect
  # tranEast   distance East traveled from this transect to the next
  #              = sum( sin(BEARINGInRadians) * DISTANCE )
  # tranNorth  distance North traveled from this transect to the next
  #              = sum( cos(BEARINGInRadians) * DISTANCE )
  # transpc    distance along channel from this transect to the next one
  # tranSlope  mean slope between this transect and the next one, weighted by
  #              the proportions of the total distance between adjacent
  #              transects over which each slope measurement was taken.
  #              Transects which have no slope data have tranSlope set to NA;
  #              otherwise the NAs would sum to 0, dangitall.
  sb$lineEast <- sin(sb$BEARING * 2*pi/360) * sb$TRANSPC * sb$PROPORTION/100
  sb$lineNorth <- cos(sb$BEARING * 2*pi/360) * sb$TRANSPC * sb$PROPORTION/100
  sb$lineSlope <- sb$SLOPE * sb$PROPORTION/100

  tranEast <- aggregate(list('tranEast'=sb$lineEast)
                       ,list('UID'=sb$UID, 'TRANSECT'=sb$TRANSECT)
                       ,sum, na.rm=TRUE
                       )
  tranNorth <- aggregate(list('tranNorth'=sb$lineNorth)
                        ,list('UID'=sb$UID, 'TRANSECT'=sb$TRANSECT)
                        ,sum, na.rm=TRUE
                        )
  tranSlope <- merge(aggregate(list('tranSlope'=sb$lineSlope)
                              ,list('UID'=sb$UID, 'TRANSECT'=sb$TRANSECT)
                              ,sum, na.rm=TRUE
                              )
                    ,aggregate(list('n'=sb$lineSlope)
                              ,list('UID'=sb$UID, 'TRANSECT'=sb$TRANSECT)
                              ,count
                              )
                    ,c('UID','TRANSECT')
                    )
  tranSlope$tranSlope <- ifelse(tranSlope$n==0, NA, tranSlope$tranSlope)
  tranSlope$n <- NULL

  transpc <- rename(subset(sb, LINE==0, select=c(UID, TRANSECT, TRANSPC))
                   ,'TRANSPC', 'transpc'
                   )
  intermediateMessage('.5')

  tran <- merge(merge(tranEast, tranNorth, c('UID','TRANSECT'), all=TRUE)
               ,merge(tranSlope, transpc, c('UID','TRANSECT'), all=TRUE)
               ,c('UID','TRANSECT'), all=TRUE
               )

   intermediateMessage('.6')

  #########################################################################
  # Intermediate calculations over entire reach
  # totEast   distance East travelled from start to end of reach
  #
  # totNorth  distance North travelled from start to end of reach
  # fishDist  distance along channel from start to end of reach
  #             = sum(transect spacing)
  #             = sum(actransp)
  # crowDist  straight line distance from start to end of reach
  #             = sqrt(totEast^2 + totNorth^2)
  # nEast     count of tranEast values used to remove meaningless values of
  #             fishDist and crowDist
  # nNorth    count of tranNorth values used to remove meaningless values of
  #             fishDist and crowDist
  totEast <- aggregate(list('totEast'=tran$tranEast)
                      ,list('UID'=tran$UID)
                      ,sum, na.rm=TRUE
                      )
  totNorth <- aggregate(list('totNorth'=tran$tranNorth)
                       ,list('UID'=tran$UID)
                       ,sum, na.rm=TRUE
                       )
  fishDist <- aggregate(list('fishDist'=tran$transpc)
                       ,list('UID'=tran$UID)
                       ,sum, na.rm=TRUE
                       )
  nEast <- aggregate(list('nEast'=tran$tranEast)
                    ,list('UID'=tran$UID)
                    ,count
                    )
  nNorth <- aggregate(list('nNorth'=tran$tranNorth)
                     ,list('UID'=tran$UID)
                     ,count
                     )
  intermediateMessage('.7')

  reach <- merge(totEast
                ,merge(totNorth, fishDist, 'UID', all=TRUE)
                ,'UID', all=TRUE
                )
  reach <- merge(reach
                ,merge(nEast, nNorth, 'UID', all=TRUE)
                ,'UID', all=TRUE
                )
  reach$crowDist <- ifelse(reach$nEast > 2 & reach$nNorth > 2
                          ,sqrt(reach$totEast^2 + reach$totNorth^2)
                          ,NA
                          )
  reach$fishDist <- ifelse(reach$nEast > 2 & reach$nNorth > 2
                          ,reach$fishDist
                          ,NA
                          )
  intermediateMessage('.8')

  #########################################################################
  # Metrics calculations
  # xslope    mean of tranSlope
  # vslope    std deviation of tranSlope
  # nslp      count of tranSlope
  # transpc   mean distance between transects
  # xbearing  overall bearing of reach based the inverse cosine of the ratio
  #             totNorth/crowDist
  # sinu      sinuosity as the ratio fishDist/crowDist
  nslp <- aggregate(list('RESULT'=tran$tranSlope)
                   ,list('UID'=tran$UID)
                   ,count
                   )
  nslp$METRIC <- 'nslp'

  xslope <- aggregate(list('RESULT'=tran$tranSlope)
                     ,list('UID'=tran$UID)
                     ,mean, na.rm=TRUE
                     )
  xslope$METRIC <- 'xslope'

  vslope <- aggregate(list('RESULT'=tran$tranSlope)
                     ,list('UID'=tran$UID)
                     ,sd, na.rm=TRUE
                     )
  vslope$METRIC <- 'vslope'

  transpc <- aggregate(list('RESULT'=tran$transpc)
                      ,list('UID'=tran$UID)
                      ,mean, na.rm=TRUE
                      )
  transpc$METRIC <- 'transpc'

  reach$RESULT <- ifelse(reach$totEast > 0
                          ,(360/(2*pi))*acos(reach$totNorth/reach$crowDist)
                          ,360 - (360/(2*pi))*acos(reach$totNorth/reach$crowDist)
                          )
  xbearing <- reach[c('UID','RESULT')]
  xbearing$METRIC <- 'xbearing'

  reach$RESULT <- ifelse(reach$crowDist == 0
                        ,NA
                        ,reach$fishDist / reach$crowDist
                        )
  sinu <- reach[c('UID','RESULT')]
  sinu$METRIC <- 'sinu'

  mets <- rbind(xslope, vslope, nslp, transpc, xbearing, sinu)
  intermediateMessage('.9')



  #########################################################################
  # Clean up and convert NaN values to NA
  mets <- mets[c('UID','METRIC','RESULT')]
  mets$RESULT <- as.character(mets$RESULT)

  mets$RESULT <- ifelse(is.nan(as.numeric(mets$RESULT)), NA, mets$RESULT)

  # Filter out calculated values with too small of a sample size to be reliable.
  badSlopeUIDs <- unique(nslp[as.numeric(nslp$RESULT)<=2,]$UID)
  if(length(badSlopeUIDs)>0) {
      mets$RESULT <- ifelse(mets$METRIC=='xslope' & mets$UID %in% badSlopeUIDs
                           ,NA
                           ,mets$RESULT
                           )
      mets$RESULT <- ifelse(mets$METRIC=='vslope' & mets$UID %in% badSlopeUIDs
                           ,NA
                           ,mets$RESULT
                           )
  }

  intermediateMessage('.  Done.', loc='end')
  return(mets)
}


metsSlopeBearingTest <- function()
# Unit test for metsSlopeBearing. Test data taken from WEMAP data on 2-Feb-2010,
# and is then transformed into the expected organization for NRSA data.
# 2000 WAZP99-0505 1               Stream with no supplemental readings
# 2000 WAZP99-0569 1               Stream with many supplemental readings
# 2003 WWYP99-0659 1               Stream with slopes in cm
# 2000 WAZP99-0569 1 no incremnt   Stream with no incremnt information
# 2000 WAZP99-0569 1 no slopes     Stream with no slope information
# 2000 WAZP99-0569 1 only 2 slopes Stream with insufficient slope information
# 2000 WIDP99-0556 1               River with some supplemental readings
# 2000 WSDP99-0531 1               River with lots of supplemental readings
#
# The expected metrics are obtained for these reaches as well, and modified as
# follows:
#   '2000 WAZP99-0569 1' xbearing changed from 42.435 to 42.43476
#   '2000 WAZP99-0569 1 only 2 slopes' xbearing changed from 42.435 to 42.43476
#   '2000 WAZP99-0569 1 no slopes' xbearing changed from 42.435 to 42.43476
#   '2000 WAZP99-0569 1 no incremnt' xbearing changed from 42.435 to NA and
#       sinu changed from 1.1251 to NA since lack of distance between stations
#       means the distances on which these metrics rely on are unavailable.
#   '2003 WWYP99-0659 1' xslope changed from 0.86733 to 1.026666666 and vslope
#       changed from 0.73286 to 0.6480359691 to account for error in previous
#       calculations of slopes at transects with supplemental slopes measured as
#       elevation change.  These should be calculated as elev/(transpc * prop)
#       but instead were calculated as (elev/(transpc * prop))*prop, which were
#       then multiplied by the proportion again and summed to determine the
#       slope of the transect.  This lead to diminished mean slopes at each
#       transect.
#
{
  # Create fake input data.  Thalweg data initially has 1 incremnt at
  # each station, instead of once per site.
  ll <- metsSlopeBearing.makeTestData()
  fakeThal_IncremntsAtEachUID <- ll[[1]]
  fakeThal <- subset(fakeThal_IncremntsAtEachUID
                    ,!(PARAMETER=='INCREMNT' & TRANSECT !='A' & STATION !=0)
                    )
#  fakeThal <- fakeThal_IncremntsAtEachUID
  fakeChanGeom <- ll[[2]]

  fakeProtocol <- metsSlopeBearing.makeProtocols()
  # thal <- fakeThal; chanGeom <- fakeChanGeom; protocols <- fakeProtocol
  # Create expected results
  expected <- metsSlopeBearing.makeExpectedResults()


  # Calculate actual results using thalweg data with extra incremnt values.
  intermediateMessage('.  Testing streams and rivers data, thalweg table has extra incremnt data.', loc='end')
  results <- metsSlopeBearing.1(fakeThal_IncremntsAtEachUID, fakeChanGeom, fakeProtocol)

  # Compare expected and actual results
  expected <- expected[order(expected$UID, expected$METRIC),]
  results <- results[order(results$UID, results$METRIC),]
  expected$RESULT <- as.numeric(expected$RESULT)
  results$RESULT <- as.numeric(results$RESULT)
  errs <- dfCompare(expected, results, c('UID','METRIC'), zeroFudge=1e-3)
#  return(errs)
  checkEquals(NULL, errs
             ,"Error: Slope and bearing metrics are broken with both protocols and extra incremnt values in thalweg table"
             )


  # Compare expected and actual results
  intermediateMessage('.  Testing streams and rivers data, thalweg table normal.', loc='end')
  results <- metsSlopeBearing.1(fakeThal, fakeChanGeom, fakeProtocol)

  expected <- expected[order(expected$UID, expected$METRIC),]
  results <- results[order(results$UID, results$METRIC),]
  expected$RESULT <- as.numeric(expected$RESULT)
  results$RESULT <- as.numeric(results$RESULT)
  errs <- dfCompare(expected, results, c('UID','METRIC'), zeroFudge=1e-3)
#  return(errs)
  checkEquals(NULL, errs
             ,"Error: Slope and bearing metrics are broken with both protocols using normal thalweg data"
             )
             
  # Compare expected and actual results
#  expected <- expected[order(expected$UID, expected$METRIC),]
#  results <- results[order(results$UID, results$METRIC),]
#  expected$RESULT <- as.numeric(expected$RESULT)
#  results$RESULT <- as.numeric(results$RESULT)
##  Testing for only streams
  fakeProtocol.s <- subset(fakeProtocol, PROTOCOL=='WADEABLE')
  fakeThal.s <- subset(fakeThal, UID %in% fakeProtocol.s$UID)
  fakeChanGeom.s <- subset(fakeChanGeom, UID %in% fakeProtocol.s$UID)
  expected.s <- subset(expected, UID %in% fakeProtocol.s$UID)

  # Calculate actual results
  intermediateMessage('.  Testing streams data.', loc='end')
  results.s <- metsSlopeBearing.1(fakeThal.s, fakeChanGeom.s, fakeProtocol.s)
  results.s$RESULT <- as.numeric(results.s$RESULT)
  errs.s <- dfCompare(expected.s, results.s, c('UID','METRIC'), zeroFudge=1e-3)
#  return(errs.s)
  checkEquals(NULL, errs.s
             ,"Error: Slope and bearing metrics are broken with wadeable data"
             )

  fakeProtocol.r <- subset(fakeProtocol, PROTOCOL=='BOATABLE')
  fakeThal.r <- subset(fakeThal, UID %in% fakeProtocol.r$UID)
  fakeChanGeom.r <- subset(fakeChanGeom, UID %in% fakeProtocol.r$UID)
  expected.r <- subset(expected, UID %in% fakeProtocol.r$UID)

  # Calculate actual results
  intermediateMessage('.  Testing rivers data.', loc='end')
  results.r <- metsSlopeBearing.1(fakeThal.r, fakeChanGeom.r, fakeProtocol.r)
  results.r$RESULT <- as.numeric(results.r$RESULT)

  errs.r <- dfCompare(expected.r, results.r, c('UID','METRIC'), zeroFudge=1e-3)
#  return(errs.r)
  checkEquals(NULL, errs.r
             ,"Error: Slope and bearing metrics are broken with boatable data"
             )


}

metsSlopeBearing.makeTestData <- function()
# Creates thalweg and channel geometry data used in testing metsSlopeBearing().
# Returns a list of dataframes, element 1 is thalweg, element 2 is chanGeom.
#
# Test data taken from WEMAP data on 2-Feb-2010, and is then transformed into
# the expected organization for NRSA data.
# 2000 WAZP99-0505 1               Stream with no supplemental readings
# 2000 WAZP99-0569 1               Stream with many supplemental readings
# 2003 WWYP99-0659 1               Stream with slopes taken in cm
# 2000 WAZP99-0569 1 no incremnt   Stream with no incremnt information
# 2000 WAZP99-0569 1 no slopes     Stream with no slope information
# 2000 WAZP99-0569 1 only 2 slopes Stream with insufficient slope information
# 2000 WIDP99-0556 1               River with some supplemental readings
# 2000 WSDP99-0531 1               River with lots of supplemental readings
#
{
#234567890123456789012345678901234567890
  wemapThal <- data.frame(matrix(
                  c('2000 WAZP99-0505 1', 'A', 1.5, 7, 354, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0505 1', 'B', 1.5, 6, 357, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0505 1', 'C', 1.5, 4, 11, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0505 1', 'D', 1.5, 7.5, 3, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0505 1', 'E', 1.5, 12.5, 9, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0505 1', 'F', 1.5, 5, 17, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0505 1', 'G', 1.5, 3.5, 5, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0505 1', 'H', 1.5, 1.5, 57, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0505 1', 'I', 1.5, 4, 23, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0505 1', 'J', 1.5, 6.5, 53, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'

                   ,'2000 WAZP99-0569 1', 'A', 1.5, 6, 50, 33
                   ,10, 58, 34, 12, 0, 33, 'PERCENT'
                   ,'2000 WAZP99-0569 1', 'B', 1.5, 10, 40, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1', 'C', 1.5, 12, 4, 25
                   ,12, 39, 75, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1', 'D', 1.5, 11, 25, 80
                   ,13, 53, 20, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1', 'E', 1.5, 22, 65, 75
                   ,8, 19, 25, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1', 'F', 1.5, 8, 37, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1', 'G', 1.5, 12, 10, 80
                   ,10, 73, 20, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1', 'H', 1.5, 6, 107, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1', 'I', 1.5, 12.2, 76, 30
                   ,14, 45, 70, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1', 'J', 1.5, 8, 23, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'

                   ,'2003 WWYP99-0659 1', 'A', 1.5, 36, 161, 100
                   ,NA,  NA, NA, NA,  NA, NA, 'CM'
                   ,'2003 WWYP99-0659 1', 'B', 1.5, 12, 110, 100
                   ,NA,  NA, NA, NA,  NA, NA, 'CM'
                   ,'2003 WWYP99-0659 1', 'C', 1.5, 12, 193,  20
                   ,4,  75, 30,  0, 124, 50, 'CM'
                   ,'2003 WWYP99-0659 1', 'D', 1.5, 26, 230, 100
                   ,NA,  NA, NA, NA,  NA, NA, 'CM'
                   ,'2003 WWYP99-0659 1', 'E', 1.5,  8, 193, 100
                   ,NA,  NA, NA, NA,  NA, NA, 'CM'
                   ,'2003 WWYP99-0659 1', 'F', 1.5, 18, 120, 100
                   ,NA,  NA, NA, NA,  NA, NA, 'CM'
                   ,'2003 WWYP99-0659 1', 'G', 1.5,  9, 210,  50
                   ,2, 108, 50, NA,  NA, NA, 'CM'
                   ,'2003 WWYP99-0659 1', 'H', 1.5, 14, 246, 100
                   ,NA,  NA, NA, NA,  NA, NA, 'CM'
                   ,'2003 WWYP99-0659 1', 'I', 1.5,  2, 157,  50
                   ,10, 238, 50, NA,  NA, NA, 'CM'
                   ,'2003 WWYP99-0659 1', 'J', 1.5,  1, 100, 100
                   ,NA,  NA, NA, NA,  NA, NA, 'CM'

                   ,'2000 WAZP99-0569 1 no incremnt', 'A', NA, 6, 50, 33
                   ,10, 58, 34, 12, 0, 33, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no incremnt', 'B', NA, 10, 40, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no incremnt', 'C', NA, 12, 4, 25
                   ,12, 39, 75, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no incremnt', 'D', NA, 11, 25, 80
                   ,13, 53, 20, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no incremnt', 'E', NA, 22, 65, 75
                   ,8, 19, 25, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no incremnt', 'F', NA, 8, 37, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no incremnt', 'G', NA, 12, 10, 80
                   ,10, 73, 20, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no incremnt', 'H', NA, 6, 107, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no incremnt', 'I', NA, 12.2, 76, 30
                   ,14, 45, 70, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no incremnt', 'J', NA, 8, 23, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'

                   ,'2000 WAZP99-0569 1 no slopes', 'A', 1.5, NA, 50, 33
                   ,NA, 58, 34, NA, 0, 33, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no slopes', 'B', 1.5, NA, 40, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no slopes', 'C', 1.5, NA, 4, 25
                   ,NA, 39, 75, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no slopes', 'D', 1.5, NA, 25, 80
                   ,NA, 53, 20, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no slopes', 'E', 1.5, NA, 65, 75
                   ,NA, 19, 25, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no slopes', 'F', 1.5, NA, 37, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no slopes', 'G', 1.5, NA, 10, 80
                   ,NA, 73, 20, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no slopes', 'H', 1.5, NA, 107, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no slopes', 'I', 1.5, NA, 76, 30
                   ,NA, 45, 70, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no slopes', 'J', 1.5, NA, 23, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'

                   ,'2000 WAZP99-0569 1 only 2 slopes', 'A', 1.5, 6, 50, 33
                   ,NA, 58, 34, NA, 0, 33, 'PERCENT'
                   ,'2000 WAZP99-0569 1 only 2 slopes', 'B', 1.5, 10, 40, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 only 2 slopes', 'C', 1.5, NA, 4, 25
                   ,NA, 39, 75, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 only 2 slopes', 'D', 1.5, NA, 25, 80
                   ,NA, 53, 20, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 only 2 slopes', 'E', 1.5, NA, 65, 75
                   ,NA, 19, 25, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 only 2 slopes', 'F', 1.5, NA, 37, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 only 2 slopes', 'G', 1.5, NA, 10, 80
                   ,NA, 73, 20, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 only 2 slopes', 'H', 1.5, NA, 107, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 only 2 slopes', 'I', 1.5, NA, 76, 30
                   ,NA, 45, 70, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 only 2 slopes', 'J', 1.5, NA, 23, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'

                   ,'2000 WIDP99-0556 1', 'A', 30, 0.2, 150, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WIDP99-0556 1', 'B', 30, 0.2, 50, 40
                   ,0.2, 140, 60, NA, NA, NA , 'PERCENT'
                   ,'2000 WIDP99-0556 1', 'C', 33.3333333, 0.2, 40, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WIDP99-0556 1', 'D', 30, 0.2, 50, 28.5714286
                   ,0.2, 40, 28.5714286, 0.2, 20, 42.8571429, 'PERCENT'
                   ,'2000 WIDP99-0556 1', 'E', 30, 0.2, 70, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WIDP99-0556 1', 'F', 30, 0.2, 60, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WIDP99-0556 1', 'G', 30, 0.1, 60, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WIDP99-0556 1', 'H', 30, 0.2, 50, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WIDP99-0556 1', 'I', 30, 0.1, 40, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WIDP99-0556 1', 'J', 30, 0.2, 30, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'

                   ,'2000 WSDP99-0531 1', 'A', 20, 0.1, 80, 45.7142857
                   ,0.1, 340, 40, 0.1, 20, 14.2857143, 'PERCENT'
                   ,'2000 WSDP99-0531 1', 'B', 20, 0.1, 50, 50
                   ,0.1, 10, 25, 0.1, 100, 25, 'PERCENT'
                   ,'2000 WSDP99-0531 1', 'C', 20, 0.1, 360, 22.5
                   ,0.1, 350, 42.5, 0.1, 60, 35, 'PERCENT'
                   ,'2000 WSDP99-0531 1', 'D', 20, 0.1, 40, 25
                   ,0.1, 40, 12.5, 0.1, 80, 62.5, 'PERCENT'
                   ,'2000 WSDP99-0531 1', 'E', 20, 0.1, 330, 12.5
                   ,0.1, 20, 87.5, NA, NA, NA, 'PERCENT'
                   ,'2000 WSDP99-0531 1', 'F', 20, 0.1, 120, 15
                   ,0.1, 330, 22.5, 0.1, 80, 62.5, 'PERCENT'
                   ,'2000 WSDP99-0531 1', 'G', 20, 0.1, 50, 75
                   ,0.1, 140, 25, NA, NA, NA, 'PERCENT'
                   ,'2000 WSDP99-0531 1', 'H', 20, 0.1, 90, 40.7407407
                   ,0.1, 340, 59.2592593, NA, NA, NA, 'PERCENT'
                   ,'2000 WSDP99-0531 1', 'I', 20, 0.1, 200, 30
                   ,0.1, 200, 32.5, 0.1, 220, 37.5, 'PERCENT'
                   ,'2000 WSDP99-0531 1', 'J', 20, 0.1, 180, 12.5
                   ,0.1, 160, 52.5, 0.1, 240, 35, 'PERCENT'
                   )
                   ,ncol=13, byrow=TRUE
                 ) # end of matrix() call
                 ,stringsAsFactors=FALSE
             ) # end of data.frame() call

  names(wemapThal) <- c('UID','TRANSECT','incremnt', 'slopet','beart','proportt'
                        ,'slope1','bear1','proport1','slope2','bear2','proport2'
                        ,'units'
                        )

  wemapRiverStations <- data.frame('UID'=c(rep('2000 WIDP99-0556 1', 10)
                                          ,rep('2000 WSDP99-0531 1', 10)
                                          )
                                  ,'TRANSECT'=rep(LETTERS[1:10], 2)
                                  ,'nsta'=c(20,20,18,20,20, 20,20,20,20,20
                                           ,20,20,20,20,20, 20,20,20,20,20
                                           )
                                  )

  # Create fakeThal from transposed wemap thalweg data.  This holds the wadeable
  # reach incremnt values; everything else is in the channel geometry.  Transect
  # C of WAZP99-0505 ended at station 8, so that is removed explicitly.  The
  # UNITS information is handled separately.
  # To allow proper counting of stations per transect during metrics calculation,
  # DEPTH is added at each station, though these values will not be used.
  units <- rename(wemapThal[c('UID','TRANSECT','units')], 'units', 'UNITS')
  
  ss <- subset(wemapThal, select=-units)
  tt <- dfLengthen(ss
                  ,c('UID','TRANSECT')
                  ,'PARAMETER'
                  ,'RESULT'
                  ,names(ss)[!(names(ss) %in%
                                       c('UID','TRANSECT')
                                     )]
                        )

  fakeThal <- subset(tt
                    ,UID %in% c('2000 WAZP99-0505 1','2000 WAZP99-0569 1'
                               ,'2003 WWYP99-0659 1'
                               ,'2000 WAZP99-0569 1 no incremnt'
                               ,'2000 WAZP99-0569 1 no slopes'
                               ,'2000 WAZP99-0569 1 only 2 slopes'
                               ) &
                     PARAMETER == 'incremnt'
                    )
  fakeThal <- merge(fakeThal, list('STATION'=0:9), all=TRUE)
  fakeThal <- subset(fakeThal
                    ,!(UID!='2000 WAZP99-0505 1' & TRANSECT=='C' & STATION=='9')
                    )
  fakeThal$PARAMETER <- 'INCREMNT'
  fakeThal$SAMPLE_TYPE <- 'PHAB_THALW'
  fakeThal$FLAG <- NA
  fakeThal$UNITS <- 'M'

  addDepths<-fakeThal
  addDepths$PARAMETER='DEPTH'
  addDepths$UNITS<-'CM'
  fakeThal<-rbind(fakeThal,addDepths)

  # Create fakeChanGeom from transposed wemap thalweg data.  River and stream
  # data are slightly different, and thus are assembled separately.  Rows with
  # missing values are removed.
  wadeable <- subset(tt
                    ,UID %in% c('2000 WAZP99-0505 1','2000 WAZP99-0569 1'
                               ,'2003 WWYP99-0659 1'
                               ,'2000 WAZP99-0569 1 no incremnt'
                               ,'2000 WAZP99-0569 1 no slopes'
                               ,'2000 WAZP99-0569 1 only 2 slopes'
                               )
                     & PARAMETER != 'incremnt'
                    )
  wadeable$LINE <- NA
  wadeable$PARAMETER <- ifelse(wadeable$PARAMETER == 'slopet', 'SLOPE'
                       ,ifelse(wadeable$PARAMETER == 'beart', 'BEARING'
                       ,ifelse(wadeable$PARAMETER == 'proportt', 'PROP'
                       ,ifelse(wadeable$PARAMETER == 'slope1', 'SLOPE2'
                       ,ifelse(wadeable$PARAMETER == 'bear1', 'BEARING2'
                       ,ifelse(wadeable$PARAMETER == 'proport1', 'PROP2'
                       ,ifelse(wadeable$PARAMETER == 'slope2', 'SLOPE3'
                       ,ifelse(wadeable$PARAMETER == 'bear2', 'BEARING3'
                       ,ifelse(wadeable$PARAMETER == 'proport2', 'PROP3', NA
                       )))))))))
  wadeable <- merge(wadeable, units, by=c('UID','TRANSECT'))
  wadeable$UNITS <- ifelse(wadeable$PARAMETER %in% c('SLOPE','SLOPE2','SLOPE3')
                          ,wadeable$UNITS
                          ,'NONE'
                          )

  bb <- subset(wemapThal
              ,!(UID %in% c('2000 WAZP99-0505 1','2000 WAZP99-0569 1'
                           ,'2003 WWYP99-0659 1'
                           ,'2000 WAZP99-0569 1 no incremnt'
                           ,'2000 WAZP99-0569 1 no slopes'
                           ,'2000 WAZP99-0569 1 only 2 slopes'
                           )
                )
              )
  bb <- merge(bb, wemapRiverStations, by=c('UID','TRANSECT'), all.x=TRUE)
  bb$distancet <- (as.numeric(bb$proportt)/100) * as.numeric(bb$incremnt) * bb$nsta
  bb$distance1 <- (as.numeric(bb$proport1)/100) * as.numeric(bb$incremnt) * bb$nsta
  bb$distance2 <- (as.numeric(bb$proport2)/100) * as.numeric(bb$incremnt) * bb$nsta
  bb$incremnt <- NULL
  bb$proportt <- NULL
  bb$proport1 <- NULL
  bb$proport2 <- NULL
  bb$nsta <- NULL

  boatable <- dfLengthen(bb, c('UID','TRANSECT'), 'PARAMETER', 'RESULT'
                        ,names(bb)[!(names(bb) %in% c('UID','TRANSECT'))]
                        )

  boatable$LINE <- ifelse(boatable$PARAMETER %in% c('slopet','beart','distancet'), 1
                  ,ifelse(boatable$PARAMETER %in% c('slope1','bear1','distance1'), 2
                  ,ifelse(boatable$PARAMETER %in% c('slope2','bear2','distance2'), 3, NA
                  )))

  boatable$PARAMETER <-
        ifelse(boatable$PARAMETER %in% c('slopet','slope1','slope2'), 'SLOPE'
       ,ifelse(boatable$PARAMETER %in% c('beart','bear1','bear2'), 'BEAR'
       ,ifelse(boatable$PARAMETER %in% c('distancet','distance1','distance2'), 'DISTANCE', NA
       )))
   boatable$UNITS <- 'NONE'

  fakeChanGeom <- subset(rbind(boatable, wadeable), !is.na(RESULT))
  fakeChanGeom$TRANLINE <- 'NONE'
  fakeChanGeom$BANK <- 'NONE'
  fakeChanGeom$SAMPLE_TYPE <- ifelse(fakeChanGeom$UID %in%
                                       c('2000 WAZP99-0505 1'
                                        ,'2000 WAZP99-0569 1'
                                        ,'2003 WWYP99-0659 1'
                                        ,'2000 WAZP99-0569 1 no incremnt'
                                        ,'2000 WAZP99-0569 1 no slopes'
                                        ,'2000 WAZP99-0569 1 only 2 slopes'
                                        )
                                    ,'PHAB_SLOPE'
                                    ,'PHAB_CHANBFRONT'
                                    )
  fakeChanGeom$FLAG <- as.character(NA)

  return(list(fakeThal, fakeChanGeom))
}


metsSlopeBearing.makeProtocols <- function()
# Create dataframe of protocol information for unit test
{
  return(data.frame('UID'=c('2000 WAZP99-0505 1'
                           ,'2000 WAZP99-0569 1'
                           ,'2003 WWYP99-0659 1'
                           ,'2000 WAZP99-0569 1 no incremnt'
                           ,'2000 WAZP99-0569 1 no slopes'
                           ,'2000 WAZP99-0569 1 only 2 slopes'
                           ,'2000 WIDP99-0556 1'
                           ,'2000 WSDP99-0531 1'
                           )
                   ,'PROTOCOL'=c('WADEABLE','WADEABLE','WADEABLE','WADEABLE'
                                ,'WADEABLE','WADEABLE','BOATABLE'
                                ,'BOATABLE'
                                )
                   ,stringsAsFactors=FALSE
                   )
        )
}


metsSlopeBearing.makeExpectedResults <- function()
# Create dataframe of calculation results for unit test
{
  expected <- rbind(data.frame('UID'='2000 WAZP99-0505 1'
                              ,'METRIC'=c('xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu'
                                         )
                              ,'RESULT'=c('5.75','2.9930','10','15.0'
                                         ,'16.442','1.0680'
                                         )
                              )
                   ,data.frame('UID'='2000 WAZP99-0569 1'
                              ,'METRIC'=c('xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu'
                                         )
                              ,'RESULT'=c('10.8300','3.5006','10','15.000'
                                         ,'42.43476','1.1251'
                                         )
                              )
                   ,data.frame('UID'='2003 WWYP99-0659 1'
                              ,'METRIC'=c('xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu'
                                         )
                              ,'RESULT'=c('1.026666666','0.6480359691','10','15.000'
                                         ,'161.842','1.66858'
                                         )
                              )
                   ,data.frame('UID'='2000 WAZP99-0569 1 no incremnt'
                              ,'METRIC'=c('xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu'
                                         )
                              ,'RESULT'=c('10.8300','3.5006','10',NA
                                         ,NA,NA
                                         )
                              )
                   ,data.frame('UID'='2000 WAZP99-0569 1 no slopes'
                              ,'METRIC'=c('xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu'
                                         )
                              ,'RESULT'=c(NA,NA,'0','15.000'
                                         ,'42.43476','1.1251'
                                         )
                              )
                   ,data.frame('UID'='2000 WAZP99-0569 1 only 2 slopes'
                              ,'METRIC'=c('xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu'
                                         )
                              ,'RESULT'=c(NA,NA,'2','15.000'
                                         ,'42.43476','1.1251'
                                         )
                              )
                   ,data.frame('UID'='2000 WIDP99-0556 1'
                              ,'METRIC'=c('xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu'
                                         )
                              ,'RESULT'=c('0.18000','0.04216','10','600.00'
                                         ,'59.395','1.23583'
                                         )
                              )
                   ,data.frame('UID'='2000 WSDP99-0531 1'
                              ,'METRIC'=c('xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu'
                                         )
                              ,'RESULT'=c('0.10000','0.00000','10','400.00'
                                         ,'51.499','2.33470'
                                         )
                              )
                   )
  expected$UID <- as.character(expected$UID)
  expected$METRIC <- as.character(expected$METRIC)
  expected$RESULT <- as.character(expected$RESULT)

  return(expected)
}


# end of file