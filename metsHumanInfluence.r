## metsHumanInfluence.r
##
## Created SSR 02/10/2010
## 02/19/2010 removed source() call
## 03/23/1010 cws moved creation of test dataframes to separate functions.
#  3/25/10 cws Changed diff() calls to dfCompare().
## 03/25/10 ssr changed metrics names to lowercase.

metsHumanInfluence <- function()
## Creating human influence metrics:
## Creating human influence metrics:
## sdb_hall	 Std dev human disturbance on bank
## sdcb_hall	 Std dev human dist. on bank or channel
## sdc_hall	 Std dev human disturbance in channel
## sdwcb_hall	 Std dev wted human dist on bank or chan
## w1h_bldg	 Rip Dist--Buildings (ProxWt Pres)
## w1h_crop	 Rip Dist--Row Crop (ProxWt Pres)
## w1h_ldfl	 Rip Dist--Trash/Landfill (ProxWt Pres)
## w1h_log	 Rip Dist--Logging Activity (ProxWt Pres)
## w1h_mine	 Rip Dist--Mining Activity (ProxWt Pres)
## w1h_park	 Rip Dist--Lawn/Park (ProxWt Pres)
## w1h_pipe	 Rip Dist--Pipes infl/effl (ProxWt Pres)
## w1h_pstr	 Rip Dist--Pasture/Hayfield (ProxWt Pres)
## w1h_pvmt	 Rip Dist--Pavement (ProxWt Pres)
## w1h_road	 Rip Dist--Road/Railroad (ProxWt Pres)
## w1h_wall	 Rip Dist--Wall/Bank Revet. (ProxWt Pres)
## w1_hag	 Rip Dist--Sum Agric Types (ProxWt Pres)
## w1_hall	 Rip Dist--Sum All Types (ProxWt Pres)
## w1_hnoag	 Rip Dist--Sum NonAg Types (ProxWt Pres)
## xb_hag	 Rip Dist-Sum Ag Types instrm & in plot
## xb_hall	 Rip Dist--Sum All Types instrm & on bank
## xb_hnoag	 Rip Dist Sum-Non ag Types instrm & Plot
## xcb_hag	 Rip Dist Sum-Ag Types instrm & on Bank
## xcb_hall	 Rip Dist--Sum All Types instrm & in plot
## xcb_hnag	 Rip Dist Sum-Non Ag Types instrm & Bank
## xc_hag	 Rip Dist-Sum of Ag Types in Ripar Plot
## xc_hall	 Rip Dist--Sum All Types in Ripar Plots
## xc_hnoag	 Rip Dist Sum-Non Ag Types in Ripar Plot
## xf_hag	 Rip Dist Sum-Ag Types Beyond Ripar Plot
## xf_hall	 Rip Dist--Sum All Types beyond Rip Plots
## xf_hnoag	 Rip Dist Sum-Non Ag Types Beyond Rip Plt
## x_hag	 Rip Dist Sum-Ag Types rip Plt & Beyond
## x_hall	 Rip Dist--Sum All Types str plt & beyond
## x_hnoag	 Rip Dist Sum-Non Ag rip Plt & Beyond

{
intermediateMessage('Human influence metrics calculations', loc='start')
intermediateMessage('.1 Read in data', loc='end')

##  Reading in data from INVASIVES2
chan <- odbcConnect('NRSA2')
tblVR <- fetchNRSATable(chan,'tblVISRIP2')
close(chan)
df <- subset (tblVR, PARAMETER %in% c('BUILD', 'LANDFL', 'LOG', 'MINE', 'PARK',
              'PAST', 'PAVE', 'PIPES', 'ROAD', 'ROW', 'WALL'),
              select=c('UID','TRANSECT','TRANSDIR','PARAMETER','RESULT'))

## Calculate the metrics
intermediateMessage('.2 call function metsHumanInfluence.1', loc='end')
mets <- metsHumanInfluence.1(df)

## Write the results to metsHumanInfluence.csv
intermediateMessage('.3 Write results', loc='end')
rc <- writeNRSACalcResults(mets, 'metsHumanInfluence.csv')
        
intermediateMessage('  Done.', loc='end')
return(rc)

}


metsHumanInfluence.1 <- function(df)
{

df2 <- df
df2$var_0 <- ifelse(df$RESULT=='0', 1, 0)
df2$var_P <- ifelse(df$RESULT=='P', 1, 0)
df2$var_C <- ifelse(df$RESULT=='C', 1, 0)
df2$var_B <- ifelse(df$RESULT=='B', 1, 0)
df2$var_CB <- df2$var_C+ df2$var_B
           
##  Calculating meanAtTransect
meanb_hall <- aggregate(list(xb_hall=df2$var_B), list(UID=df2$UID
                        , PARAMETER=df2$PARAMETER), mean, na.rm=T)
meanc_hall <- aggregate(list(xc_hall=df2$var_C), list(UID=df2$UID
                        , PARAMETER=df2$PARAMETER), mean, na.rm=T) 
meancb_hall <- aggregate(list(xcb_hall=df2$var_CB), list(UID=df2$UID
                        , PARAMETER=df2$PARAMETER), mean, na.rm=T)
meanf_hall <- aggregate(list(xf_hall=df2$var_P), list(UID=df2$UID
                        , PARAMETER=df2$PARAMETER), mean, na.rm=T) 

## sum(meanAtTransect)
xb_hall <- aggregate(list(xb_hall=meanb_hall$xb_hall)
                   , list(UID=meanb_hall$UID), sum, na.rm=T)
xc_hall <- aggregate(list(xc_hall=meanc_hall$xc_hall)
                   , list(UID=meanc_hall$UID), sum, na.rm=T) 
xcb_hall <- aggregate(list(xcb_hall=meancb_hall$xcb_hall)
                   , list(UID=meancb_hall$UID), sum, na.rm=T)
xf_hall <- aggregate(list(xf_hall=meanf_hall$xf_hall)
                   , list(UID=meanf_hall$UID), sum, na.rm=T) 

##  Creating ag (agriculture parameters) and noag (non-agriculture parameters)
ag <- subset(df2, (PARAMETER %in% c('ROW', 'PAST')))
noag <- subset(df2, !(PARAMETER %in% c('ROW', 'PAST')))

##  Calculating meanAtTransect for agriculture parameters
meanb_hag <- aggregate(list(xb_hag=ag$var_B), list(UID=ag$UID
                      , PARAMETER=ag$PARAMETER), mean, na.rm=T)
meanc_hag <- aggregate(list(xc_hag=ag$var_C), list(UID=ag$UID
                      , PARAMETER=ag$PARAMETER), mean, na.rm=T) 
meancb_hag <- aggregate(list(xcb_hag=ag$var_CB), list(UID=ag$UID
                      , PARAMETER=ag$PARAMETER), mean, na.rm=T)
meanf_hag <- aggregate(list(xf_hag=ag$var_P), list(UID=ag$UID
                      , PARAMETER=ag$PARAMETER), mean, na.rm=T) 

## sum(meanAtTransect) for agriculture parameters
xb_hag <- aggregate(list(xb_hag=meanb_hag$xb_hag), list(UID=meanb_hag$UID)
                   , sum, na.rm=T)
xc_hag <- aggregate(list(xc_hag=meanc_hag$xc_hag), list(UID=meanc_hag$UID)
                   , sum, na.rm=T) 
xcb_hag <- aggregate(list(xcb_hag=meancb_hag$xcb_hag), list(UID=meancb_hag$UID)
                   , sum, na.rm=T)
xf_hag <- aggregate(list(xf_hag=meanf_hag$xf_hag), list(UID=meanf_hag$UID)
                   , sum, na.rm=T) 

##  Calculating meanAtTransect for non-agriculture parameters
meanb_hnoag <- aggregate(list(xb_hnoag=noag$var_B), list(UID=noag$UID
                        , PARAMETER=noag$PARAMETER), mean, na.rm=T)
meanc_hnoag <- aggregate(list(xc_hnoag=noag$var_C), list(UID=noag$UID
                        , PARAMETER=noag$PARAMETER), mean, na.rm=T) 
meancb_hnoag <- aggregate(list(xcb_hnag=noag$var_CB), list(UID=noag$UID
                        , PARAMETER=noag$PARAMETER), mean, na.rm=T)
meanf_hnoag <- aggregate(list(xf_hnoag=noag$var_P), list(UID=noag$UID
                        , PARAMETER=noag$PARAMETER), mean, na.rm=T) 

## sum(meanAtTransect) for non-agriculture parameters
xb_hnoag <- aggregate(list(xb_hnoag=meanb_hnoag$xb_hnoag)
                    , list(UID=meanb_hnoag$UID), sum, na.rm=T)
xc_hnoag <- aggregate(list(xc_hnoag=meanc_hnoag$xc_hnoag)
                    , list(UID=meanc_hnoag$UID), sum, na.rm=T) 
xcb_hnoag <- aggregate(list(xcb_hnag=meancb_hnoag$xcb_hnag)
                     , list(UID=meancb_hnoag$UID), sum, na.rm=T)
xf_hnoag <- aggregate(list(xf_hnoag=meanf_hnoag$xf_hnoag)
                    , list(UID=meanf_hnoag$UID), sum, na.rm=T) 

df3 <- merge(xb_hall, xc_hall, all=T)
df3 <- merge(df3, xcb_hall, all=T)
df3 <- merge(df3, xf_hall, all=T)
df3 <- merge(df3, xb_hag, all=T)
df3 <- merge(df3, xc_hag, all=T)
df3 <- merge(df3, xcb_hag, all=T)
df3 <- merge(df3, xf_hag, all=T)
df3 <- merge(df3, xb_hnoag, all=T)
df3 <- merge(df3, xc_hnoag, all=T)
df3 <- merge(df3, xcb_hnoag, all=T)
df3 <- merge(df3, xf_hnoag, all=T)

df3$METRIC <- ''

df3$x_hall <- df3$xb_hall + df3$xc_hall + df3$xf_hall
df3$x_hag <-  df3$xb_hag + df3$xc_hag + df3$xf_hag
df3$x_hnoag <- df3$xb_hnoag + df3$xc_hnoag + df3$xf_hnoag

## Creating dataframes for export to csv file
x_hall <- subset(df3, select=c(UID,METRIC,x_hall))
 x_hall$METRIC <- 'x_hall'
 x_hall <- rename(x_hall, 'x_hall', 'RESULT')
x_hag <- subset(df3, select=c(UID,METRIC,x_hag))
 x_hag$METRIC <- 'x_hag'
 x_hag <- rename(x_hag, 'x_hag', 'RESULT')
x_hnoag <- subset(df3, select=c(UID,METRIC,x_hnoag))
 x_hnoag$METRIC <- 'x_hnoag'
 x_hnoag <- rename(x_hnoag, 'x_hnoag', 'RESULT')
 
 xb_hall <- rename(xb_hall, 'xb_hall', 'RESULT')
  xb_hall$METRIC <- 'xb_hall' 
 xc_hall <- rename(xc_hall, 'xc_hall', 'RESULT')
  xc_hall$METRIC <- 'xc_hall' 
 xcb_hall <- rename(xcb_hall, 'xcb_hall', 'RESULT')
  xcb_hall$METRIC <- 'xcb_hall' 
 xf_hall <- rename(xf_hall, 'xf_hall', 'RESULT')
  xf_hall$METRIC <- 'xf_hall' 
 xb_hag <- rename(xb_hag, 'xb_hag', 'RESULT')
  xb_hag$METRIC <- 'xb_hag' 
 xc_hag <- rename(xc_hag, 'xc_hag', 'RESULT')
  xc_hag$METRIC <- 'xc_hag' 
 xcb_hag <- rename(xcb_hag, 'xcb_hag', 'RESULT')
  xcb_hag$METRIC <- 'xcb_hag' 
 xf_hag <- rename(xf_hag, 'xf_hag', 'RESULT')
  xf_hag$METRIC <- 'xf_hag' 
 xb_hnoag <- rename(xb_hnoag, 'xb_hnoag', 'RESULT')
  xb_hnoag$METRIC <- 'xb_hnoag' 
 xc_hnoag <- rename(xc_hnoag, 'xc_hnoag', 'RESULT')
  xc_hnoag$METRIC <- 'xc_hnoag' 
 xcb_hnoag <- rename(xcb_hnoag, 'xcb_hnag', 'RESULT')
  xcb_hnoag$METRIC <- 'xcb_hnag' 
 xf_hnoag <- rename(xf_hnoag, 'xf_hnoag', 'RESULT')
  xf_hnoag$METRIC <- 'xf_hnoag' 

 ## Standard deviations of sumAtTransect
sumb_hall <- aggregate(list(sum_b=df2$var_B), list(UID=df2$UID, TRANSECT=df2$TRANSECT), sum, na.rm=T)
sumc_hall <- aggregate(list(sum_c=df2$var_C), list(UID=df2$UID, TRANSECT=df2$TRANSECT), sum, na.rm=T)
sumcb_hall <- aggregate(list(sum_cb=df2$var_CB), list(UID=df2$UID, TRANSECT=df2$TRANSECT), sum, na.rm=T)
df2$wcb = (1.5 * df2$var_B) + df2$var_C
sumwcb_hall <- aggregate(list(sum_wcb=df2$wcb), list(UID=df2$UID, TRANSECT=df2$TRANSECT), sum, na.rm=T)


sdb_hall <- aggregate(list(RESULT=sumb_hall$sum_b), list(UID=sumb_hall$UID), sd)
 sdb_hall$METRIC <- 'sdb_hall'
sdc_hall <- aggregate(list(RESULT=sumc_hall$sum_c), list(UID=sumc_hall$UID), sd)
 sdc_hall$METRIC <- 'sdc_hall'
sdcb_hall <- aggregate(list(RESULT=sumcb_hall$sum_cb), list(UID=sumcb_hall$UID), sd)
 sdcb_hall$METRIC <- 'sdcb_hall'
sdwcb_hall <- aggregate(list(RESULT=sumwcb_hall$sum_wcb), list(UID=sumwcb_hall$UID), sd)
 sdwcb_hall$METRIC <- 'sdwcb_hall'

## Weighted sums 
df3$w1_hall <- (1.5 * df3$xb_hall) + df3$xc_hall + (0.6667 * df3$xf_hall)
df3$w1_hnoag <- (1.5 * df3$xb_hnoag) + df3$xc_hnoag + (0.6667 * df3$xf_hnoag)
df3$w1_hag <- (1.5 * df3$xb_hag) + df3$xc_hag + (0.6667 * df3$xf_hag)

w1_hall <- subset(df3, select=c('UID', 'METRIC', 'w1_hall'))
w1_hnoag <- subset(df3, select=c('UID', 'METRIC', 'w1_hnoag'))
w1_hag <- subset(df3, select=c('UID', 'METRIC', 'w1_hag'))
                                  
# Create table for weighing influence proximity values
weights0PCB <- data.frame(proximity=c('0', 'P', 'C', 'B')
                           ,calc=     c(0.0, 0.6667, 1.0, 1.5)
                           , stringsAsFactors=F
                          )

# Convert proximity classes to numeric values and characterize influence types
df5 <- merge(df, weights0PCB
                ,by.x='RESULT'
                ,by.y='proximity'
                ,all.x=TRUE
                ,sort=FALSE
                )

##  Creating W1H_ variables
w1h_sums <- aggregate(list(RESULT=df5$calc),
               list(UID=df5$UID, METRIC=df5$PARAMETER),mean, na.rm=T)

w1h_sums$METRIC <- ifelse(w1h_sums$METRIC=='BUILD', 'w1h_bldg', w1h_sums$METRIC)
w1h_sums$METRIC <- ifelse(w1h_sums$METRIC=='LANDFL', 'w1h_ldfl', w1h_sums$METRIC)
w1h_sums$METRIC <- ifelse(w1h_sums$METRIC=='LOG', 'w1h_log', w1h_sums$METRIC)
w1h_sums$METRIC <- ifelse(w1h_sums$METRIC=='MINE', 'w1h_mine', w1h_sums$METRIC)
w1h_sums$METRIC <- ifelse(w1h_sums$METRIC=='PARK', 'w1h_park', w1h_sums$METRIC)
w1h_sums$METRIC <- ifelse(w1h_sums$METRIC=='PAST', 'w1h_pstr', w1h_sums$METRIC)
w1h_sums$METRIC <- ifelse(w1h_sums$METRIC=='PAVE', 'w1h_pvmt', w1h_sums$METRIC)
w1h_sums$METRIC <- ifelse(w1h_sums$METRIC=='PIPES', 'w1h_pipe', w1h_sums$METRIC)
w1h_sums$METRIC <- ifelse(w1h_sums$METRIC=='ROAD', 'w1h_road', w1h_sums$METRIC)
w1h_sums$METRIC <- ifelse(w1h_sums$METRIC=='ROW', 'w1h_crop', w1h_sums$METRIC)
w1h_sums$METRIC <- ifelse(w1h_sums$METRIC=='WALL', 'w1h_wall', w1h_sums$METRIC)


w1_ag <- subset(w1h_sums,(METRIC %in% c('w1h_crop', 'w1h_pstr')))
w1_hag <- aggregate(list(RESULT=w1_ag$RESULT), list(UID=w1_ag$UID), sum, na.rm=T)
w1_hag$METRIC <- 'w1_hag'

w1_noag <- subset(w1h_sums, !(METRIC %in% c('w1h_crop', 'w1h_pstr')))
w1_hnoag <- aggregate(list(RESULT=w1_noag$RESULT), list(UID=w1_noag$UID), sum, na.rm=T)
w1_hnoag$METRIC <- 'w1_hnoag'

w1_hall <- aggregate(list(RESULT=w1h_sums$RESULT),list(UID=w1h_sums$UID),sum, na.rm=T)
w1_hall$METRIC <- 'w1_hall'




aa <- rbind(xb_hag,xc_hag,xcb_hag,xf_hag,xb_hnoag,xcb_hnoag,xc_hnoag,xf_hnoag
           ,xb_hall,xcb_hall,xc_hall,xf_hall,sdb_hall,sdcb_hall,sdc_hall
           ,sdwcb_hall,w1_hag,w1_hnoag,w1_hall)
bb <- aa[,c('UID', 'METRIC', 'RESULT')] 
hiMets <- rbind(bb,w1h_sums,x_hag,x_hnoag,x_hall)
 
}

#####################################
##  Testing metsHumanInfluence
##  Correct results return [1] TRUE
metsHumanInfluenceTest <- function()
{
  ## Create test data, expected results and actual results
  testData <- metsHumanInfluence.createData()
  testResults <- metsHumanInfluence.createResults()
  rr <- metsHumanInfluence.1(testData)

  ## Get both results data frames in the same order
  testMets <- rr[order(rr$UID, rr$METRIC),]
  sasMets <- testResults[order(testResults$UID, testResults$METRIC),]

  ##  Compare values in data frames
  errs <- dfCompare(sasMets, testMets, c('UID','METRIC'), zeroFudge=1e-9)
  checkEquals(NULL, errs, "Error: metsHumanInfluence is broken.")

}

metsHumanInfluence.createData <- function()
# Create dataframe of human influence data for unit test
{
  fred <- textConnection(
              "UID TRANSECT TRANSDIR PARAMETER RESULT
              'EPA01-0159' 'A' 'LF' 'WALL' '0'
              'EPA01-0159' 'A' 'LF' 'BUILD' '0'
              'EPA01-0159' 'A' 'LF' 'PAVE' '0'
              'EPA01-0159' 'A' 'LF' 'ROAD' '0'
              'EPA01-0159' 'A' 'LF' 'PIPES' '0'
              'EPA01-0159' 'A' 'LF' 'LANDFL' '0'
              'EPA01-0159' 'A' 'LF' 'PARK' '0'
              'EPA01-0159' 'A' 'LF' 'ROW' '0'
              'EPA01-0159' 'A' 'LF' 'PAST' '0'
              'EPA01-0159' 'A' 'LF' 'LOG' '0'
              'EPA01-0159' 'A' 'LF' 'MINE' '0'
              'EPA01-0159' 'A' 'RT' 'WALL' '0'
              'EPA01-0159' 'A' 'RT' 'BUILD' '0'
              'EPA01-0159' 'A' 'RT' 'PAVE' '0'
              'EPA01-0159' 'A' 'RT' 'ROAD' '0'
              'EPA01-0159' 'A' 'RT' 'PIPES' '0'
              'EPA01-0159' 'A' 'RT' 'LANDFL' '0'
              'EPA01-0159' 'A' 'RT' 'PARK' '0'
              'EPA01-0159' 'A' 'RT' 'ROW' '0'
              'EPA01-0159' 'A' 'RT' 'PAST' '0'
              'EPA01-0159' 'A' 'RT' 'LOG' '0'
              'EPA01-0159' 'A' 'RT' 'MINE' '0'
              'EPA01-0159' 'B' 'LF' 'WALL' '0'
              'EPA01-0159' 'B' 'LF' 'BUILD' '0'
              'EPA01-0159' 'B' 'LF' 'PAVE' '0'
              'EPA01-0159' 'B' 'LF' 'ROAD' '0'
              'EPA01-0159' 'B' 'LF' 'PIPES' '0'
              'EPA01-0159' 'B' 'LF' 'LANDFL' '0'
              'EPA01-0159' 'B' 'LF' 'PARK' '0'
              'EPA01-0159' 'B' 'LF' 'ROW' '0'
              'EPA01-0159' 'B' 'LF' 'PAST' 'C'
              'EPA01-0159' 'B' 'LF' 'LOG' '0'
              'EPA01-0159' 'B' 'LF' 'MINE' '0'
              'EPA01-0159' 'B' 'RT' 'WALL' '0'
              'EPA01-0159' 'B' 'RT' 'BUILD' '0'
              'EPA01-0159' 'B' 'RT' 'PAVE' '0'
              'EPA01-0159' 'B' 'RT' 'ROAD' '0'
              'EPA01-0159' 'B' 'RT' 'PIPES' '0'
              'EPA01-0159' 'B' 'RT' 'LANDFL' '0'
              'EPA01-0159' 'B' 'RT' 'PARK' '0'
              'EPA01-0159' 'B' 'RT' 'ROW' '0'
              'EPA01-0159' 'B' 'RT' 'PAST' 'P'
              'EPA01-0159' 'B' 'RT' 'LOG' '0'
              'EPA01-0159' 'B' 'RT' 'MINE' '0'
              'EPA01-0159' 'C' 'LF' 'WALL' '0'
              'EPA01-0159' 'C' 'LF' 'BUILD' '0'
              'EPA01-0159' 'C' 'LF' 'PAVE' '0'
              'EPA01-0159' 'C' 'LF' 'ROAD' '0'
              'EPA01-0159' 'C' 'LF' 'PIPES' '0'
              'EPA01-0159' 'C' 'LF' 'LANDFL' '0'
              'EPA01-0159' 'C' 'LF' 'PARK' '0'
              'EPA01-0159' 'C' 'LF' 'ROW' '0'
              'EPA01-0159' 'C' 'LF' 'PAST' 'P'
              'EPA01-0159' 'C' 'LF' 'LOG' '0'
              'EPA01-0159' 'C' 'LF' 'MINE' '0'
              'EPA01-0159' 'C' 'RT' 'WALL' '0'
              'EPA01-0159' 'C' 'RT' 'BUILD' '0'
              'EPA01-0159' 'C' 'RT' 'PAVE' '0'
              'EPA01-0159' 'C' 'RT' 'ROAD' '0'
              'EPA01-0159' 'C' 'RT' 'PIPES' '0'
              'EPA01-0159' 'C' 'RT' 'LANDFL' '0'
              'EPA01-0159' 'C' 'RT' 'PARK' '0'
              'EPA01-0159' 'C' 'RT' 'ROW' '0'
              'EPA01-0159' 'C' 'RT' 'PAST' 'P'
              'EPA01-0159' 'C' 'RT' 'LOG' '0'
              'EPA01-0159' 'C' 'RT' 'MINE' '0'
              'EPA01-0159' 'D' 'LF' 'WALL' '0'
              'EPA01-0159' 'D' 'LF' 'BUILD' '0'
              'EPA01-0159' 'D' 'LF' 'PAVE' '0'
              'EPA01-0159' 'D' 'LF' 'ROAD' '0'
              'EPA01-0159' 'D' 'LF' 'PIPES' '0'
              'EPA01-0159' 'D' 'LF' 'LANDFL' '0'
              'EPA01-0159' 'D' 'LF' 'PARK' '0'
              'EPA01-0159' 'D' 'LF' 'ROW' '0'
              'EPA01-0159' 'D' 'LF' 'PAST' 'P'
              'EPA01-0159' 'D' 'LF' 'LOG' '0'
              'EPA01-0159' 'D' 'LF' 'MINE' '0'
              'EPA01-0159' 'D' 'RT' 'WALL' '0'
              'EPA01-0159' 'D' 'RT' 'BUILD' '0'
              'EPA01-0159' 'D' 'RT' 'PAVE' '0'
              'EPA01-0159' 'D' 'RT' 'ROAD' '0'
              'EPA01-0159' 'D' 'RT' 'PIPES' '0'
              'EPA01-0159' 'D' 'RT' 'LANDFL' '0'
              'EPA01-0159' 'D' 'RT' 'PARK' '0'
              'EPA01-0159' 'D' 'RT' 'ROW' '0'
              'EPA01-0159' 'D' 'RT' 'PAST' 'P'
              'EPA01-0159' 'D' 'RT' 'LOG' '0'
              'EPA01-0159' 'D' 'RT' 'MINE' '0'
              'EPA01-0159' 'E' 'LF' 'WALL' '0'
              'EPA01-0159' 'E' 'LF' 'BUILD' '0'
              'EPA01-0159' 'E' 'LF' 'PAVE' '0'
              'EPA01-0159' 'E' 'LF' 'ROAD' '0'
              'EPA01-0159' 'E' 'LF' 'PIPES' '0'
              'EPA01-0159' 'E' 'LF' 'LANDFL' '0'
              'EPA01-0159' 'E' 'LF' 'PARK' '0'
              'EPA01-0159' 'E' 'LF' 'ROW' '0'
              'EPA01-0159' 'E' 'LF' 'PAST' 'P'
              'EPA01-0159' 'E' 'LF' 'LOG' '0'
              'EPA01-0159' 'E' 'LF' 'MINE' '0'
              'EPA01-0159' 'E' 'RT' 'WALL' '0'
              'EPA01-0159' 'E' 'RT' 'BUILD' '0'
              'EPA01-0159' 'E' 'RT' 'PAVE' '0'
              'EPA01-0159' 'E' 'RT' 'ROAD' '0'
              'EPA01-0159' 'E' 'RT' 'PIPES' '0'
              'EPA01-0159' 'E' 'RT' 'LANDFL' '0'
              'EPA01-0159' 'E' 'RT' 'PARK' '0'
              'EPA01-0159' 'E' 'RT' 'ROW' '0'
              'EPA01-0159' 'E' 'RT' 'PAST' 'P'
              'EPA01-0159' 'E' 'RT' 'LOG' '0'
              'EPA01-0159' 'E' 'RT' 'MINE' '0'
              'EPA01-0159' 'F' 'LF' 'WALL' '0'
              'EPA01-0159' 'F' 'LF' 'BUILD' '0'
              'EPA01-0159' 'F' 'LF' 'PAVE' '0'
              'EPA01-0159' 'F' 'LF' 'ROAD' '0'
              'EPA01-0159' 'F' 'LF' 'PIPES' '0'
              'EPA01-0159' 'F' 'LF' 'LANDFL' '0'
              'EPA01-0159' 'F' 'LF' 'PARK' '0'
              'EPA01-0159' 'F' 'LF' 'ROW' '0'
              'EPA01-0159' 'F' 'LF' 'PAST' 'P'
              'EPA01-0159' 'F' 'LF' 'LOG' '0'
              'EPA01-0159' 'F' 'LF' 'MINE' '0'
              'EPA01-0159' 'F' 'RT' 'WALL' '0'
              'EPA01-0159' 'F' 'RT' 'BUILD' '0'
              'EPA01-0159' 'F' 'RT' 'PAVE' '0'
              'EPA01-0159' 'F' 'RT' 'ROAD' '0'
              'EPA01-0159' 'F' 'RT' 'PIPES' '0'
              'EPA01-0159' 'F' 'RT' 'LANDFL' '0'
              'EPA01-0159' 'F' 'RT' 'PARK' '0'
              'EPA01-0159' 'F' 'RT' 'ROW' '0'
              'EPA01-0159' 'F' 'RT' 'PAST' 'P'
              'EPA01-0159' 'F' 'RT' 'LOG' '0'
              'EPA01-0159' 'F' 'RT' 'MINE' '0'
              'EPA01-0159' 'G' 'LF' 'WALL' '0'
              'EPA01-0159' 'G' 'LF' 'BUILD' '0'
              'EPA01-0159' 'G' 'LF' 'PAVE' '0'
              'EPA01-0159' 'G' 'LF' 'ROAD' '0'
              'EPA01-0159' 'G' 'LF' 'PIPES' '0'
              'EPA01-0159' 'G' 'LF' 'LANDFL' '0'
              'EPA01-0159' 'G' 'LF' 'PARK' '0'
              'EPA01-0159' 'G' 'LF' 'ROW' '0'
              'EPA01-0159' 'G' 'LF' 'PAST' 'P'
              'EPA01-0159' 'G' 'LF' 'LOG' '0'
              'EPA01-0159' 'G' 'LF' 'MINE' '0'
              'EPA01-0159' 'G' 'RT' 'WALL' '0'
              'EPA01-0159' 'G' 'RT' 'BUILD' '0'
              'EPA01-0159' 'G' 'RT' 'PAVE' '0'
              'EPA01-0159' 'G' 'RT' 'ROAD' '0'
              'EPA01-0159' 'G' 'RT' 'PIPES' '0'
              'EPA01-0159' 'G' 'RT' 'LANDFL' '0'
              'EPA01-0159' 'G' 'RT' 'PARK' '0'
              'EPA01-0159' 'G' 'RT' 'ROW' '0'
              'EPA01-0159' 'G' 'RT' 'PAST' 'P'
              'EPA01-0159' 'G' 'RT' 'LOG' '0'
              'EPA01-0159' 'G' 'RT' 'MINE' '0'
              'EPA01-0159' 'H' 'LF' 'WALL' '0'
              'EPA01-0159' 'H' 'LF' 'BUILD' '0'
              'EPA01-0159' 'H' 'LF' 'PAVE' '0'
              'EPA01-0159' 'H' 'LF' 'ROAD' '0'
              'EPA01-0159' 'H' 'LF' 'PIPES' '0'
              'EPA01-0159' 'H' 'LF' 'LANDFL' '0'
              'EPA01-0159' 'H' 'LF' 'PARK' '0'
              'EPA01-0159' 'H' 'LF' 'ROW' '0'
              'EPA01-0159' 'H' 'LF' 'PAST' 'P'
              'EPA01-0159' 'H' 'LF' 'LOG' '0'
              'EPA01-0159' 'H' 'LF' 'MINE' '0'
              'EPA01-0159' 'H' 'RT' 'WALL' '0'
              'EPA01-0159' 'H' 'RT' 'BUILD' '0'
              'EPA01-0159' 'H' 'RT' 'PAVE' '0'
              'EPA01-0159' 'H' 'RT' 'ROAD' '0'
              'EPA01-0159' 'H' 'RT' 'PIPES' '0'
              'EPA01-0159' 'H' 'RT' 'LANDFL' '0'
              'EPA01-0159' 'H' 'RT' 'PARK' '0'
              'EPA01-0159' 'H' 'RT' 'ROW' '0'
              'EPA01-0159' 'H' 'RT' 'PAST' 'P'
              'EPA01-0159' 'H' 'RT' 'LOG' '0'
              'EPA01-0159' 'H' 'RT' 'MINE' '0'
              'EPA01-0159' 'I' 'LF' 'WALL' '0'
              'EPA01-0159' 'I' 'LF' 'BUILD' '0'
              'EPA01-0159' 'I' 'LF' 'PAVE' '0'
              'EPA01-0159' 'I' 'LF' 'ROAD' '0'
              'EPA01-0159' 'I' 'LF' 'PIPES' '0'
              'EPA01-0159' 'I' 'LF' 'LANDFL' '0'
              'EPA01-0159' 'I' 'LF' 'PARK' '0'
              'EPA01-0159' 'I' 'LF' 'ROW' '0'
              'EPA01-0159' 'I' 'LF' 'PAST' 'P'
              'EPA01-0159' 'I' 'LF' 'LOG' '0'
              'EPA01-0159' 'I' 'LF' 'MINE' '0'
              'EPA01-0159' 'I' 'RT' 'WALL' '0'
              'EPA01-0159' 'I' 'RT' 'BUILD' '0'
              'EPA01-0159' 'I' 'RT' 'PAVE' '0'
              'EPA01-0159' 'I' 'RT' 'ROAD' '0'
              'EPA01-0159' 'I' 'RT' 'PIPES' '0'
              'EPA01-0159' 'I' 'RT' 'LANDFL' '0'
              'EPA01-0159' 'I' 'RT' 'PARK' '0'
              'EPA01-0159' 'I' 'RT' 'ROW' '0'
              'EPA01-0159' 'I' 'RT' 'PAST' 'P'
              'EPA01-0159' 'I' 'RT' 'LOG' '0'
              'EPA01-0159' 'I' 'RT' 'MINE' '0'
              'EPA01-0159' 'J' 'LF' 'WALL' '0'
              'EPA01-0159' 'J' 'LF' 'BUILD' '0'
              'EPA01-0159' 'J' 'LF' 'PAVE' '0'
              'EPA01-0159' 'J' 'LF' 'ROAD' '0'
              'EPA01-0159' 'J' 'LF' 'PIPES' '0'
              'EPA01-0159' 'J' 'LF' 'LANDFL' '0'
              'EPA01-0159' 'J' 'LF' 'PARK' '0'
              'EPA01-0159' 'J' 'LF' 'ROW' '0'
              'EPA01-0159' 'J' 'LF' 'PAST' 'P'
              'EPA01-0159' 'J' 'LF' 'LOG' '0'
              'EPA01-0159' 'J' 'LF' 'MINE' '0'
              'EPA01-0159' 'J' 'RT' 'WALL' '0'
              'EPA01-0159' 'J' 'RT' 'BUILD' '0'
              'EPA01-0159' 'J' 'RT' 'PAVE' '0'
              'EPA01-0159' 'J' 'RT' 'ROAD' '0'
              'EPA01-0159' 'J' 'RT' 'PIPES' '0'
              'EPA01-0159' 'J' 'RT' 'LANDFL' '0'
              'EPA01-0159' 'J' 'RT' 'PARK' '0'
              'EPA01-0159' 'J' 'RT' 'ROW' '0'
              'EPA01-0159' 'J' 'RT' 'PAST' 'P'
              'EPA01-0159' 'J' 'RT' 'LOG' '0'
              'EPA01-0159' 'J' 'RT' 'MINE' '0'
              'EPA01-0159' 'K' 'LF' 'WALL' '0'
              'EPA01-0159' 'K' 'LF' 'BUILD' '0'
              'EPA01-0159' 'K' 'LF' 'PAVE' '0'
              'EPA01-0159' 'K' 'LF' 'ROAD' '0'
              'EPA01-0159' 'K' 'LF' 'PIPES' '0'
              'EPA01-0159' 'K' 'LF' 'LANDFL' '0'
              'EPA01-0159' 'K' 'LF' 'PARK' '0'
              'EPA01-0159' 'K' 'LF' 'ROW' '0'
              'EPA01-0159' 'K' 'LF' 'PAST' 'P'
              'EPA01-0159' 'K' 'LF' 'LOG' '0'
              'EPA01-0159' 'K' 'LF' 'MINE' '0'
              'EPA01-0159' 'K' 'RT' 'WALL' '0'
              'EPA01-0159' 'K' 'RT' 'BUILD' '0'
              'EPA01-0159' 'K' 'RT' 'PAVE' '0'
              'EPA01-0159' 'K' 'RT' 'ROAD' '0'
              'EPA01-0159' 'K' 'RT' 'PIPES' '0'
              'EPA01-0159' 'K' 'RT' 'LANDFL' '0'
              'EPA01-0159' 'K' 'RT' 'PARK' '0'
              'EPA01-0159' 'K' 'RT' 'ROW' '0'
              'EPA01-0159' 'K' 'RT' 'PAST' 'P'
              'EPA01-0159' 'K' 'RT' 'LOG' '0'
              'EPA01-0159' 'K' 'RT' 'MINE' '0'
              'EPA01-0161' 'A' 'LF' 'WALL' '0'
              'EPA01-0161' 'A' 'LF' 'BUILD' '0'
              'EPA01-0161' 'A' 'LF' 'PAVE' '0'
              'EPA01-0161' 'A' 'LF' 'ROAD' '0'
              'EPA01-0161' 'A' 'LF' 'PIPES' '0'
              'EPA01-0161' 'A' 'LF' 'LANDFL' '0'
              'EPA01-0161' 'A' 'LF' 'PARK' '0'
              'EPA01-0161' 'A' 'LF' 'ROW' '0'
              'EPA01-0161' 'A' 'LF' 'PAST' '0'
              'EPA01-0161' 'A' 'LF' 'LOG' '0'
              'EPA01-0161' 'A' 'LF' 'MINE' '0'
              'EPA01-0161' 'A' 'RT' 'WALL' '0'
              'EPA01-0161' 'A' 'RT' 'BUILD' '0'
              'EPA01-0161' 'A' 'RT' 'PAVE' '0'
              'EPA01-0161' 'A' 'RT' 'ROAD' '0'
              'EPA01-0161' 'A' 'RT' 'PIPES' '0'
              'EPA01-0161' 'A' 'RT' 'LANDFL' '0'
              'EPA01-0161' 'A' 'RT' 'PARK' '0'
              'EPA01-0161' 'A' 'RT' 'ROW' 'P'
              'EPA01-0161' 'A' 'RT' 'PAST' '0'
              'EPA01-0161' 'A' 'RT' 'LOG' '0'
              'EPA01-0161' 'A' 'RT' 'MINE' '0'
              'EPA01-0161' 'B' 'LF' 'WALL' '0'
              'EPA01-0161' 'B' 'LF' 'BUILD' '0'
              'EPA01-0161' 'B' 'LF' 'PAVE' '0'
              'EPA01-0161' 'B' 'LF' 'ROAD' '0'
              'EPA01-0161' 'B' 'LF' 'PIPES' '0'
              'EPA01-0161' 'B' 'LF' 'LANDFL' '0'
              'EPA01-0161' 'B' 'LF' 'PARK' '0'
              'EPA01-0161' 'B' 'LF' 'ROW' '0'
              'EPA01-0161' 'B' 'LF' 'PAST' '0'
              'EPA01-0161' 'B' 'LF' 'LOG' '0'
              'EPA01-0161' 'B' 'LF' 'MINE' '0'
              'EPA01-0161' 'B' 'RT' 'WALL' '0'
              'EPA01-0161' 'B' 'RT' 'BUILD' '0'
              'EPA01-0161' 'B' 'RT' 'PAVE' '0'
              'EPA01-0161' 'B' 'RT' 'ROAD' '0'
              'EPA01-0161' 'B' 'RT' 'PIPES' '0'
              'EPA01-0161' 'B' 'RT' 'LANDFL' '0'
              'EPA01-0161' 'B' 'RT' 'PARK' '0'
              'EPA01-0161' 'B' 'RT' 'ROW' 'P'
              'EPA01-0161' 'B' 'RT' 'PAST' '0'
              'EPA01-0161' 'B' 'RT' 'LOG' '0'
              'EPA01-0161' 'B' 'RT' 'MINE' '0'
              'EPA01-0161' 'C' 'LF' 'WALL' '0'
              'EPA01-0161' 'C' 'LF' 'BUILD' '0'
              'EPA01-0161' 'C' 'LF' 'PAVE' '0'
              'EPA01-0161' 'C' 'LF' 'ROAD' '0'
              'EPA01-0161' 'C' 'LF' 'PIPES' '0'
              'EPA01-0161' 'C' 'LF' 'LANDFL' '0'
              'EPA01-0161' 'C' 'LF' 'PARK' '0'
              'EPA01-0161' 'C' 'LF' 'ROW' 'P'
              'EPA01-0161' 'C' 'LF' 'PAST' '0'
              'EPA01-0161' 'C' 'LF' 'LOG' '0'
              'EPA01-0161' 'C' 'LF' 'MINE' '0'
              'EPA01-0161' 'C' 'RT' 'WALL' '0'
              'EPA01-0161' 'C' 'RT' 'BUILD' '0'
              'EPA01-0161' 'C' 'RT' 'PAVE' '0'
              'EPA01-0161' 'C' 'RT' 'ROAD' '0'
              'EPA01-0161' 'C' 'RT' 'PIPES' '0'
              'EPA01-0161' 'C' 'RT' 'LANDFL' '0'
              'EPA01-0161' 'C' 'RT' 'PARK' '0'
              'EPA01-0161' 'C' 'RT' 'ROW' 'P'
              'EPA01-0161' 'C' 'RT' 'PAST' '0'
              'EPA01-0161' 'C' 'RT' 'LOG' '0'
              'EPA01-0161' 'C' 'RT' 'MINE' '0'
              'EPA01-0161' 'D' 'LF' 'WALL' '0'
              'EPA01-0161' 'D' 'LF' 'BUILD' '0'
              'EPA01-0161' 'D' 'LF' 'PAVE' '0'
              'EPA01-0161' 'D' 'LF' 'ROAD' '0'
              'EPA01-0161' 'D' 'LF' 'PIPES' '0'
              'EPA01-0161' 'D' 'LF' 'LANDFL' '0'
              'EPA01-0161' 'D' 'LF' 'PARK' '0'
              'EPA01-0161' 'D' 'LF' 'ROW' 'P'
              'EPA01-0161' 'D' 'LF' 'PAST' '0'
              'EPA01-0161' 'D' 'LF' 'LOG' '0'
              'EPA01-0161' 'D' 'LF' 'MINE' '0'
              'EPA01-0161' 'D' 'RT' 'WALL' '0'
              'EPA01-0161' 'D' 'RT' 'BUILD' '0'
              'EPA01-0161' 'D' 'RT' 'PAVE' '0'
              'EPA01-0161' 'D' 'RT' 'ROAD' '0'
              'EPA01-0161' 'D' 'RT' 'PIPES' '0'
              'EPA01-0161' 'D' 'RT' 'LANDFL' '0'
              'EPA01-0161' 'D' 'RT' 'PARK' '0'
              'EPA01-0161' 'D' 'RT' 'ROW' 'P'
              'EPA01-0161' 'D' 'RT' 'PAST' '0'
              'EPA01-0161' 'D' 'RT' 'LOG' '0'
              'EPA01-0161' 'D' 'RT' 'MINE' '0'
              'EPA01-0161' 'E' 'LF' 'WALL' '0'
              'EPA01-0161' 'E' 'LF' 'BUILD' '0'
              'EPA01-0161' 'E' 'LF' 'PAVE' '0'
              'EPA01-0161' 'E' 'LF' 'ROAD' '0'
              'EPA01-0161' 'E' 'LF' 'PIPES' '0'
              'EPA01-0161' 'E' 'LF' 'LANDFL' '0'
              'EPA01-0161' 'E' 'LF' 'PARK' '0'
              'EPA01-0161' 'E' 'LF' 'ROW' 'P'
              'EPA01-0161' 'E' 'LF' 'PAST' '0'
              'EPA01-0161' 'E' 'LF' 'LOG' '0'
              'EPA01-0161' 'E' 'LF' 'MINE' '0'
              'EPA01-0161' 'E' 'RT' 'WALL' '0'
              'EPA01-0161' 'E' 'RT' 'BUILD' '0'
              'EPA01-0161' 'E' 'RT' 'PAVE' '0'
              'EPA01-0161' 'E' 'RT' 'ROAD' '0'
              'EPA01-0161' 'E' 'RT' 'PIPES' '0'
              'EPA01-0161' 'E' 'RT' 'LANDFL' '0'
              'EPA01-0161' 'E' 'RT' 'PARK' '0'
              'EPA01-0161' 'E' 'RT' 'ROW' 'P'
              'EPA01-0161' 'E' 'RT' 'PAST' '0'
              'EPA01-0161' 'E' 'RT' 'LOG' '0'
              'EPA01-0161' 'E' 'RT' 'MINE' '0'
              'EPA01-0161' 'F' 'LF' 'WALL' '0'
              'EPA01-0161' 'F' 'LF' 'BUILD' '0'
              'EPA01-0161' 'F' 'LF' 'PAVE' '0'
              'EPA01-0161' 'F' 'LF' 'ROAD' '0'
              'EPA01-0161' 'F' 'LF' 'PIPES' '0'
              'EPA01-0161' 'F' 'LF' 'LANDFL' '0'
              'EPA01-0161' 'F' 'LF' 'PARK' '0'
              'EPA01-0161' 'F' 'LF' 'ROW' 'P'
              'EPA01-0161' 'F' 'LF' 'PAST' '0'
              'EPA01-0161' 'F' 'LF' 'LOG' '0'
              'EPA01-0161' 'F' 'LF' 'MINE' '0'
              'EPA01-0161' 'F' 'RT' 'WALL' '0'
              'EPA01-0161' 'F' 'RT' 'BUILD' '0'
              'EPA01-0161' 'F' 'RT' 'PAVE' '0'
              'EPA01-0161' 'F' 'RT' 'ROAD' '0'
              'EPA01-0161' 'F' 'RT' 'PIPES' '0'
              'EPA01-0161' 'F' 'RT' 'LANDFL' 'B'
              'EPA01-0161' 'F' 'RT' 'PARK' '0'
              'EPA01-0161' 'F' 'RT' 'ROW' 'P'
              'EPA01-0161' 'F' 'RT' 'PAST' '0'
              'EPA01-0161' 'F' 'RT' 'LOG' '0'
              'EPA01-0161' 'F' 'RT' 'MINE' '0'
              'EPA01-0161' 'G' 'LF' 'WALL' '0'
              'EPA01-0161' 'G' 'LF' 'BUILD' '0'
              'EPA01-0161' 'G' 'LF' 'PAVE' '0'
              'EPA01-0161' 'G' 'LF' 'ROAD' '0'
              'EPA01-0161' 'G' 'LF' 'PIPES' '0'
              'EPA01-0161' 'G' 'LF' 'LANDFL' '0'
              'EPA01-0161' 'G' 'LF' 'PARK' '0'
              'EPA01-0161' 'G' 'LF' 'ROW' 'P'
              'EPA01-0161' 'G' 'LF' 'PAST' '0'
              'EPA01-0161' 'G' 'LF' 'LOG' '0'
              'EPA01-0161' 'G' 'LF' 'MINE' '0'
              'EPA01-0161' 'G' 'RT' 'WALL' '0'
              'EPA01-0161' 'G' 'RT' 'BUILD' '0'
              'EPA01-0161' 'G' 'RT' 'PAVE' '0'
              'EPA01-0161' 'G' 'RT' 'ROAD' '0'
              'EPA01-0161' 'G' 'RT' 'PIPES' '0'
              'EPA01-0161' 'G' 'RT' 'LANDFL' '0'
              'EPA01-0161' 'G' 'RT' 'PARK' '0'
              'EPA01-0161' 'G' 'RT' 'ROW' 'P'
              'EPA01-0161' 'G' 'RT' 'PAST' '0'
              'EPA01-0161' 'G' 'RT' 'LOG' '0'
              'EPA01-0161' 'G' 'RT' 'MINE' '0'
              'EPA01-0161' 'H' 'LF' 'WALL' '0'
              'EPA01-0161' 'H' 'LF' 'BUILD' '0'
              'EPA01-0161' 'H' 'LF' 'PAVE' '0'
              'EPA01-0161' 'H' 'LF' 'ROAD' '0'
              'EPA01-0161' 'H' 'LF' 'PIPES' '0'
              'EPA01-0161' 'H' 'LF' 'LANDFL' '0'
              'EPA01-0161' 'H' 'LF' 'PARK' '0'
              'EPA01-0161' 'H' 'LF' 'ROW' 'P'
              'EPA01-0161' 'H' 'LF' 'PAST' '0'
              'EPA01-0161' 'H' 'LF' 'LOG' '0'
              'EPA01-0161' 'H' 'LF' 'MINE' '0'
              'EPA01-0161' 'H' 'RT' 'WALL' '0'
              'EPA01-0161' 'H' 'RT' 'BUILD' '0'
              'EPA01-0161' 'H' 'RT' 'PAVE' '0'
              'EPA01-0161' 'H' 'RT' 'ROAD' '0'
              'EPA01-0161' 'H' 'RT' 'PIPES' '0'
              'EPA01-0161' 'H' 'RT' 'LANDFL' '0'
              'EPA01-0161' 'H' 'RT' 'PARK' '0'
              'EPA01-0161' 'H' 'RT' 'ROW' 'P'
              'EPA01-0161' 'H' 'RT' 'PAST' '0'
              'EPA01-0161' 'H' 'RT' 'LOG' '0'
              'EPA01-0161' 'H' 'RT' 'MINE' '0'
              'EPA01-0161' 'I' 'LF' 'WALL' '0'
              'EPA01-0161' 'I' 'LF' 'BUILD' '0'
              'EPA01-0161' 'I' 'LF' 'PAVE' '0'
              'EPA01-0161' 'I' 'LF' 'ROAD' '0'
              'EPA01-0161' 'I' 'LF' 'PIPES' '0'
              'EPA01-0161' 'I' 'LF' 'LANDFL' '0'
              'EPA01-0161' 'I' 'LF' 'PARK' '0'
              'EPA01-0161' 'I' 'LF' 'ROW' 'P'
              'EPA01-0161' 'I' 'LF' 'PAST' '0'
              'EPA01-0161' 'I' 'LF' 'LOG' '0'
              'EPA01-0161' 'I' 'LF' 'MINE' '0'
              'EPA01-0161' 'I' 'RT' 'WALL' '0'
              'EPA01-0161' 'I' 'RT' 'BUILD' '0'
              'EPA01-0161' 'I' 'RT' 'PAVE' '0'
              'EPA01-0161' 'I' 'RT' 'ROAD' '0'
              'EPA01-0161' 'I' 'RT' 'PIPES' '0'
              'EPA01-0161' 'I' 'RT' 'LANDFL' '0'
              'EPA01-0161' 'I' 'RT' 'PARK' '0'
              'EPA01-0161' 'I' 'RT' 'ROW' 'P'
              'EPA01-0161' 'I' 'RT' 'PAST' '0'
              'EPA01-0161' 'I' 'RT' 'LOG' '0'
              'EPA01-0161' 'I' 'RT' 'MINE' '0'
              'EPA01-0161' 'J' 'LF' 'WALL' '0'
              'EPA01-0161' 'J' 'LF' 'BUILD' '0'
              'EPA01-0161' 'J' 'LF' 'PAVE' '0'
              'EPA01-0161' 'J' 'LF' 'ROAD' '0'
              'EPA01-0161' 'J' 'LF' 'PIPES' '0'
              'EPA01-0161' 'J' 'LF' 'LANDFL' '0'
              'EPA01-0161' 'J' 'LF' 'PARK' '0'
              'EPA01-0161' 'J' 'LF' 'ROW' 'P'
              'EPA01-0161' 'J' 'LF' 'PAST' '0'
              'EPA01-0161' 'J' 'LF' 'LOG' '0'
              'EPA01-0161' 'J' 'LF' 'MINE' '0'
              'EPA01-0161' 'J' 'RT' 'WALL' '0'
              'EPA01-0161' 'J' 'RT' 'BUILD' '0'
              'EPA01-0161' 'J' 'RT' 'PAVE' '0'
              'EPA01-0161' 'J' 'RT' 'ROAD' '0'
              'EPA01-0161' 'J' 'RT' 'PIPES' '0'
              'EPA01-0161' 'J' 'RT' 'LANDFL' '0'
              'EPA01-0161' 'J' 'RT' 'PARK' '0'
              'EPA01-0161' 'J' 'RT' 'ROW' 'P'
              'EPA01-0161' 'J' 'RT' 'PAST' '0'
              'EPA01-0161' 'J' 'RT' 'LOG' '0'
              'EPA01-0161' 'J' 'RT' 'MINE' '0'
              'EPA01-0161' 'K' 'LF' 'WALL' '0'
              'EPA01-0161' 'K' 'LF' 'BUILD' '0'
              'EPA01-0161' 'K' 'LF' 'PAVE' '0'
              'EPA01-0161' 'K' 'LF' 'ROAD' '0'
              'EPA01-0161' 'K' 'LF' 'PIPES' '0'
              'EPA01-0161' 'K' 'LF' 'LANDFL' '0'
              'EPA01-0161' 'K' 'LF' 'PARK' '0'
              'EPA01-0161' 'K' 'LF' 'ROW' 'P'
              'EPA01-0161' 'K' 'LF' 'PAST' '0'
              'EPA01-0161' 'K' 'LF' 'LOG' '0'
              'EPA01-0161' 'K' 'LF' 'MINE' '0'
              'EPA01-0161' 'K' 'RT' 'WALL' '0'
              'EPA01-0161' 'K' 'RT' 'BUILD' '0'
              'EPA01-0161' 'K' 'RT' 'PAVE' '0'
              'EPA01-0161' 'K' 'RT' 'ROAD' '0'
              'EPA01-0161' 'K' 'RT' 'PIPES' '0'
              'EPA01-0161' 'K' 'RT' 'LANDFL' '0'
              'EPA01-0161' 'K' 'RT' 'PARK' '0'
              'EPA01-0161' 'K' 'RT' 'ROW' '0'
              'EPA01-0161' 'K' 'RT' 'PAST' '0'
              'EPA01-0161' 'K' 'RT' 'LOG' '0'
              'EPA01-0161' 'K' 'RT' 'MINE' '0'
              'EPA01-0167' 'A' 'LF' 'WALL' '0'
              'EPA01-0167' 'A' 'LF' 'BUILD' '0'
              'EPA01-0167' 'A' 'LF' 'PAVE' '0'
              'EPA01-0167' 'A' 'LF' 'ROAD' '0'
              'EPA01-0167' 'A' 'LF' 'PIPES' '0'
              'EPA01-0167' 'A' 'LF' 'LANDFL' '0'
              'EPA01-0167' 'A' 'LF' 'PARK' '0'
              'EPA01-0167' 'A' 'LF' 'ROW' '0'
              'EPA01-0167' 'A' 'LF' 'PAST' '0'
              'EPA01-0167' 'A' 'LF' 'LOG' '0'
              'EPA01-0167' 'A' 'LF' 'MINE' '0'
              'EPA01-0167' 'A' 'RT' 'WALL' '0'
              'EPA01-0167' 'A' 'RT' 'BUILD' '0'
              'EPA01-0167' 'A' 'RT' 'PAVE' '0'
              'EPA01-0167' 'A' 'RT' 'ROAD' '0'
              'EPA01-0167' 'A' 'RT' 'PIPES' '0'
              'EPA01-0167' 'A' 'RT' 'LANDFL' '0'
              'EPA01-0167' 'A' 'RT' 'PARK' '0'
              'EPA01-0167' 'A' 'RT' 'ROW' '0'
              'EPA01-0167' 'A' 'RT' 'PAST' '0'
              'EPA01-0167' 'A' 'RT' 'LOG' '0'
              'EPA01-0167' 'A' 'RT' 'MINE' '0'
              'EPA01-0167' 'B' 'LF' 'WALL' '0'
              'EPA01-0167' 'B' 'LF' 'BUILD' '0'
              'EPA01-0167' 'B' 'LF' 'PAVE' '0'
              'EPA01-0167' 'B' 'LF' 'ROAD' '0'
              'EPA01-0167' 'B' 'LF' 'PIPES' '0'
              'EPA01-0167' 'B' 'LF' 'LANDFL' '0'
              'EPA01-0167' 'B' 'LF' 'PARK' '0'
              'EPA01-0167' 'B' 'LF' 'ROW' '0'
              'EPA01-0167' 'B' 'LF' 'PAST' '0'
              'EPA01-0167' 'B' 'LF' 'LOG' '0'
              'EPA01-0167' 'B' 'LF' 'MINE' '0'
              'EPA01-0167' 'B' 'RT' 'WALL' '0'
              'EPA01-0167' 'B' 'RT' 'BUILD' '0'
              'EPA01-0167' 'B' 'RT' 'PAVE' '0'
              'EPA01-0167' 'B' 'RT' 'ROAD' '0'
              'EPA01-0167' 'B' 'RT' 'PIPES' '0'
              'EPA01-0167' 'B' 'RT' 'LANDFL' '0'
              'EPA01-0167' 'B' 'RT' 'PARK' '0'
              'EPA01-0167' 'B' 'RT' 'ROW' '0'
              'EPA01-0167' 'B' 'RT' 'PAST' '0'
              'EPA01-0167' 'B' 'RT' 'LOG' '0'
              'EPA01-0167' 'B' 'RT' 'MINE' '0'
              'EPA01-0167' 'C' 'LF' 'WALL' '0'
              'EPA01-0167' 'C' 'LF' 'BUILD' '0'
              'EPA01-0167' 'C' 'LF' 'PAVE' '0'
              'EPA01-0167' 'C' 'LF' 'ROAD' '0'
              'EPA01-0167' 'C' 'LF' 'PIPES' '0'
              'EPA01-0167' 'C' 'LF' 'LANDFL' '0'
              'EPA01-0167' 'C' 'LF' 'PARK' '0'
              'EPA01-0167' 'C' 'LF' 'ROW' '0'
              'EPA01-0167' 'C' 'LF' 'PAST' '0'
              'EPA01-0167' 'C' 'LF' 'LOG' '0'
              'EPA01-0167' 'C' 'LF' 'MINE' '0'
              'EPA01-0167' 'C' 'RT' 'WALL' '0'
              'EPA01-0167' 'C' 'RT' 'BUILD' '0'
              'EPA01-0167' 'C' 'RT' 'PAVE' '0'
              'EPA01-0167' 'C' 'RT' 'ROAD' '0'
              'EPA01-0167' 'C' 'RT' 'PIPES' '0'
              'EPA01-0167' 'C' 'RT' 'LANDFL' '0'
              'EPA01-0167' 'C' 'RT' 'PARK' '0'
              'EPA01-0167' 'C' 'RT' 'ROW' '0'
              'EPA01-0167' 'C' 'RT' 'PAST' '0'
              'EPA01-0167' 'C' 'RT' 'LOG' '0'
              'EPA01-0167' 'C' 'RT' 'MINE' '0'
              'EPA01-0167' 'D' 'LF' 'WALL' '0'
              'EPA01-0167' 'D' 'LF' 'BUILD' '0'
              'EPA01-0167' 'D' 'LF' 'PAVE' '0'
              'EPA01-0167' 'D' 'LF' 'ROAD' '0'
              'EPA01-0167' 'D' 'LF' 'PIPES' '0'
              'EPA01-0167' 'D' 'LF' 'LANDFL' '0'
              'EPA01-0167' 'D' 'LF' 'PARK' '0'
              'EPA01-0167' 'D' 'LF' 'ROW' '0'
              'EPA01-0167' 'D' 'LF' 'PAST' '0'
              'EPA01-0167' 'D' 'LF' 'LOG' '0'
              'EPA01-0167' 'D' 'LF' 'MINE' '0'
              'EPA01-0167' 'D' 'RT' 'WALL' '0'
              'EPA01-0167' 'D' 'RT' 'BUILD' '0'
              'EPA01-0167' 'D' 'RT' 'PAVE' '0'
              'EPA01-0167' 'D' 'RT' 'ROAD' '0'
              'EPA01-0167' 'D' 'RT' 'PIPES' '0'
              'EPA01-0167' 'D' 'RT' 'LANDFL' '0'
              'EPA01-0167' 'D' 'RT' 'PARK' '0'
              'EPA01-0167' 'D' 'RT' 'ROW' '0'
              'EPA01-0167' 'D' 'RT' 'PAST' '0'
              'EPA01-0167' 'D' 'RT' 'LOG' '0'
              'EPA01-0167' 'D' 'RT' 'MINE' '0'
              'EPA01-0167' 'E' 'LF' 'WALL' '0'
              'EPA01-0167' 'E' 'LF' 'BUILD' '0'
              'EPA01-0167' 'E' 'LF' 'PAVE' '0'
              'EPA01-0167' 'E' 'LF' 'ROAD' '0'
              'EPA01-0167' 'E' 'LF' 'PIPES' '0'
              'EPA01-0167' 'E' 'LF' 'LANDFL' '0'
              'EPA01-0167' 'E' 'LF' 'PARK' '0'
              'EPA01-0167' 'E' 'LF' 'ROW' '0'
              'EPA01-0167' 'E' 'LF' 'PAST' '0'
              'EPA01-0167' 'E' 'LF' 'LOG' '0'
              'EPA01-0167' 'E' 'LF' 'MINE' '0'
              'EPA01-0167' 'E' 'RT' 'WALL' '0'
              'EPA01-0167' 'E' 'RT' 'BUILD' '0'
              'EPA01-0167' 'E' 'RT' 'PAVE' '0'
              'EPA01-0167' 'E' 'RT' 'ROAD' '0'
              'EPA01-0167' 'E' 'RT' 'PIPES' '0'
              'EPA01-0167' 'E' 'RT' 'LANDFL' '0'
              'EPA01-0167' 'E' 'RT' 'PARK' '0'
              'EPA01-0167' 'E' 'RT' 'ROW' '0'
              'EPA01-0167' 'E' 'RT' 'PAST' '0'
              'EPA01-0167' 'E' 'RT' 'LOG' '0'
              'EPA01-0167' 'E' 'RT' 'MINE' '0'
              'EPA01-0167' 'F' 'LF' 'WALL' '0'
              'EPA01-0167' 'F' 'LF' 'BUILD' '0'
              'EPA01-0167' 'F' 'LF' 'PAVE' '0'
              'EPA01-0167' 'F' 'LF' 'ROAD' '0'
              'EPA01-0167' 'F' 'LF' 'PIPES' '0'
              'EPA01-0167' 'F' 'LF' 'LANDFL' '0'
              'EPA01-0167' 'F' 'LF' 'PARK' '0'
              'EPA01-0167' 'F' 'LF' 'ROW' '0'
              'EPA01-0167' 'F' 'LF' 'PAST' '0'
              'EPA01-0167' 'F' 'LF' 'LOG' '0'
              'EPA01-0167' 'F' 'LF' 'MINE' '0'
              'EPA01-0167' 'F' 'RT' 'WALL' '0'
              'EPA01-0167' 'F' 'RT' 'BUILD' '0'
              'EPA01-0167' 'F' 'RT' 'PAVE' '0'
              'EPA01-0167' 'F' 'RT' 'ROAD' '0'
              'EPA01-0167' 'F' 'RT' 'PIPES' '0'
              'EPA01-0167' 'F' 'RT' 'LANDFL' '0'
              'EPA01-0167' 'F' 'RT' 'PARK' '0'
              'EPA01-0167' 'F' 'RT' 'ROW' '0'
              'EPA01-0167' 'F' 'RT' 'PAST' '0'
              'EPA01-0167' 'F' 'RT' 'LOG' '0'
              'EPA01-0167' 'F' 'RT' 'MINE' '0'
              'EPA01-0167' 'G' 'LF' 'WALL' '0'
              'EPA01-0167' 'G' 'LF' 'BUILD' '0'
              'EPA01-0167' 'G' 'LF' 'PAVE' '0'
              'EPA01-0167' 'G' 'LF' 'ROAD' '0'
              'EPA01-0167' 'G' 'LF' 'PIPES' '0'
              'EPA01-0167' 'G' 'LF' 'LANDFL' '0'
              'EPA01-0167' 'G' 'LF' 'PARK' '0'
              'EPA01-0167' 'G' 'LF' 'ROW' '0'
              'EPA01-0167' 'G' 'LF' 'PAST' '0'
              'EPA01-0167' 'G' 'LF' 'LOG' '0'
              'EPA01-0167' 'G' 'LF' 'MINE' '0'
              'EPA01-0167' 'G' 'RT' 'WALL' '0'
              'EPA01-0167' 'G' 'RT' 'BUILD' '0'
              'EPA01-0167' 'G' 'RT' 'PAVE' '0'
              'EPA01-0167' 'G' 'RT' 'ROAD' '0'
              'EPA01-0167' 'G' 'RT' 'PIPES' '0'
              'EPA01-0167' 'G' 'RT' 'LANDFL' '0'
              'EPA01-0167' 'G' 'RT' 'PARK' '0'
              'EPA01-0167' 'G' 'RT' 'ROW' '0'
              'EPA01-0167' 'G' 'RT' 'PAST' '0'
              'EPA01-0167' 'G' 'RT' 'LOG' '0'
              'EPA01-0167' 'G' 'RT' 'MINE' '0'
              'EPA01-0167' 'H' 'LF' 'WALL' '0'
              'EPA01-0167' 'H' 'LF' 'BUILD' '0'
              'EPA01-0167' 'H' 'LF' 'PAVE' '0'
              'EPA01-0167' 'H' 'LF' 'ROAD' '0'
              'EPA01-0167' 'H' 'LF' 'PIPES' '0'
              'EPA01-0167' 'H' 'LF' 'LANDFL' '0'
              'EPA01-0167' 'H' 'LF' 'PARK' '0'
              'EPA01-0167' 'H' 'LF' 'ROW' '0'
              'EPA01-0167' 'H' 'LF' 'PAST' '0'
              'EPA01-0167' 'H' 'LF' 'LOG' '0'
              'EPA01-0167' 'H' 'LF' 'MINE' '0'
              'EPA01-0167' 'H' 'RT' 'WALL' '0'
              'EPA01-0167' 'H' 'RT' 'BUILD' '0'
              'EPA01-0167' 'H' 'RT' 'PAVE' '0'
              'EPA01-0167' 'H' 'RT' 'ROAD' '0'
              'EPA01-0167' 'H' 'RT' 'PIPES' '0'
              'EPA01-0167' 'H' 'RT' 'LANDFL' '0'
              'EPA01-0167' 'H' 'RT' 'PARK' '0'
              'EPA01-0167' 'H' 'RT' 'ROW' '0'
              'EPA01-0167' 'H' 'RT' 'PAST' '0'
              'EPA01-0167' 'H' 'RT' 'LOG' '0'
              'EPA01-0167' 'H' 'RT' 'MINE' '0'
              'EPA01-0167' 'I' 'LF' 'WALL' '0'
              'EPA01-0167' 'I' 'LF' 'BUILD' '0'
              'EPA01-0167' 'I' 'LF' 'PAVE' '0'
              'EPA01-0167' 'I' 'LF' 'ROAD' '0'
              'EPA01-0167' 'I' 'LF' 'PIPES' '0'
              'EPA01-0167' 'I' 'LF' 'LANDFL' '0'
              'EPA01-0167' 'I' 'LF' 'PARK' '0'
              'EPA01-0167' 'I' 'LF' 'ROW' '0'
              'EPA01-0167' 'I' 'LF' 'PAST' '0'
              'EPA01-0167' 'I' 'LF' 'LOG' '0'
              'EPA01-0167' 'I' 'LF' 'MINE' '0'
              'EPA01-0167' 'I' 'RT' 'WALL' '0'
              'EPA01-0167' 'I' 'RT' 'BUILD' '0'
              'EPA01-0167' 'I' 'RT' 'PAVE' '0'
              'EPA01-0167' 'I' 'RT' 'ROAD' '0'
              'EPA01-0167' 'I' 'RT' 'PIPES' '0'
              'EPA01-0167' 'I' 'RT' 'LANDFL' '0'
              'EPA01-0167' 'I' 'RT' 'PARK' '0'
              'EPA01-0167' 'I' 'RT' 'ROW' '0'
              'EPA01-0167' 'I' 'RT' 'PAST' '0'
              'EPA01-0167' 'I' 'RT' 'LOG' '0'
              'EPA01-0167' 'I' 'RT' 'MINE' '0'
              'EPA01-0167' 'J' 'LF' 'WALL' '0'
              'EPA01-0167' 'J' 'LF' 'BUILD' '0'
              'EPA01-0167' 'J' 'LF' 'PAVE' '0'
              'EPA01-0167' 'J' 'LF' 'ROAD' '0'
              'EPA01-0167' 'J' 'LF' 'PIPES' '0'
              'EPA01-0167' 'J' 'LF' 'LANDFL' '0'
              'EPA01-0167' 'J' 'LF' 'PARK' '0'
              'EPA01-0167' 'J' 'LF' 'ROW' '0'
              'EPA01-0167' 'J' 'LF' 'PAST' '0'
              'EPA01-0167' 'J' 'LF' 'LOG' '0'
              'EPA01-0167' 'J' 'LF' 'MINE' '0'
              'EPA01-0167' 'J' 'RT' 'WALL' '0'
              'EPA01-0167' 'J' 'RT' 'BUILD' '0'
              'EPA01-0167' 'J' 'RT' 'PAVE' '0'
              'EPA01-0167' 'J' 'RT' 'ROAD' '0'
              'EPA01-0167' 'J' 'RT' 'PIPES' '0'
              'EPA01-0167' 'J' 'RT' 'LANDFL' '0'
              'EPA01-0167' 'J' 'RT' 'PARK' '0'
              'EPA01-0167' 'J' 'RT' 'ROW' '0'
              'EPA01-0167' 'J' 'RT' 'PAST' '0'
              'EPA01-0167' 'J' 'RT' 'LOG' '0'
              'EPA01-0167' 'J' 'RT' 'MINE' '0'
              'EPA01-0167' 'K' 'LF' 'WALL' '0'
              'EPA01-0167' 'K' 'LF' 'BUILD' '0'
              'EPA01-0167' 'K' 'LF' 'PAVE' '0'
              'EPA01-0167' 'K' 'LF' 'ROAD' '0'
              'EPA01-0167' 'K' 'LF' 'PIPES' '0'
              'EPA01-0167' 'K' 'LF' 'LANDFL' '0'
              'EPA01-0167' 'K' 'LF' 'PARK' '0'
              'EPA01-0167' 'K' 'LF' 'ROW' '0'
              'EPA01-0167' 'K' 'LF' 'PAST' '0'
              'EPA01-0167' 'K' 'LF' 'LOG' '0'
              'EPA01-0167' 'K' 'LF' 'MINE' '0'
              'EPA01-0167' 'K' 'RT' 'WALL' '0'
              'EPA01-0167' 'K' 'RT' 'BUILD' '0'
              'EPA01-0167' 'K' 'RT' 'PAVE' '0'
              'EPA01-0167' 'K' 'RT' 'ROAD' '0'
              'EPA01-0167' 'K' 'RT' 'PIPES' '0'
              'EPA01-0167' 'K' 'RT' 'LANDFL' '0'
              'EPA01-0167' 'K' 'RT' 'PARK' '0'
              'EPA01-0167' 'K' 'RT' 'ROW' '0'
              'EPA01-0167' 'K' 'RT' 'PAST' '0'
              'EPA01-0167' 'K' 'RT' 'LOG' '0'
              'EPA01-0167' 'K' 'RT' 'MINE' '0'
              'EPA01-0209' 'A' 'LF' 'WALL' '0'
              'EPA01-0209' 'A' 'LF' 'BUILD' '0'
              'EPA01-0209' 'A' 'LF' 'PAVE' '0'
              'EPA01-0209' 'A' 'LF' 'ROAD' '0'
              'EPA01-0209' 'A' 'LF' 'PIPES' '0'
              'EPA01-0209' 'A' 'LF' 'LANDFL' '0'
              'EPA01-0209' 'A' 'LF' 'PARK' '0'
              'EPA01-0209' 'A' 'LF' 'ROW' '0'
              'EPA01-0209' 'A' 'LF' 'PAST' '0'
              'EPA01-0209' 'A' 'LF' 'LOG' '0'
              'EPA01-0209' 'A' 'LF' 'MINE' '0'
              'EPA01-0209' 'A' 'RT' 'WALL' '0'
              'EPA01-0209' 'A' 'RT' 'BUILD' '0'
              'EPA01-0209' 'A' 'RT' 'PAVE' '0'
              'EPA01-0209' 'A' 'RT' 'ROAD' '0'
              'EPA01-0209' 'A' 'RT' 'PIPES' '0'
              'EPA01-0209' 'A' 'RT' 'LANDFL' '0'
              'EPA01-0209' 'A' 'RT' 'PARK' '0'
              'EPA01-0209' 'A' 'RT' 'ROW' '0'
              'EPA01-0209' 'A' 'RT' 'PAST' '0'
              'EPA01-0209' 'A' 'RT' 'LOG' '0'
              'EPA01-0209' 'A' 'RT' 'MINE' '0'
              'EPA01-0209' 'B' 'LF' 'WALL' '0'
              'EPA01-0209' 'B' 'LF' 'BUILD' '0'
              'EPA01-0209' 'B' 'LF' 'PAVE' '0'
              'EPA01-0209' 'B' 'LF' 'ROAD' 'B'
              'EPA01-0209' 'B' 'LF' 'PIPES' '0'
              'EPA01-0209' 'B' 'LF' 'LANDFL' '0'
              'EPA01-0209' 'B' 'LF' 'PARK' '0'
              'EPA01-0209' 'B' 'LF' 'ROW' '0'
              'EPA01-0209' 'B' 'LF' 'PAST' '0'
              'EPA01-0209' 'B' 'LF' 'LOG' '0'
              'EPA01-0209' 'B' 'LF' 'MINE' '0'
              'EPA01-0209' 'B' 'RT' 'WALL' '0'
              'EPA01-0209' 'B' 'RT' 'BUILD' '0'
              'EPA01-0209' 'B' 'RT' 'PAVE' '0'
              'EPA01-0209' 'B' 'RT' 'ROAD' 'B'
              'EPA01-0209' 'B' 'RT' 'PIPES' '0'
              'EPA01-0209' 'B' 'RT' 'LANDFL' '0'
              'EPA01-0209' 'B' 'RT' 'PARK' '0'
              'EPA01-0209' 'B' 'RT' 'ROW' '0'
              'EPA01-0209' 'B' 'RT' 'PAST' 'B'
              'EPA01-0209' 'B' 'RT' 'LOG' '0'
              'EPA01-0209' 'B' 'RT' 'MINE' '0'
              'EPA01-0209' 'C' 'LF' 'WALL' '0'
              'EPA01-0209' 'C' 'LF' 'BUILD' '0'
              'EPA01-0209' 'C' 'LF' 'PAVE' '0'
              'EPA01-0209' 'C' 'LF' 'ROAD' 'B'
              'EPA01-0209' 'C' 'LF' 'PIPES' '0'
              'EPA01-0209' 'C' 'LF' 'LANDFL' '0'
              'EPA01-0209' 'C' 'LF' 'PARK' '0'
              'EPA01-0209' 'C' 'LF' 'ROW' '0'
              'EPA01-0209' 'C' 'LF' 'PAST' '0'
              'EPA01-0209' 'C' 'LF' 'LOG' '0'
              'EPA01-0209' 'C' 'LF' 'MINE' '0'
              'EPA01-0209' 'C' 'RT' 'WALL' '0'
              'EPA01-0209' 'C' 'RT' 'BUILD' '0'
              'EPA01-0209' 'C' 'RT' 'PAVE' '0'
              'EPA01-0209' 'C' 'RT' 'ROAD' 'B'
              'EPA01-0209' 'C' 'RT' 'PIPES' '0'
              'EPA01-0209' 'C' 'RT' 'LANDFL' '0'
              'EPA01-0209' 'C' 'RT' 'PARK' '0'
              'EPA01-0209' 'C' 'RT' 'ROW' '0'
              'EPA01-0209' 'C' 'RT' 'PAST' '0'
              'EPA01-0209' 'C' 'RT' 'LOG' '0'
              'EPA01-0209' 'C' 'RT' 'MINE' '0'
              'EPA01-0209' 'D' 'LF' 'WALL' '0'
              'EPA01-0209' 'D' 'LF' 'BUILD' '0'
              'EPA01-0209' 'D' 'LF' 'PAVE' '0'
              'EPA01-0209' 'D' 'LF' 'ROAD' 'P'
              'EPA01-0209' 'D' 'LF' 'PIPES' '0'
              'EPA01-0209' 'D' 'LF' 'LANDFL' '0'
              'EPA01-0209' 'D' 'LF' 'PARK' '0'
              'EPA01-0209' 'D' 'LF' 'ROW' '0'
              'EPA01-0209' 'D' 'LF' 'PAST' '0'
              'EPA01-0209' 'D' 'LF' 'LOG' '0'
              'EPA01-0209' 'D' 'LF' 'MINE' '0'
              'EPA01-0209' 'D' 'RT' 'WALL' '0'
              'EPA01-0209' 'D' 'RT' 'BUILD' '0'
              'EPA01-0209' 'D' 'RT' 'PAVE' '0'
              'EPA01-0209' 'D' 'RT' 'ROAD' '0'
              'EPA01-0209' 'D' 'RT' 'PIPES' '0'
              'EPA01-0209' 'D' 'RT' 'LANDFL' '0'
              'EPA01-0209' 'D' 'RT' 'PARK' '0'
              'EPA01-0209' 'D' 'RT' 'ROW' '0'
              'EPA01-0209' 'D' 'RT' 'PAST' '0'
              'EPA01-0209' 'D' 'RT' 'LOG' '0'
              'EPA01-0209' 'D' 'RT' 'MINE' '0'
              'EPA01-0209' 'E' 'LF' 'WALL' '0'
              'EPA01-0209' 'E' 'LF' 'BUILD' '0'
              'EPA01-0209' 'E' 'LF' 'PAVE' '0'
              'EPA01-0209' 'E' 'LF' 'ROAD' '0'
              'EPA01-0209' 'E' 'LF' 'PIPES' '0'
              'EPA01-0209' 'E' 'LF' 'LANDFL' '0'
              'EPA01-0209' 'E' 'LF' 'PARK' '0'
              'EPA01-0209' 'E' 'LF' 'ROW' '0'
              'EPA01-0209' 'E' 'LF' 'PAST' '0'
              'EPA01-0209' 'E' 'LF' 'LOG' '0'
              'EPA01-0209' 'E' 'LF' 'MINE' '0'
              'EPA01-0209' 'E' 'RT' 'WALL' '0'
              'EPA01-0209' 'E' 'RT' 'BUILD' '0'
              'EPA01-0209' 'E' 'RT' 'PAVE' '0'
              'EPA01-0209' 'E' 'RT' 'ROAD' '0'
              'EPA01-0209' 'E' 'RT' 'PIPES' '0'
              'EPA01-0209' 'E' 'RT' 'LANDFL' '0'
              'EPA01-0209' 'E' 'RT' 'PARK' '0'
              'EPA01-0209' 'E' 'RT' 'ROW' '0'
              'EPA01-0209' 'E' 'RT' 'PAST' '0'
              'EPA01-0209' 'E' 'RT' 'LOG' '0'
              'EPA01-0209' 'E' 'RT' 'MINE' '0'
              'EPA01-0209' 'F' 'LF' 'WALL' '0'
              'EPA01-0209' 'F' 'LF' 'BUILD' '0'
              'EPA01-0209' 'F' 'LF' 'PAVE' '0'
              'EPA01-0209' 'F' 'LF' 'ROAD' '0'
              'EPA01-0209' 'F' 'LF' 'PIPES' '0'
              'EPA01-0209' 'F' 'LF' 'LANDFL' '0'
              'EPA01-0209' 'F' 'LF' 'PARK' '0'
              'EPA01-0209' 'F' 'LF' 'ROW' '0'
              'EPA01-0209' 'F' 'LF' 'PAST' '0'
              'EPA01-0209' 'F' 'LF' 'LOG' '0'
              'EPA01-0209' 'F' 'LF' 'MINE' '0'
              'EPA01-0209' 'F' 'RT' 'WALL' '0'
              'EPA01-0209' 'F' 'RT' 'BUILD' '0'
              'EPA01-0209' 'F' 'RT' 'PAVE' '0'
              'EPA01-0209' 'F' 'RT' 'ROAD' '0'
              'EPA01-0209' 'F' 'RT' 'PIPES' '0'
              'EPA01-0209' 'F' 'RT' 'LANDFL' '0'
              'EPA01-0209' 'F' 'RT' 'PARK' '0'
              'EPA01-0209' 'F' 'RT' 'ROW' '0'
              'EPA01-0209' 'F' 'RT' 'PAST' '0'
              'EPA01-0209' 'F' 'RT' 'LOG' '0'
              'EPA01-0209' 'F' 'RT' 'MINE' '0'
              'EPA01-0209' 'G' 'LF' 'WALL' '0'
              'EPA01-0209' 'G' 'LF' 'BUILD' '0'
              'EPA01-0209' 'G' 'LF' 'PAVE' '0'
              'EPA01-0209' 'G' 'LF' 'ROAD' '0'
              'EPA01-0209' 'G' 'LF' 'PIPES' '0'
              'EPA01-0209' 'G' 'LF' 'LANDFL' '0'
              'EPA01-0209' 'G' 'LF' 'PARK' '0'
              'EPA01-0209' 'G' 'LF' 'ROW' '0'
              'EPA01-0209' 'G' 'LF' 'PAST' '0'
              'EPA01-0209' 'G' 'LF' 'LOG' '0'
              'EPA01-0209' 'G' 'LF' 'MINE' '0'
              'EPA01-0209' 'G' 'RT' 'WALL' '0'
              'EPA01-0209' 'G' 'RT' 'BUILD' '0'
              'EPA01-0209' 'G' 'RT' 'PAVE' '0'
              'EPA01-0209' 'G' 'RT' 'ROAD' '0'
              'EPA01-0209' 'G' 'RT' 'PIPES' '0'
              'EPA01-0209' 'G' 'RT' 'LANDFL' '0'
              'EPA01-0209' 'G' 'RT' 'PARK' '0'
              'EPA01-0209' 'G' 'RT' 'ROW' '0'
              'EPA01-0209' 'G' 'RT' 'PAST' '0'
              'EPA01-0209' 'G' 'RT' 'LOG' '0'
              'EPA01-0209' 'G' 'RT' 'MINE' '0'
              'EPA01-0209' 'H' 'LF' 'WALL' '0'
              'EPA01-0209' 'H' 'LF' 'BUILD' '0'
              'EPA01-0209' 'H' 'LF' 'PAVE' '0'
              'EPA01-0209' 'H' 'LF' 'ROAD' '0'
              'EPA01-0209' 'H' 'LF' 'PIPES' '0'
              'EPA01-0209' 'H' 'LF' 'LANDFL' '0'
              'EPA01-0209' 'H' 'LF' 'PARK' '0'
              'EPA01-0209' 'H' 'LF' 'ROW' '0'
              'EPA01-0209' 'H' 'LF' 'PAST' '0'
              'EPA01-0209' 'H' 'LF' 'LOG' '0'
              'EPA01-0209' 'H' 'LF' 'MINE' '0'
              'EPA01-0209' 'H' 'RT' 'WALL' '0'
              'EPA01-0209' 'H' 'RT' 'BUILD' '0'
              'EPA01-0209' 'H' 'RT' 'PAVE' '0'
              'EPA01-0209' 'H' 'RT' 'ROAD' 'P'
              'EPA01-0209' 'H' 'RT' 'PIPES' '0'
              'EPA01-0209' 'H' 'RT' 'LANDFL' '0'
              'EPA01-0209' 'H' 'RT' 'PARK' '0'
              'EPA01-0209' 'H' 'RT' 'ROW' '0'
              'EPA01-0209' 'H' 'RT' 'PAST' '0'
              'EPA01-0209' 'H' 'RT' 'LOG' '0'
              'EPA01-0209' 'H' 'RT' 'MINE' '0'
              'EPA01-0209' 'I' 'LF' 'WALL' '0'
              'EPA01-0209' 'I' 'LF' 'BUILD' '0'
              'EPA01-0209' 'I' 'LF' 'PAVE' '0'
              'EPA01-0209' 'I' 'LF' 'ROAD' '0'
              'EPA01-0209' 'I' 'LF' 'PIPES' '0'
              'EPA01-0209' 'I' 'LF' 'LANDFL' '0'
              'EPA01-0209' 'I' 'LF' 'PARK' '0'
              'EPA01-0209' 'I' 'LF' 'ROW' '0'
              'EPA01-0209' 'I' 'LF' 'PAST' '0'
              'EPA01-0209' 'I' 'LF' 'LOG' '0'
              'EPA01-0209' 'I' 'LF' 'MINE' '0'
              'EPA01-0209' 'I' 'RT' 'WALL' '0'
              'EPA01-0209' 'I' 'RT' 'BUILD' '0'
              'EPA01-0209' 'I' 'RT' 'PAVE' '0'
              'EPA01-0209' 'I' 'RT' 'ROAD' '0'
              'EPA01-0209' 'I' 'RT' 'PIPES' '0'
              'EPA01-0209' 'I' 'RT' 'LANDFL' '0'
              'EPA01-0209' 'I' 'RT' 'PARK' '0'
              'EPA01-0209' 'I' 'RT' 'ROW' '0'
              'EPA01-0209' 'I' 'RT' 'PAST' '0'
              'EPA01-0209' 'I' 'RT' 'LOG' '0'
              'EPA01-0209' 'I' 'RT' 'MINE' '0'
              'EPA01-0209' 'J' 'LF' 'WALL' '0'
              'EPA01-0209' 'J' 'LF' 'BUILD' '0'
              'EPA01-0209' 'J' 'LF' 'PAVE' '0'
              'EPA01-0209' 'J' 'LF' 'ROAD' '0'
              'EPA01-0209' 'J' 'LF' 'PIPES' '0'
              'EPA01-0209' 'J' 'LF' 'LANDFL' '0'
              'EPA01-0209' 'J' 'LF' 'PARK' '0'
              'EPA01-0209' 'J' 'LF' 'ROW' '0'
              'EPA01-0209' 'J' 'LF' 'PAST' '0'
              'EPA01-0209' 'J' 'LF' 'LOG' '0'
              'EPA01-0209' 'J' 'LF' 'MINE' '0'
              'EPA01-0209' 'J' 'RT' 'WALL' '0'
              'EPA01-0209' 'J' 'RT' 'BUILD' '0'
              'EPA01-0209' 'J' 'RT' 'PAVE' '0'
              'EPA01-0209' 'J' 'RT' 'ROAD' '0'
              'EPA01-0209' 'J' 'RT' 'PIPES' '0'
              'EPA01-0209' 'J' 'RT' 'LANDFL' '0'
              'EPA01-0209' 'J' 'RT' 'PARK' '0'
              'EPA01-0209' 'J' 'RT' 'ROW' '0'
              'EPA01-0209' 'J' 'RT' 'PAST' '0'
              'EPA01-0209' 'J' 'RT' 'LOG' '0'
              'EPA01-0209' 'J' 'RT' 'MINE' '0'
              'EPA01-0209' 'K' 'LF' 'WALL' '0'
              'EPA01-0209' 'K' 'LF' 'BUILD' '0'
              'EPA01-0209' 'K' 'LF' 'PAVE' '0'
              'EPA01-0209' 'K' 'LF' 'ROAD' '0'
              'EPA01-0209' 'K' 'LF' 'PIPES' '0'
              'EPA01-0209' 'K' 'LF' 'LANDFL' '0'
              'EPA01-0209' 'K' 'LF' 'PARK' '0'
              'EPA01-0209' 'K' 'LF' 'ROW' '0'
              'EPA01-0209' 'K' 'LF' 'PAST' '0'
              'EPA01-0209' 'K' 'LF' 'LOG' '0'
              'EPA01-0209' 'K' 'LF' 'MINE' '0'
              'EPA01-0209' 'K' 'RT' 'WALL' '0'
              'EPA01-0209' 'K' 'RT' 'BUILD' '0'
              'EPA01-0209' 'K' 'RT' 'PAVE' '0'
              'EPA01-0209' 'K' 'RT' 'ROAD' '0'
              'EPA01-0209' 'K' 'RT' 'PIPES' '0'
              'EPA01-0209' 'K' 'RT' 'LANDFL' '0'
              'EPA01-0209' 'K' 'RT' 'PARK' '0'
              'EPA01-0209' 'K' 'RT' 'ROW' '0'
              'EPA01-0209' 'K' 'RT' 'PAST' '0'
              'EPA01-0209' 'K' 'RT' 'LOG' '0'
              'EPA01-0209' 'K' 'RT' 'MINE' '0'
              'EPA01-0209' 'XA' 'LF' 'WALL' '0'
              'EPA01-0209' 'XA' 'LF' 'BUILD' '0'
              'EPA01-0209' 'XA' 'LF' 'PAVE' '0'
              'EPA01-0209' 'XA' 'LF' 'ROAD' '0'
              'EPA01-0209' 'XA' 'LF' 'PIPES' '0'
              'EPA01-0209' 'XA' 'LF' 'LANDFL' 'B'
              'EPA01-0209' 'XA' 'LF' 'PARK' '0'
              'EPA01-0209' 'XA' 'LF' 'ROW' '0'
              'EPA01-0209' 'XA' 'LF' 'PAST' '0'
              'EPA01-0209' 'XA' 'LF' 'LOG' '0'
              'EPA01-0209' 'XA' 'LF' 'MINE' '0'
              'EPA01-0209' 'XA' 'RT' 'WALL' '0'
              'EPA01-0209' 'XA' 'RT' 'BUILD' '0'
              'EPA01-0209' 'XA' 'RT' 'PAVE' '0'
              'EPA01-0209' 'XA' 'RT' 'ROAD' '0'
              'EPA01-0209' 'XA' 'RT' 'PIPES' '0'
              'EPA01-0209' 'XA' 'RT' 'LANDFL' '0'
              'EPA01-0209' 'XA' 'RT' 'PARK' '0'
              'EPA01-0209' 'XA' 'RT' 'ROW' '0'
              'EPA01-0209' 'XA' 'RT' 'PAST' '0'
              'EPA01-0209' 'XA' 'RT' 'LOG' '0'
              'EPA01-0209' 'XA' 'RT' 'MINE' '0'
              'EPA01-0210' 'A' 'LF' 'WALL' '0'
              'EPA01-0210' 'A' 'LF' 'BUILD' '0'
              'EPA01-0210' 'A' 'LF' 'PAVE' '0'
              'EPA01-0210' 'A' 'LF' 'ROAD' '0'
              'EPA01-0210' 'A' 'LF' 'PIPES' '0'
              'EPA01-0210' 'A' 'LF' 'LANDFL' '0'
              'EPA01-0210' 'A' 'LF' 'PARK' '0'
              'EPA01-0210' 'A' 'LF' 'ROW' '0'
              'EPA01-0210' 'A' 'LF' 'PAST' '0'
              'EPA01-0210' 'A' 'LF' 'LOG' '0'
              'EPA01-0210' 'A' 'LF' 'MINE' '0'
              'EPA01-0210' 'A' 'RT' 'WALL' '0'
              'EPA01-0210' 'A' 'RT' 'BUILD' '0'
              'EPA01-0210' 'A' 'RT' 'PAVE' '0'
              'EPA01-0210' 'A' 'RT' 'ROAD' '0'
              'EPA01-0210' 'A' 'RT' 'PIPES' '0'
              'EPA01-0210' 'A' 'RT' 'LANDFL' '0'
              'EPA01-0210' 'A' 'RT' 'PARK' '0'
              'EPA01-0210' 'A' 'RT' 'ROW' '0'
              'EPA01-0210' 'A' 'RT' 'PAST' '0'
              'EPA01-0210' 'A' 'RT' 'LOG' '0'
              'EPA01-0210' 'A' 'RT' 'MINE' '0'
              'EPA01-0210' 'B' 'LF' 'WALL' '0'
              'EPA01-0210' 'B' 'LF' 'BUILD' '0'
              'EPA01-0210' 'B' 'LF' 'PAVE' '0'
              'EPA01-0210' 'B' 'LF' 'ROAD' '0'
              'EPA01-0210' 'B' 'LF' 'PIPES' '0'
              'EPA01-0210' 'B' 'LF' 'LANDFL' 'P'
              'EPA01-0210' 'B' 'LF' 'PARK' '0'
              'EPA01-0210' 'B' 'LF' 'ROW' '0'
              'EPA01-0210' 'B' 'LF' 'PAST' '0'
              'EPA01-0210' 'B' 'LF' 'LOG' '0'
              'EPA01-0210' 'B' 'LF' 'MINE' '0'
              'EPA01-0210' 'B' 'RT' 'WALL' '0'
              'EPA01-0210' 'B' 'RT' 'BUILD' '0'
              'EPA01-0210' 'B' 'RT' 'PAVE' '0'
              'EPA01-0210' 'B' 'RT' 'ROAD' '0'
              'EPA01-0210' 'B' 'RT' 'PIPES' '0'
              'EPA01-0210' 'B' 'RT' 'LANDFL' '0'
              'EPA01-0210' 'B' 'RT' 'PARK' '0'
              'EPA01-0210' 'B' 'RT' 'ROW' '0'
              'EPA01-0210' 'B' 'RT' 'PAST' '0'
              'EPA01-0210' 'B' 'RT' 'LOG' '0'
              'EPA01-0210' 'B' 'RT' 'MINE' '0'
              'EPA01-0210' 'C' 'LF' 'WALL' '0'
              'EPA01-0210' 'C' 'LF' 'BUILD' '0'
              'EPA01-0210' 'C' 'LF' 'PAVE' '0'
              'EPA01-0210' 'C' 'LF' 'ROAD' '0'
              'EPA01-0210' 'C' 'LF' 'PIPES' '0'
              'EPA01-0210' 'C' 'LF' 'LANDFL' '0'
              'EPA01-0210' 'C' 'LF' 'PARK' '0'
              'EPA01-0210' 'C' 'LF' 'ROW' '0'
              'EPA01-0210' 'C' 'LF' 'PAST' '0'
              'EPA01-0210' 'C' 'LF' 'LOG' '0'
              'EPA01-0210' 'C' 'LF' 'MINE' '0'
              'EPA01-0210' 'C' 'RT' 'WALL' '0'
              'EPA01-0210' 'C' 'RT' 'BUILD' '0'
              'EPA01-0210' 'C' 'RT' 'PAVE' '0'
              'EPA01-0210' 'C' 'RT' 'ROAD' '0'
              'EPA01-0210' 'C' 'RT' 'PIPES' '0'
              'EPA01-0210' 'C' 'RT' 'LANDFL' '0'
              'EPA01-0210' 'C' 'RT' 'PARK' '0'
              'EPA01-0210' 'C' 'RT' 'ROW' '0'
              'EPA01-0210' 'C' 'RT' 'PAST' '0'
              'EPA01-0210' 'C' 'RT' 'LOG' '0'
              'EPA01-0210' 'C' 'RT' 'MINE' '0'
              'EPA01-0210' 'D' 'LF' 'WALL' '0'
              'EPA01-0210' 'D' 'LF' 'BUILD' '0'
              'EPA01-0210' 'D' 'LF' 'PAVE' '0'
              'EPA01-0210' 'D' 'LF' 'ROAD' '0'
              'EPA01-0210' 'D' 'LF' 'PIPES' '0'
              'EPA01-0210' 'D' 'LF' 'LANDFL' '0'
              'EPA01-0210' 'D' 'LF' 'PARK' '0'
              'EPA01-0210' 'D' 'LF' 'ROW' '0'
              'EPA01-0210' 'D' 'LF' 'PAST' '0'
              'EPA01-0210' 'D' 'LF' 'LOG' '0'
              'EPA01-0210' 'D' 'LF' 'MINE' '0'
              'EPA01-0210' 'D' 'RT' 'WALL' '0'
              'EPA01-0210' 'D' 'RT' 'BUILD' '0'
              'EPA01-0210' 'D' 'RT' 'PAVE' '0'
              'EPA01-0210' 'D' 'RT' 'ROAD' '0'
              'EPA01-0210' 'D' 'RT' 'PIPES' '0'
              'EPA01-0210' 'D' 'RT' 'LANDFL' '0'
              'EPA01-0210' 'D' 'RT' 'PARK' '0'
              'EPA01-0210' 'D' 'RT' 'ROW' '0'
              'EPA01-0210' 'D' 'RT' 'PAST' '0'
              'EPA01-0210' 'D' 'RT' 'LOG' '0'
              'EPA01-0210' 'D' 'RT' 'MINE' '0'
              'EPA01-0210' 'E' 'LF' 'WALL' '0'
              'EPA01-0210' 'E' 'LF' 'BUILD' '0'
              'EPA01-0210' 'E' 'LF' 'PAVE' '0'
              'EPA01-0210' 'E' 'LF' 'ROAD' '0'
              'EPA01-0210' 'E' 'LF' 'PIPES' '0'
              'EPA01-0210' 'E' 'LF' 'LANDFL' '0'
              'EPA01-0210' 'E' 'LF' 'PARK' '0'
              'EPA01-0210' 'E' 'LF' 'ROW' '0'
              'EPA01-0210' 'E' 'LF' 'PAST' '0'
              'EPA01-0210' 'E' 'LF' 'LOG' '0'
              'EPA01-0210' 'E' 'LF' 'MINE' '0'
              'EPA01-0210' 'E' 'RT' 'WALL' '0'
              'EPA01-0210' 'E' 'RT' 'BUILD' '0'
              'EPA01-0210' 'E' 'RT' 'PAVE' '0'
              'EPA01-0210' 'E' 'RT' 'ROAD' '0'
              'EPA01-0210' 'E' 'RT' 'PIPES' '0'
              'EPA01-0210' 'E' 'RT' 'LANDFL' '0'
              'EPA01-0210' 'E' 'RT' 'PARK' '0'
              'EPA01-0210' 'E' 'RT' 'ROW' '0'
              'EPA01-0210' 'E' 'RT' 'PAST' '0'
              'EPA01-0210' 'E' 'RT' 'LOG' '0'
              'EPA01-0210' 'E' 'RT' 'MINE' '0'
              'EPA01-0210' 'F' 'LF' 'WALL' '0'
              'EPA01-0210' 'F' 'LF' 'BUILD' '0'
              'EPA01-0210' 'F' 'LF' 'PAVE' '0'
              'EPA01-0210' 'F' 'LF' 'ROAD' '0'
              'EPA01-0210' 'F' 'LF' 'PIPES' '0'
              'EPA01-0210' 'F' 'LF' 'LANDFL' '0'
              'EPA01-0210' 'F' 'LF' 'PARK' '0'
              'EPA01-0210' 'F' 'LF' 'ROW' '0'
              'EPA01-0210' 'F' 'LF' 'PAST' '0'
              'EPA01-0210' 'F' 'LF' 'LOG' '0'
              'EPA01-0210' 'F' 'LF' 'MINE' '0'
              'EPA01-0210' 'F' 'RT' 'WALL' '0'
              'EPA01-0210' 'F' 'RT' 'BUILD' '0'
              'EPA01-0210' 'F' 'RT' 'PAVE' '0'
              'EPA01-0210' 'F' 'RT' 'ROAD' '0'
              'EPA01-0210' 'F' 'RT' 'PIPES' '0'
              'EPA01-0210' 'F' 'RT' 'LANDFL' '0'
              'EPA01-0210' 'F' 'RT' 'PARK' '0'
              'EPA01-0210' 'F' 'RT' 'ROW' '0'
              'EPA01-0210' 'F' 'RT' 'PAST' '0'
              'EPA01-0210' 'F' 'RT' 'LOG' '0'
              'EPA01-0210' 'F' 'RT' 'MINE' '0'
              'EPA01-0210' 'G' 'LF' 'WALL' '0'
              'EPA01-0210' 'G' 'LF' 'BUILD' '0'
              'EPA01-0210' 'G' 'LF' 'PAVE' '0'
              'EPA01-0210' 'G' 'LF' 'ROAD' '0'
              'EPA01-0210' 'G' 'LF' 'PIPES' '0'
              'EPA01-0210' 'G' 'LF' 'LANDFL' '0'
              'EPA01-0210' 'G' 'LF' 'PARK' '0'
              'EPA01-0210' 'G' 'LF' 'ROW' '0'
              'EPA01-0210' 'G' 'LF' 'PAST' '0'
              'EPA01-0210' 'G' 'LF' 'LOG' '0'
              'EPA01-0210' 'G' 'LF' 'MINE' '0'
              'EPA01-0210' 'G' 'RT' 'WALL' '0'
              'EPA01-0210' 'G' 'RT' 'BUILD' '0'
              'EPA01-0210' 'G' 'RT' 'PAVE' '0'
              'EPA01-0210' 'G' 'RT' 'ROAD' '0'
              'EPA01-0210' 'G' 'RT' 'PIPES' '0'
              'EPA01-0210' 'G' 'RT' 'LANDFL' '0'
              'EPA01-0210' 'G' 'RT' 'PARK' '0'
              'EPA01-0210' 'G' 'RT' 'ROW' '0'
              'EPA01-0210' 'G' 'RT' 'PAST' '0'
              'EPA01-0210' 'G' 'RT' 'LOG' '0'
              'EPA01-0210' 'G' 'RT' 'MINE' '0'
              'EPA01-0210' 'H' 'LF' 'WALL' '0'
              'EPA01-0210' 'H' 'LF' 'BUILD' '0'
              'EPA01-0210' 'H' 'LF' 'PAVE' '0'
              'EPA01-0210' 'H' 'LF' 'ROAD' '0'
              'EPA01-0210' 'H' 'LF' 'PIPES' '0'
              'EPA01-0210' 'H' 'LF' 'LANDFL' '0'
              'EPA01-0210' 'H' 'LF' 'PARK' '0'
              'EPA01-0210' 'H' 'LF' 'ROW' '0'
              'EPA01-0210' 'H' 'LF' 'PAST' '0'
              'EPA01-0210' 'H' 'LF' 'LOG' '0'
              'EPA01-0210' 'H' 'LF' 'MINE' '0'
              'EPA01-0210' 'H' 'RT' 'WALL' '0'
              'EPA01-0210' 'H' 'RT' 'BUILD' '0'
              'EPA01-0210' 'H' 'RT' 'PAVE' '0'
              'EPA01-0210' 'H' 'RT' 'ROAD' '0'
              'EPA01-0210' 'H' 'RT' 'PIPES' '0'
              'EPA01-0210' 'H' 'RT' 'LANDFL' '0'
              'EPA01-0210' 'H' 'RT' 'PARK' '0'
              'EPA01-0210' 'H' 'RT' 'ROW' '0'
              'EPA01-0210' 'H' 'RT' 'PAST' '0'
              'EPA01-0210' 'H' 'RT' 'LOG' '0'
              'EPA01-0210' 'H' 'RT' 'MINE' '0'
              'EPA01-0210' 'I' 'LF' 'WALL' '0'
              'EPA01-0210' 'I' 'LF' 'BUILD' '0'
              'EPA01-0210' 'I' 'LF' 'PAVE' '0'
              'EPA01-0210' 'I' 'LF' 'ROAD' '0'
              'EPA01-0210' 'I' 'LF' 'PIPES' '0'
              'EPA01-0210' 'I' 'LF' 'LANDFL' '0'
              'EPA01-0210' 'I' 'LF' 'PARK' '0'
              'EPA01-0210' 'I' 'LF' 'ROW' '0'
              'EPA01-0210' 'I' 'LF' 'PAST' '0'
              'EPA01-0210' 'I' 'LF' 'LOG' '0'
              'EPA01-0210' 'I' 'LF' 'MINE' '0'
              'EPA01-0210' 'I' 'RT' 'WALL' '0'
              'EPA01-0210' 'I' 'RT' 'BUILD' '0'
              'EPA01-0210' 'I' 'RT' 'PAVE' '0'
              'EPA01-0210' 'I' 'RT' 'ROAD' '0'
              'EPA01-0210' 'I' 'RT' 'PIPES' '0'
              'EPA01-0210' 'I' 'RT' 'LANDFL' '0'
              'EPA01-0210' 'I' 'RT' 'PARK' '0'
              'EPA01-0210' 'I' 'RT' 'ROW' '0'
              'EPA01-0210' 'I' 'RT' 'PAST' '0'
              'EPA01-0210' 'I' 'RT' 'LOG' '0'
              'EPA01-0210' 'I' 'RT' 'MINE' '0'
              'EPA01-0210' 'J' 'LF' 'WALL' '0'
              'EPA01-0210' 'J' 'LF' 'BUILD' '0'
              'EPA01-0210' 'J' 'LF' 'PAVE' '0'
              'EPA01-0210' 'J' 'LF' 'ROAD' '0'
              'EPA01-0210' 'J' 'LF' 'PIPES' '0'
              'EPA01-0210' 'J' 'LF' 'LANDFL' '0'
              'EPA01-0210' 'J' 'LF' 'PARK' '0'
              'EPA01-0210' 'J' 'LF' 'ROW' '0'
              'EPA01-0210' 'J' 'LF' 'PAST' '0'
              'EPA01-0210' 'J' 'LF' 'LOG' '0'
              'EPA01-0210' 'J' 'LF' 'MINE' '0'
              'EPA01-0210' 'J' 'RT' 'WALL' '0'
              'EPA01-0210' 'J' 'RT' 'BUILD' '0'
              'EPA01-0210' 'J' 'RT' 'PAVE' '0'
              'EPA01-0210' 'J' 'RT' 'ROAD' '0'
              'EPA01-0210' 'J' 'RT' 'PIPES' '0'
              'EPA01-0210' 'J' 'RT' 'LANDFL' '0'
              'EPA01-0210' 'J' 'RT' 'PARK' '0'
              'EPA01-0210' 'J' 'RT' 'ROW' '0'
              'EPA01-0210' 'J' 'RT' 'PAST' '0'
              'EPA01-0210' 'J' 'RT' 'LOG' '0'
              'EPA01-0210' 'J' 'RT' 'MINE' '0'
              'EPA01-0210' 'K' 'LF' 'WALL' '0'
              'EPA01-0210' 'K' 'LF' 'BUILD' '0'
              'EPA01-0210' 'K' 'LF' 'PAVE' '0'
              'EPA01-0210' 'K' 'LF' 'ROAD' '0'
              'EPA01-0210' 'K' 'LF' 'PIPES' '0'
              'EPA01-0210' 'K' 'LF' 'LANDFL' '0'
              'EPA01-0210' 'K' 'LF' 'PARK' '0'
              'EPA01-0210' 'K' 'LF' 'ROW' '0'
              'EPA01-0210' 'K' 'LF' 'PAST' '0'
              'EPA01-0210' 'K' 'LF' 'LOG' '0'
              'EPA01-0210' 'K' 'LF' 'MINE' '0'
              'EPA01-0210' 'K' 'RT' 'WALL' '0'
              'EPA01-0210' 'K' 'RT' 'BUILD' '0'
              'EPA01-0210' 'K' 'RT' 'PAVE' '0'
              'EPA01-0210' 'K' 'RT' 'ROAD' '0'
              'EPA01-0210' 'K' 'RT' 'PIPES' '0'
              'EPA01-0210' 'K' 'RT' 'LANDFL' '0'
              'EPA01-0210' 'K' 'RT' 'PARK' '0'
              'EPA01-0210' 'K' 'RT' 'ROW' '0'
              'EPA01-0210' 'K' 'RT' 'PAST' '0'
              'EPA01-0210' 'K' 'RT' 'LOG' '0'
              'EPA01-0210' 'K' 'RT' 'MINE' '0'
              "
              )
  testData <-read.table(fred, header=TRUE, stringsAsFactors=FALSE)
  close(fred)

  return(testData)
}

metsHumanInfluence.createResults <- function()
# creates dataframe of expected calculation results for unit test
{
  bob <- textConnection(
              "UID METRIC RESULT
              'EPA01-0159' 'sdb_hall' 0
              'EPA01-0159' 'sdc_hall' 0.301511345
              'EPA01-0159' 'sdcb_hall' 0.301511345
              'EPA01-0159' 'sdwcb_hall' 0.301511345
              'EPA01-0159' 'w1_hag' 0.621240909
              'EPA01-0159' 'w1_hall' 0.621240909
              'EPA01-0159' 'w1_hnoag' 0
              'EPA01-0159' 'w1h_bldg' 0
              'EPA01-0159' 'w1h_crop' 0
              'EPA01-0159' 'w1h_ldfl' 0
              'EPA01-0159' 'w1h_log' 0
              'EPA01-0159' 'w1h_mine' 0
              'EPA01-0159' 'w1h_park' 0
              'EPA01-0159' 'w1h_pipe' 0
              'EPA01-0159' 'w1h_pstr' 0.621240909
              'EPA01-0159' 'w1h_pvmt' 0
              'EPA01-0159' 'w1h_road' 0
              'EPA01-0159' 'w1h_wall' 0
              'EPA01-0159' 'x_hag' 0.909090909
              'EPA01-0159' 'x_hall' 0.909090909
              'EPA01-0159' 'x_hnoag' 0
              'EPA01-0159' 'xb_hag' 0
              'EPA01-0159' 'xb_hall' 0
              'EPA01-0159' 'xb_hnoag' 0
              'EPA01-0159' 'xc_hag' 0.045454545
              'EPA01-0159' 'xc_hall' 0.045454545
              'EPA01-0159' 'xc_hnoag' 0
              'EPA01-0159' 'xcb_hag' 0.045454545
              'EPA01-0159' 'xcb_hall' 0.045454545
              'EPA01-0159' 'xcb_hnag' 0
              'EPA01-0159' 'xf_hag' 0.863636364
              'EPA01-0159' 'xf_hall' 0.863636364
              'EPA01-0159' 'xf_hnoag' 0
              'EPA01-0161' 'sdb_hall' 0.301511345
              'EPA01-0161' 'sdc_hall' 0
              'EPA01-0161' 'sdcb_hall' 0.301511345
              'EPA01-0161' 'sdwcb_hall' 0.452267017
              'EPA01-0161' 'w1_hag' 0.575786364
              'EPA01-0161' 'w1_hall' 0.643968182
              'EPA01-0161' 'w1_hnoag' 0.068181818
              'EPA01-0161' 'w1h_bldg' 0
              'EPA01-0161' 'w1h_crop' 0.575786364
              'EPA01-0161' 'w1h_ldfl' 0.068181818
              'EPA01-0161' 'w1h_log' 0
              'EPA01-0161' 'w1h_mine' 0
              'EPA01-0161' 'w1h_park' 0
              'EPA01-0161' 'w1h_pipe' 0
              'EPA01-0161' 'w1h_pstr' 0
              'EPA01-0161' 'w1h_pvmt' 0
              'EPA01-0161' 'w1h_road' 0
              'EPA01-0161' 'w1h_wall' 0
              'EPA01-0161' 'x_hag' 0.863636364
              'EPA01-0161' 'x_hall' 0.909090909
              'EPA01-0161' 'x_hnoag' 0.045454545
              'EPA01-0161' 'xb_hag' 0
              'EPA01-0161' 'xb_hall' 0.045454545
              'EPA01-0161' 'xb_hnoag' 0.045454545
              'EPA01-0161' 'xc_hag' 0
              'EPA01-0161' 'xc_hall' 0
              'EPA01-0161' 'xc_hnoag' 0
              'EPA01-0161' 'xcb_hag' 0
              'EPA01-0161' 'xcb_hall' 0.045454545
              'EPA01-0161' 'xcb_hnag' 0.045454545
              'EPA01-0161' 'xf_hag' 0.863636364
              'EPA01-0161' 'xf_hall' 0.863636364
              'EPA01-0161' 'xf_hnoag' 0
              'EPA01-0167' 'sdb_hall' 0
              'EPA01-0167' 'sdc_hall' 0
              'EPA01-0167' 'sdcb_hall' 0
              'EPA01-0167' 'sdwcb_hall' 0
              'EPA01-0167' 'w1_hag' 0
              'EPA01-0167' 'w1_hall' 0
              'EPA01-0167' 'w1_hnoag' 0
              'EPA01-0167' 'w1h_bldg' 0
              'EPA01-0167' 'w1h_crop' 0
              'EPA01-0167' 'w1h_ldfl' 0
              'EPA01-0167' 'w1h_log' 0
              'EPA01-0167' 'w1h_mine' 0
              'EPA01-0167' 'w1h_park' 0
              'EPA01-0167' 'w1h_pipe' 0
              'EPA01-0167' 'w1h_pstr' 0
              'EPA01-0167' 'w1h_pvmt' 0
              'EPA01-0167' 'w1h_road' 0
              'EPA01-0167' 'w1h_wall' 0
              'EPA01-0167' 'x_hag' 0
              'EPA01-0167' 'x_hall' 0
              'EPA01-0167' 'x_hnoag' 0
              'EPA01-0167' 'xb_hag' 0
              'EPA01-0167' 'xb_hall' 0
              'EPA01-0167' 'xb_hnoag' 0
              'EPA01-0167' 'xc_hag' 0
              'EPA01-0167' 'xc_hall' 0
              'EPA01-0167' 'xc_hnoag' 0
              'EPA01-0167' 'xcb_hag' 0
              'EPA01-0167' 'xcb_hall' 0
              'EPA01-0167' 'xcb_hnag' 0
              'EPA01-0167' 'xf_hag' 0
              'EPA01-0167' 'xf_hall' 0
              'EPA01-0167' 'xf_hnoag' 0
              'EPA01-0209' 'sdb_hall' 1
              'EPA01-0209' 'sdc_hall' 0
              'EPA01-0209' 'sdcb_hall' 1
              'EPA01-0209' 'sdwcb_hall' 1.5
              'EPA01-0209' 'w1_hag' 0.0625
              'EPA01-0209' 'w1_hall' 0.430558333
              'EPA01-0209' 'w1_hnoag' 0.368058333
              'EPA01-0209' 'w1h_bldg' 0
              'EPA01-0209' 'w1h_crop' 0
              'EPA01-0209' 'w1h_ldfl' 0.0625
              'EPA01-0209' 'w1h_log' 0
              'EPA01-0209' 'w1h_mine' 0
              'EPA01-0209' 'w1h_park' 0
              'EPA01-0209' 'w1h_pipe' 0
              'EPA01-0209' 'w1h_pstr' 0.0625
              'EPA01-0209' 'w1h_pvmt' 0
              'EPA01-0209' 'w1h_road' 0.305558333
              'EPA01-0209' 'w1h_wall' 0
              'EPA01-0209' 'x_hag' 0.041666667
              'EPA01-0209' 'x_hall' 0.333333333
              'EPA01-0209' 'x_hnoag' 0.291666667
              'EPA01-0209' 'xb_hag' 0.041666667
              'EPA01-0209' 'xb_hall' 0.25
              'EPA01-0209' 'xb_hnoag' 0.208333333
              'EPA01-0209' 'xc_hag' 0
              'EPA01-0209' 'xc_hall' 0
              'EPA01-0209' 'xc_hnoag' 0
              'EPA01-0209' 'xcb_hag' 0.041666667
              'EPA01-0209' 'xcb_hall' 0.25
              'EPA01-0209' 'xcb_hnag' 0.208333333
              'EPA01-0209' 'xf_hag' 0
              'EPA01-0209' 'xf_hall' 0.083333333
              'EPA01-0209' 'xf_hnoag' 0.083333333
              'EPA01-0210' 'sdb_hall' 0
              'EPA01-0210' 'sdc_hall' 0
              'EPA01-0210' 'sdcb_hall' 0
              'EPA01-0210' 'sdwcb_hall' 0
              'EPA01-0210' 'w1_hag' 0
              'EPA01-0210' 'w1_hall' 0.030304545
              'EPA01-0210' 'w1_hnoag' 0.030304545
              'EPA01-0210' 'w1h_bldg' 0
              'EPA01-0210' 'w1h_crop' 0
              'EPA01-0210' 'w1h_ldfl' 0.030304545
              'EPA01-0210' 'w1h_log' 0
              'EPA01-0210' 'w1h_mine' 0
              'EPA01-0210' 'w1h_park' 0
              'EPA01-0210' 'w1h_pipe' 0
              'EPA01-0210' 'w1h_pstr' 0
              'EPA01-0210' 'w1h_pvmt' 0
              'EPA01-0210' 'w1h_road' 0
              'EPA01-0210' 'w1h_wall' 0
              'EPA01-0210' 'x_hag' 0
              'EPA01-0210' 'x_hall' 0.045454545
              'EPA01-0210' 'x_hnoag' 0.045454545
              'EPA01-0210' 'xb_hag' 0
              'EPA01-0210' 'xb_hall' 0
              'EPA01-0210' 'xb_hnoag' 0
              'EPA01-0210' 'xc_hag' 0
              'EPA01-0210' 'xc_hall' 0
              'EPA01-0210' 'xc_hnoag' 0
              'EPA01-0210' 'xcb_hag' 0
              'EPA01-0210' 'xcb_hall' 0
              'EPA01-0210' 'xcb_hnag' 0
              'EPA01-0210' 'xf_hag' 0
              'EPA01-0210' 'xf_hall' 0.045454545
              'EPA01-0210' 'xf_hnoag' 0.045454545
              "
  )

  testResults <-read.table(bob, header=TRUE, stringsAsFactors=FALSE)
  close(bob)

  return(testResults)
}