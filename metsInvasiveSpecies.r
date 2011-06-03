## metsInvasiveSpecies.r
##
## Created SSR 01/29/2010
## 03/23/10 cws moved creation of test dataframes into separate functions
#   3/25/10 cws Changed diff() calls to dfCompare().
#   6/24/10 cws Modified to handle odd case when transect has NO_INVASIVES
#           marked as well as an invasive species marked.  This shouldn't,
#           in the hopeful sense, make it through QA but we can assume a
#           species marked as present actually is present and safely ignore
#           the NO_INVASIVES check in these cases.

metsInvasiveSpecies <- function()
## Creating mhinplnt.csv which contains invasive species metrics:
## f_myrspi = count of E_WTRMILF/count of transects
## f_hydver = count of HYDRILLA/count of transects
## f_eiccra = count of W_HYACINTH/count of transects
## f_nympel = count of YLW_FLTHEAR/count of transects
## f_lytsal = count of P_LSTRIFE/count of transects
## f_arudon = count of G_REED/count of transects
## f_butumb = count of FLWR_RUSH/count of transects
## f_tamspp = count of SALT_CED/count of transects
## f_rosmul = count of MF_ROSE/count of transects
## f_eupesu = count of SPURGE/count of transects
## f_none = count of NO_INVASIVES/count of transects
## ip_count = sum(f_myrspi, f_hydver, f_eiccra, f_nympel, f_lytsal, f_arudon,
##                f_butumb, f_tamspp, f_rosmul, f_eupesu)
{
                 
intermediateMessage('Invasive species metrics calculations', loc='start')
intermediateMessage('.1 Read in data', loc='end')

##  Reading in data from tblINVASIVELEGACY2
chan <- odbcConnect('NRSA2')
tblINVASIVELEGACY2 <- fetchNRSATable(chan,'tblINVASIVELEGACY2')
close(chan)
## Retataining invasive legacy data
df <- subset(tblINVASIVELEGACY2, PARAMETER %in% c('E_WTRMILF','HYDRILLA','W_HYACINTH',
             'YLW_FLTHEAR','P_LSTRIFE','G_REED','FLWR_RUSH','SALT_CED','MF_ROSE',
             'SPURGE','NO_INVASIVES'), select=c('UID','TRANSECT','PARAMETER','RESULT'))

## Calculate the metrics
intermediateMessage('.2 call function metsInvasiveSpecies.1', loc='end')
mets <- metsInvasiveSpecies.1(df)

## Write the results to 
intermediateMessage('.3 Write results', loc='end')
rc <- writeNRSACalcResults(mets, 'metsInvasiveSpecies.csv')
        
intermediateMessage('  Done.', loc='end')
return(rc)

}


metsInvasiveSpecies.1 <- function(df)
{
df$RESULT <- ifelse(df$RESULT=='X' | df$RESULT == 'Y', 1, df$RESULT)              
## Creating counts of individual invasive species
aa <- aggregate(list(RESULT=df$RESULT), by=list(UID=df$UID, METRIC=df$PARAMETER), count)

##Creating count of transects observed
bb <- subset(df, select=c('UID', 'TRANSECT'))
cc <- bb[!duplicated(bb),]
dd <- aggregate(list(tranCount=cc$TRANSECT), list(UID=cc$UID), count)
ee <- merge(aa,dd)

##  Creating metrics for individual species
## Because individual counts are not kept with the calculated metrics,
## we are changing the species name to the species metric name
ee$METRIC <- ifelse(ee$METRIC=='E_WTRMILF', 'f_myrspi', ee$METRIC)
ee$METRIC <- ifelse(ee$METRIC=='HYDRILLA', 'f_hydver', ee$METRIC)
ee$METRIC <- ifelse(ee$METRIC=='W_HYACINTH', 'f_eiccra', ee$METRIC)
ee$METRIC <- ifelse(ee$METRIC=='YLW_FLTHEAR', 'f_nympel', ee$METRIC)
ee$METRIC <- ifelse(ee$METRIC=='P_LSTRIFE', 'f_lytsal', ee$METRIC)
ee$METRIC <- ifelse(ee$METRIC=='G_REED', 'f_arudon', ee$METRIC)
ee$METRIC <- ifelse(ee$METRIC=='FLWR_RUSH', 'f_butumb', ee$METRIC)
ee$METRIC <- ifelse(ee$METRIC=='SALT_CED', 'f_tamspp', ee$METRIC)
ee$METRIC <- ifelse(ee$METRIC=='MF_ROSE', 'f_rosmul', ee$METRIC)
ee$METRIC <- ifelse(ee$METRIC=='SPURGE', 'f_eupesu', ee$METRIC)
ee$METRIC <- ifelse(ee$METRIC=='NO_INVASIVES', 'f_none', ee$METRIC)
## All invasive species metrics are number of observations within a site
## divided by the number of transects observed during site visit
ee$RESULT <- ee$RESULT/ee$tranCount
## Done with count of transects, deleting it
ee$tranCount <- NULL

## Calculating ip_score, which is the sum of all metrics of invasive species
## present.
ff <- aggregate(list(RESULT=ee[ee$METRIC != 'f_none',]$RESULT),
                     list(UID=ee[ee$METRIC != 'f_none',]$UID), sum)
ff$METRIC <- 'ip_score'
ff <- ff[,c('UID','METRIC','RESULT')]

## If there are no invasive species present, it will not have been calculated
## in the previous step, none must equal 1, and therefore ip_score must equal 0.
## This does not take into account transects in which NO_INVASIVES were checked
## along with an invasive.  This *should* be caught during QA, but if it isn't
## we'll assume the invasive actually exists and ignore the NO_INVASIVES flag.
gg <- subset(ee, METRIC=='f_none' & RESULT == 1)
gg$METRIC <- 'ip_score'
gg$RESULT <- 0
gg <- subset(gg, !(UID %in% ff$UID))


##  Creating final file -- indivual species metrics, ip_score when invasive
## species are present, and ip_score when no invasive species are present.
mhinplnt <- rbind(ee,ff,gg)
}

######################
## Testing using EMAP data
######################
metsInvasiveSpeciesTest <- function()
{
  ## Creating test data
  testData <- metsInvasiveSpecies.createData()
  testResults <- metsInvasiveSpecies.createMetrics()

  invTest <- metsInvasiveSpecies.1(testData)

  rMets <- invTest[order(invTest$UID,invTest$METRIC),]
  sasMets <- testResults[order(testResults$UID,testResults$METRIC),]

  errs <- dfCompare(rMets, sasMets, c('UID', 'METRIC'), zeroFudge=1e-9)
  checkEquals(NULL, errs, "Error: metsInvasiveSpecies is broken.")

}

metsInvasiveSpecies.createData <- function()
# creates dataframe of invasive species data for unit test
{
  fred <- textConnection(
              "UID TRANSECT PARAMETER RESULT
              'EPA01-0124' 'A' 'NO_INVASIVES' 'X'
              'EPA01-0124' 'B' 'NO_INVASIVES' 'X'
              'EPA01-0124' 'C' 'NO_INVASIVES' 'X'
              'EPA01-0124' 'D' 'NO_INVASIVES' 'X'
              'EPA01-0124' 'E' 'P_LSTRIFE' 'Y'
              'EPA01-0124' 'F' 'P_LSTRIFE' 'Y'
              'EPA01-0124' 'G' 'NO_INVASIVES' 'X'
              'EPA01-0124' 'H' 'NO_INVASIVES' 'X'
              'EPA01-0124' 'I' 'NO_INVASIVES' 'X'
              'EPA01-0124' 'J' 'NO_INVASIVES' 'X'
              'EPA01-0124' 'K' 'NO_INVASIVES' 'X'
              'EPA01-0144' 'A' 'NO_INVASIVES' 'X'
              'EPA01-0144' 'B' 'NO_INVASIVES' 'X'
              'EPA01-0144' 'C' 'NO_INVASIVES' 'X'
              'EPA01-0144' 'D' 'NO_INVASIVES' 'X'
              'EPA01-0144' 'E' 'NO_INVASIVES' 'X'
              'EPA01-0144' 'F' 'NO_INVASIVES' 'X'
              'EPA01-0144' 'G' 'NO_INVASIVES' 'X'
              'EPA01-0144' 'H' 'NO_INVASIVES' 'X'
              'EPA01-0144' 'I' 'NO_INVASIVES' 'X'
              'EPA01-0144' 'J' 'NO_INVASIVES' 'X'
              'EPA01-0144' 'K' 'NO_INVASIVES' 'X'
              'EPA01-0147' 'A' 'NO_INVASIVES' 'X'
              'EPA01-0147' 'B' 'NO_INVASIVES' 'X'
              'EPA01-0147' 'C' 'NO_INVASIVES' 'X'
              'EPA01-0147' 'D' 'NO_INVASIVES' 'X'
              'EPA01-0147' 'E' 'NO_INVASIVES' 'X'
              'EPA01-0147' 'F' 'NO_INVASIVES' 'X'
              'EPA01-0147' 'G' 'NO_INVASIVES' 'X'
              'EPA01-0147' 'H' 'NO_INVASIVES' 'X'
              'EPA01-0147' 'I' 'NO_INVASIVES' 'X'
              'EPA01-0147' 'J' 'NO_INVASIVES' 'X'
              'EPA01-0147' 'K' 'NO_INVASIVES' 'X'
              'EPA01-0159' 'A' 'P_LSTRIFE' 'Y'
              'EPA01-0159' 'B' 'P_LSTRIFE' 'Y'
              'EPA01-0159' 'C' 'W_HYACINTH' 'Y'
              'EPA01-0159' 'C' 'P_LSTRIFE' 'Y'
              'EPA01-0159' 'D' 'W_HYACINTH' 'Y'
              'EPA01-0159' 'D' 'P_LSTRIFE' 'Y'
              'EPA01-0159' 'E' 'W_HYACINTH' 'Y'
              'EPA01-0159' 'E' 'P_LSTRIFE' 'Y'
              'EPA01-0159' 'F' 'W_HYACINTH' 'Y'
              'EPA01-0159' 'F' 'P_LSTRIFE' 'Y'
              'EPA01-0159' 'G' 'W_HYACINTH' 'Y'
              'EPA01-0159' 'G' 'P_LSTRIFE' 'Y'
              'EPA01-0159' 'H' 'W_HYACINTH' 'Y'
              'EPA01-0159' 'H' 'P_LSTRIFE' 'Y'
              'EPA01-0159' 'I' 'W_HYACINTH' 'Y'
              'EPA01-0159' 'I' 'P_LSTRIFE' 'Y'
              'EPA01-0159' 'J' 'W_HYACINTH' 'Y'
              'EPA01-0159' 'J' 'P_LSTRIFE' 'Y'
              'EPA01-0159' 'K' 'W_HYACINTH' 'Y'
              'EPA01-0159' 'K' 'P_LSTRIFE' 'Y'
              'EPA01-0161' 'A' 'P_LSTRIFE' 'Y'
              'EPA01-0161' 'B' 'P_LSTRIFE' 'Y'
              'EPA01-0161' 'C' 'P_LSTRIFE' 'Y'
              'EPA01-0161' 'D' 'P_LSTRIFE' 'Y'
              'EPA01-0161' 'E' 'P_LSTRIFE' 'Y'
              'EPA01-0161' 'F' 'P_LSTRIFE' 'Y'
              'EPA01-0161' 'G' 'P_LSTRIFE' 'Y'
              'EPA01-0161' 'H' 'P_LSTRIFE' 'Y'
              'EPA01-0161' 'I' 'P_LSTRIFE' 'Y'
              'EPA01-0161' 'J' 'P_LSTRIFE' 'Y'
              'EPA01-0161' 'K' 'NO_INVASIVES' 'X'
              'EPA01-0167' 'A' 'NO_INVASIVES' 'X'
              'EPA01-0167' 'B' 'NO_INVASIVES' 'X'
              'EPA01-0167' 'C' 'NO_INVASIVES' 'X'
              'EPA01-0167' 'D' 'NO_INVASIVES' 'X'
              'EPA01-0167' 'E' 'NO_INVASIVES' 'X'
              'EPA01-0167' 'F' 'NO_INVASIVES' 'X'
              'EPA01-0167' 'G' 'NO_INVASIVES' 'X'
              'EPA01-0167' 'H' 'NO_INVASIVES' 'X'
              'EPA01-0167' 'I' 'NO_INVASIVES' 'X'
              'EPA01-0167' 'J' 'NO_INVASIVES' 'X'
              'EPA01-0167' 'K' 'NO_INVASIVES' 'X'
              'EPA01-0209' 'A' 'NO_INVASIVES' 'X'
              'EPA01-0209' 'B' 'NO_INVASIVES' 'X'
              'EPA01-0209' 'C' 'NO_INVASIVES' 'X'
              'EPA01-0209' 'D' 'NO_INVASIVES' 'X'
              'EPA01-0209' 'E' 'NO_INVASIVES' 'X'
              'EPA01-0209' 'F' 'NO_INVASIVES' 'X'
              'EPA01-0209' 'G' 'NO_INVASIVES' 'X'
              'EPA01-0209' 'H' 'NO_INVASIVES' 'X'
              'EPA01-0209' 'I' 'NO_INVASIVES' 'X'
              'EPA01-0209' 'J' 'NO_INVASIVES' 'X'
              'EPA01-0209' 'K' 'NO_INVASIVES' 'X'
              'EPA01-0210' 'A' 'NO_INVASIVES' 'X'
              'EPA01-0210' 'B' 'NO_INVASIVES' 'X'
              'EPA01-0210' 'C' 'NO_INVASIVES' 'X'
              'EPA01-0210' 'D' 'NO_INVASIVES' 'X'
              'EPA01-0210' 'E' 'NO_INVASIVES' 'X'
              'EPA01-0210' 'F' 'NO_INVASIVES' 'X'
              'EPA01-0210' 'G' 'NO_INVASIVES' 'X'
              'EPA01-0210' 'H' 'NO_INVASIVES' 'X'
              'EPA01-0210' 'I' 'NO_INVASIVES' 'X'
              'EPA01-0210' 'J' 'NO_INVASIVES' 'X'
              'EPA01-0210' 'K' 'NO_INVASIVES' 'X'
              'EPA01-0212' 'A' 'NO_INVASIVES' 'X'
              'EPA01-0212' 'B' 'NO_INVASIVES' 'X'
              'EPA01-0212' 'C' 'NO_INVASIVES' 'X'
              'EPA01-0212' 'D' 'NO_INVASIVES' 'X'
              'EPA01-0212' 'E' 'NO_INVASIVES' 'X'
              'EPA01-0212' 'F' 'NO_INVASIVES' 'X'
              'EPA01-0212' 'G' 'NO_INVASIVES' 'X'
              'EPA01-0212' 'H' 'NO_INVASIVES' 'X'
              'EPA01-0212' 'I' 'NO_INVASIVES' 'X'
              'EPA01-0212' 'J' 'NO_INVASIVES' 'X'
              'EPA01-0212' 'K' 'NO_INVASIVES' 'X'
              'EPA01-0214' 'A' 'NO_INVASIVES' 'X'
              'EPA01-0214' 'B' 'NO_INVASIVES' 'X'
              'EPA01-0214' 'C' 'NO_INVASIVES' 'X'
              'EPA01-0214' 'D' 'NO_INVASIVES' 'X'
              'EPA01-0214' 'E' 'NO_INVASIVES' 'X'
              'EPA01-0214' 'F' 'NO_INVASIVES' 'X'
              'EPA01-0214' 'G' 'NO_INVASIVES' 'X'
              'EPA01-0214' 'H' 'NO_INVASIVES' 'X'
              'EPA01-0214' 'I' 'NO_INVASIVES' 'X'
              'EPA01-0214' 'J' 'NO_INVASIVES' 'X'
              'EPA01-0214' 'K' 'NO_INVASIVES' 'X'
              'EPA01-0215' 'A' 'NO_INVASIVES' 'X'
              'EPA01-0215' 'B' 'NO_INVASIVES' 'X'
              'EPA01-0215' 'C' 'NO_INVASIVES' 'X'
              'EPA01-0215' 'D' 'NO_INVASIVES' 'X'
              'EPA01-0215' 'E' 'NO_INVASIVES' 'X'
              'EPA01-0215' 'F' 'NO_INVASIVES' 'X'
              'EPA01-0215' 'G' 'NO_INVASIVES' 'X'
              'EPA01-0215' 'H' 'NO_INVASIVES' 'X'
              'EPA01-0215' 'I' 'NO_INVASIVES' 'X'
              'EPA01-0215' 'J' 'NO_INVASIVES' 'X'
              'EPA01-0215' 'K' 'NO_INVASIVES' 'X'
              'EPA01-0238' 'A' 'W_HYACINTH' 'Y'
              'EPA01-0238' 'A' 'FLWR_RUSH' 'Y'
              'EPA01-0238' 'B' 'W_HYACINTH' 'Y'
              'EPA01-0238' 'B' 'FLWR_RUSH' 'Y'
              'EPA01-0238' 'C' 'W_HYACINTH' 'Y'
              'EPA01-0238' 'C' 'FLWR_RUSH' 'Y'
              'EPA01-0238' 'D' 'W_HYACINTH' 'Y'
              'EPA01-0238' 'D' 'FLWR_RUSH' 'Y'
              'EPA01-0238' 'E' 'W_HYACINTH' 'Y'
              'EPA01-0238' 'E' 'FLWR_RUSH' 'Y'
              'EPA01-0238' 'F' 'W_HYACINTH' 'Y'
              'EPA01-0238' 'F' 'FLWR_RUSH' 'Y'
              'EPA01-0238' 'G' 'W_HYACINTH' 'Y'
              'EPA01-0238' 'G' 'FLWR_RUSH' 'Y'
              'EPA01-0238' 'H' 'W_HYACINTH' 'Y'
              'EPA01-0238' 'H' 'FLWR_RUSH' 'Y'
              'EPA01-0238' 'I' 'W_HYACINTH' 'Y'
              'EPA01-0238' 'I' 'FLWR_RUSH' 'Y'
              'EPA01-0238' 'J' 'W_HYACINTH' 'Y'
              'EPA01-0238' 'J' 'FLWR_RUSH' 'Y'
              'EPA01-0238' 'K' 'W_HYACINTH' 'Y'
              'EPA01-0238' 'K' 'FLWR_RUSH' 'Y'
              'EPA01-0239' 'A' 'NO_INVASIVES' 'X'
              'EPA01-0239' 'B' 'NO_INVASIVES' 'X'
              'EPA01-0239' 'C' 'NO_INVASIVES' 'X'
              'EPA01-0239' 'D' 'NO_INVASIVES' 'X'
              'EPA01-0239' 'E' 'NO_INVASIVES' 'X'
              'EPA01-0239' 'F' 'NO_INVASIVES' 'X'
              'EPA01-0239' 'G' 'NO_INVASIVES' 'X'
              'EPA01-0239' 'H' 'NO_INVASIVES' 'X'
              'EPA01-0239' 'I' 'NO_INVASIVES' 'X'
              'EPA01-0239' 'J' 'NO_INVASIVES' 'X'
              'EPA01-0239' 'K' 'NO_INVASIVES' 'X'
              'EPA01-0240' 'A' 'NO_INVASIVES' 'X'
              'EPA01-0240' 'B' 'NO_INVASIVES' 'X'
              'EPA01-0240' 'C' 'NO_INVASIVES' 'X'
              'EPA01-0240' 'D' 'NO_INVASIVES' 'X'
              'EPA01-0240' 'E' 'NO_INVASIVES' 'X'
              'EPA01-0240' 'F' 'NO_INVASIVES' 'X'
              'EPA01-0240' 'G' 'NO_INVASIVES' 'X'
              'EPA01-0240' 'H' 'NO_INVASIVES' 'X'
              'EPA01-0240' 'I' 'NO_INVASIVES' 'X'
              'EPA01-0240' 'J' 'NO_INVASIVES' 'X'
              'EPA01-0240' 'K' 'NO_INVASIVES' 'X'
              'EPA01-0242' 'A' 'NO_INVASIVES' 'X'
              'EPA01-0242' 'B' 'NO_INVASIVES' 'X'
              'EPA01-0242' 'C' 'NO_INVASIVES' 'X'
              'EPA01-0242' 'D' 'NO_INVASIVES' 'X'
              'EPA01-0242' 'E' 'NO_INVASIVES' 'X'
              'EPA01-0242' 'F' 'NO_INVASIVES' 'X'
              'EPA01-0242' 'G' 'NO_INVASIVES' 'X'
              'EPA01-0242' 'H' 'NO_INVASIVES' 'X'
              'EPA01-0242' 'I' 'NO_INVASIVES' 'X'
              'EPA01-0242' 'J' 'NO_INVASIVES' 'X'
              'EPA01-0242' 'K' 'NO_INVASIVES' 'X'
              'EPA01-0246' 'A' 'NO_INVASIVES' 'X'
              'EPA01-0246' 'B' 'NO_INVASIVES' 'X'
              'EPA01-0246' 'C' 'NO_INVASIVES' 'X'
              'EPA01-0246' 'D' 'NO_INVASIVES' 'X'
              'EPA01-0246' 'E' 'NO_INVASIVES' 'X'
              'EPA01-0246' 'F' 'NO_INVASIVES' 'X'
              'EPA01-0246' 'G' 'NO_INVASIVES' 'X'
              'EPA01-0246' 'H' 'NO_INVASIVES' 'X'
              'EPA01-0246' 'I' 'NO_INVASIVES' 'X'
              'EPA01-0246' 'J' 'NO_INVASIVES' 'X'
              'EPA01-0246' 'K' 'NO_INVASIVES' 'X'
              'EPA01-0248' 'A' 'NO_INVASIVES' 'X'
              'EPA01-0248' 'B' 'NO_INVASIVES' 'X'
              'EPA01-0248' 'C' 'NO_INVASIVES' 'X'
              'EPA01-0248' 'D' 'NO_INVASIVES' 'X'
              'EPA01-0248' 'E' 'NO_INVASIVES' 'X'
              'EPA01-0248' 'F' 'NO_INVASIVES' 'X'
              'EPA01-0248' 'G' 'NO_INVASIVES' 'X'
              'EPA01-0248' 'H' 'NO_INVASIVES' 'X'
              'EPA01-0248' 'I' 'NO_INVASIVES' 'X'
              'EPA01-0248' 'J' 'NO_INVASIVES' 'X'
              'EPA01-0248' 'K' 'NO_INVASIVES' 'X'
              'EPA01-0302' 'A' 'NO_INVASIVES' 'X'
              'EPA01-0302' 'B' 'NO_INVASIVES' 'X'
              'EPA01-0302' 'C' 'NO_INVASIVES' 'X'
              'EPA01-0302' 'D' 'NO_INVASIVES' 'X'
              'EPA01-0302' 'E' 'NO_INVASIVES' 'X'
              'EPA01-0302' 'F' 'NO_INVASIVES' 'X'
              'EPA01-0302' 'G' 'NO_INVASIVES' 'X'
              'EPA01-0302' 'H' 'NO_INVASIVES' 'X'
              'EPA01-0302' 'I' 'NO_INVASIVES' 'X'
              'EPA01-0302' 'J' 'NO_INVASIVES' 'X'
              'EPA01-0302' 'K' 'NO_INVASIVES' 'X'
              'EPA01-0303' 'A' 'NO_INVASIVES' 'X'
              'EPA01-0303' 'B' 'NO_INVASIVES' 'X'
              'EPA01-0303' 'C' 'NO_INVASIVES' 'X'
              'EPA01-0303' 'D' 'NO_INVASIVES' 'X'
              'EPA01-0303' 'E' 'NO_INVASIVES' 'X'
              'EPA01-0303' 'F' 'NO_INVASIVES' 'X'
              'EPA01-0303' 'G' 'NO_INVASIVES' 'X'
              'EPA01-0303' 'H' 'NO_INVASIVES' 'X'
              'EPA01-0303' 'I' 'NO_INVASIVES' 'X'
              'EPA01-0303' 'J' 'NO_INVASIVES' 'X'
              'EPA01-0303' 'K' 'NO_INVASIVES' 'X'
              'EPA01-0305' 'A' 'NO_INVASIVES' 'X'
              'EPA01-0305' 'B' 'NO_INVASIVES' 'X'
              'EPA01-0305' 'C' 'NO_INVASIVES' 'X'
              'EPA01-0305' 'D' 'NO_INVASIVES' 'X'
              'EPA01-0305' 'E' 'NO_INVASIVES' 'X'
              'EPA01-0305' 'F' 'NO_INVASIVES' 'X'
              'EPA01-0305' 'G' 'NO_INVASIVES' 'X'
              'EPA01-0305' 'H' 'NO_INVASIVES' 'X'
              'EPA01-0305' 'I' 'NO_INVASIVES' 'X'
              'EPA01-0305' 'J' 'NO_INVASIVES' 'X'
              'EPA01-0305' 'K' 'NO_INVASIVES' 'X'
              'EPA01-0306' 'A' 'NO_INVASIVES' 'X'
              'EPA01-0306' 'B' 'NO_INVASIVES' 'X'
              'EPA01-0306' 'C' 'NO_INVASIVES' 'X'
              'EPA01-0306' 'D' 'NO_INVASIVES' 'X'
              'EPA01-0306' 'E' 'NO_INVASIVES' 'X'
              'EPA01-0306' 'F' 'NO_INVASIVES' 'X'
              'EPA01-0306' 'G' 'NO_INVASIVES' 'X'
              'EPA01-0306' 'H' 'FLWR_RUSH' 'Y'
              'EPA01-0306' 'I' 'FLWR_RUSH' 'Y'
              'EPA01-0306' 'J' 'FLWR_RUSH' 'Y'
              'EPA01-0306' 'K' 'NO_INVASIVES' 'X'
              'EPA01-0308' 'A' 'NO_INVASIVES' 'X'
              'EPA01-0308' 'B' 'NO_INVASIVES' 'X'
              'EPA01-0308' 'C' 'NO_INVASIVES' 'X'
              'EPA01-0308' 'D' 'NO_INVASIVES' 'X'
              'EPA01-0308' 'E' 'NO_INVASIVES' 'X'
              'EPA01-0308' 'F' 'NO_INVASIVES' 'X'
              'EPA01-0308' 'G' 'W_HYACINTH' 'Y'
              'EPA01-0308' 'H' 'NO_INVASIVES' 'X'
              'EPA01-0308' 'I' 'NO_INVASIVES' 'X'
              'EPA01-0308' 'J' 'NO_INVASIVES' 'X'
              'EPA01-0308' 'K' 'NO_INVASIVES' 'X'
              'EPA01-0314' 'A' 'W_HYACINTH' 'Y'
              'EPA01-0314' 'B' 'W_HYACINTH' 'Y'
              'EPA01-0314' 'C' 'W_HYACINTH' 'Y'
              'EPA01-0314' 'C' 'FLWR_RUSH' 'Y'
              'EPA01-0314' 'D' 'W_HYACINTH' 'Y'
              'EPA01-0314' 'E' 'W_HYACINTH' 'Y'
              'EPA01-0314' 'F' 'W_HYACINTH' 'Y'
              'EPA01-0314' 'F' 'FLWR_RUSH' 'Y'
              'EPA01-0314' 'G' 'W_HYACINTH' 'Y'
              'EPA01-0314' 'H' 'W_HYACINTH' 'Y'
              'EPA01-0314' 'I' 'W_HYACINTH' 'Y'
              'EPA01-0314' 'J' 'W_HYACINTH' 'Y'
              'EPA01-0314' 'K' 'W_HYACINTH' 'Y'
              'EPA01-0315' 'A' 'NO_INVASIVES' 'X'
              'EPA01-0315' 'B' 'NO_INVASIVES' 'X'
              'EPA01-0315' 'C' 'NO_INVASIVES' 'X'
              'EPA01-0315' 'D' 'NO_INVASIVES' 'X'
              'EPA01-0315' 'E' 'NO_INVASIVES' 'X'
              'EPA01-0315' 'F' 'NO_INVASIVES' 'X'
              'EPA01-0315' 'G' 'NO_INVASIVES' 'X'
              'EPA01-0315' 'H' 'NO_INVASIVES' 'X'
              'EPA01-0315' 'I' 'NO_INVASIVES' 'X'
              'EPA01-0315' 'J' 'NO_INVASIVES' 'X'
              'EPA01-0315' 'K' 'NO_INVASIVES' 'X'
              'EPA01-0421' 'A' 'NO_INVASIVES' 'X'
              'EPA01-0421' 'B' 'NO_INVASIVES' 'X'
              'EPA01-0421' 'C' 'NO_INVASIVES' 'X'
              'EPA01-0421' 'D' 'NO_INVASIVES' 'X'
              'EPA01-0421' 'E' 'NO_INVASIVES' 'X'
              'EPA01-0421' 'F' 'NO_INVASIVES' 'X'
              'EPA01-0421' 'G' 'NO_INVASIVES' 'X'
              'EPA01-0421' 'H' 'NO_INVASIVES' 'X'
              'EPA01-0421' 'I' 'NO_INVASIVES' 'X'
              'EPA01-0421' 'J' 'NO_INVASIVES' 'X'
              'EPA01-0421' 'K' 'NO_INVASIVES' 'X'
              'EPA01-0424' 'A' 'NO_INVASIVES' 'X'
              'EPA01-0424' 'B' 'NO_INVASIVES' 'X'
              'EPA01-0424' 'C' 'NO_INVASIVES' 'X'
              'EPA01-0424' 'D' 'NO_INVASIVES' 'X'
              'EPA01-0424' 'E' 'NO_INVASIVES' 'X'
              'EPA01-0424' 'F' 'NO_INVASIVES' 'X'
              'EPA01-0424' 'G' 'NO_INVASIVES' 'X'
              'EPA01-0424' 'H' 'NO_INVASIVES' 'X'
              'EPA01-0424' 'I' 'NO_INVASIVES' 'X'
              'EPA01-0424' 'J' 'NO_INVASIVES' 'X'
              'EPA01-0424' 'K' 'NO_INVASIVES' 'X'
              'EPA01-0432' 'A' 'NO_INVASIVES' 'X'
              'EPA01-0432' 'B' 'NO_INVASIVES' 'X'
              'EPA01-0432' 'C' 'NO_INVASIVES' 'X'
              'EPA01-0432' 'D' 'NO_INVASIVES' 'X'
              'EPA01-0432' 'E' 'NO_INVASIVES' 'X'
              'EPA01-0432' 'F' 'NO_INVASIVES' 'X'
              'EPA01-0432' 'G' 'NO_INVASIVES' 'X'
              'EPA01-0432' 'H' 'NO_INVASIVES' 'X'
              'EPA01-0432' 'I' 'NO_INVASIVES' 'X'
              'EPA01-0432' 'J' 'NO_INVASIVES' 'X'
              'EPA01-0432' 'K' 'NO_INVASIVES' 'X'
              'EPA01-0448' 'A' 'NO_INVASIVES' 'X'
              'EPA01-0448' 'B' 'NO_INVASIVES' 'X'
              'EPA01-0448' 'C' 'NO_INVASIVES' 'X'
              'EPA01-0448' 'D' 'NO_INVASIVES' 'X'
              'EPA01-0448' 'E' 'NO_INVASIVES' 'X'
              'EPA01-0448' 'F' 'NO_INVASIVES' 'X'
              'EPA01-0448' 'G' 'NO_INVASIVES' 'X'
              'EPA01-0448' 'H' 'NO_INVASIVES' 'X'
              'EPA01-0448' 'I' 'NO_INVASIVES' 'X'
              'EPA01-0448' 'J' 'NO_INVASIVES' 'X'
              'EPA01-0448' 'K' 'NO_INVASIVES' 'X'
              'EPA01-0450' 'A' 'NO_INVASIVES' 'X'
              'EPA01-0450' 'B' 'NO_INVASIVES' 'X'
              'EPA01-0450' 'C' 'NO_INVASIVES' 'X'
              'EPA01-0450' 'D' 'NO_INVASIVES' 'X'
              'EPA01-0450' 'E' 'NO_INVASIVES' 'X'
              'EPA01-0450' 'F' 'NO_INVASIVES' 'X'
              'EPA01-0450' 'G' 'NO_INVASIVES' 'X'
              'EPA01-0450' 'H' 'NO_INVASIVES' 'X'
              'EPA01-0450' 'I' 'NO_INVASIVES' 'X'
              'EPA01-0450' 'J' 'NO_INVASIVES' 'X'
              'EPA01-0450' 'K' 'NO_INVASIVES' 'X'
              'EPA01-0452' 'A' 'NO_INVASIVES' 'X'
              'EPA01-0452' 'B' 'NO_INVASIVES' 'X'
              'EPA01-0452' 'C' 'NO_INVASIVES' 'X'
              'EPA01-0452' 'D' 'NO_INVASIVES' 'X'
              'EPA01-0452' 'E' 'NO_INVASIVES' 'X'
              'EPA01-0452' 'F' 'NO_INVASIVES' 'X'
              'EPA01-0452' 'G' 'NO_INVASIVES' 'X'
              'EPA01-0452' 'H' 'NO_INVASIVES' 'X'
              'EPA01-0452' 'I' 'NO_INVASIVES' 'X'
              'EPA01-0452' 'J' 'NO_INVASIVES' 'X'
              'EPA01-0452' 'K' 'NO_INVASIVES' 'X'
              'EPA01-0457' 'A' 'NO_INVASIVES' 'X'
              'EPA01-0457' 'B' 'NO_INVASIVES' 'X'
              'EPA01-0457' 'C' 'NO_INVASIVES' 'X'
              'EPA01-0457' 'D' 'NO_INVASIVES' 'X'
              'EPA01-0457' 'E' 'NO_INVASIVES' 'X'
              'EPA01-0457' 'F' 'NO_INVASIVES' 'X'
              'EPA01-0457' 'G' 'NO_INVASIVES' 'X'
              'EPA01-0457' 'H' 'NO_INVASIVES' 'X'
              'EPA01-0457' 'I' 'NO_INVASIVES' 'X'
              'EPA01-0457' 'J' 'NO_INVASIVES' 'X'
              'EPA01-0457' 'K' 'NO_INVASIVES' 'X'
              'EPA01-0458' 'A' 'NO_INVASIVES' 'X'
              'EPA01-0458' 'B' 'G_REED' 'Y'
              'EPA01-0458' 'C' 'G_REED' 'Y'
              'EPA01-0458' 'D' 'G_REED' 'Y'
              'EPA01-0458' 'E' 'NO_INVASIVES' 'X'
              'EPA01-0458' 'F' 'G_REED' 'Y'
              'EPA01-0458' 'G' 'G_REED' 'Y'
              'EPA01-0458' 'H' 'G_REED' 'Y'
              'EPA01-0458' 'I' 'G_REED' 'Y'
              'EPA01-0458' 'J' 'G_REED' 'Y'
              'EPA01-0458' 'K' 'G_REED' 'Y'
              'EPA01-0537' 'A' 'P_LSTRIFE' 'Y'
              'EPA01-0537' 'B' 'P_LSTRIFE' 'Y'
              'EPA01-0537' 'C' 'P_LSTRIFE' 'Y'
              'EPA01-0537' 'D' 'P_LSTRIFE' 'Y'
              'EPA01-0537' 'E' 'P_LSTRIFE' 'Y'
              'EPA01-0537' 'F' 'P_LSTRIFE' 'Y'
              'EPA01-0537' 'G' 'P_LSTRIFE' 'Y'
              'EPA01-0537' 'H' 'P_LSTRIFE' 'Y'
              'EPA01-0537' 'I' 'P_LSTRIFE' 'Y'
              'EPA01-0537' 'J' 'P_LSTRIFE' 'Y'
              'EPA01-0537' 'K' 'P_LSTRIFE' 'Y'
              'EPA01-0555' 'A' 'NO_INVASIVES' 'X'
              'EPA01-0555' 'B' 'P_LSTRIFE' 'Y'
              'EPA01-0555' 'C' 'NO_INVASIVES' 'X'
              'EPA01-0555' 'D' 'NO_INVASIVES' 'X'
              'EPA01-0555' 'E' 'NO_INVASIVES' 'X'
              'EPA01-0555' 'F' 'NO_INVASIVES' 'X'
              'EPA01-0555' 'G' 'NO_INVASIVES' 'X'
              'EPA01-0555' 'H' 'NO_INVASIVES' 'X'
              'EPA01-0555' 'I' 'NO_INVASIVES' 'X'
              'EPA01-0555' 'J' 'NO_INVASIVES' 'X'
              'EPA01-0555' 'K' 'NO_INVASIVES' 'X'
              'EPA01-0556' 'A' 'NO_INVASIVES' 'X'
              'EPA01-0556' 'B' 'NO_INVASIVES' 'X'
              'EPA01-0556' 'C' 'NO_INVASIVES' 'X'
              'EPA01-0556' 'D' 'NO_INVASIVES' 'X'
              'EPA01-0556' 'E' 'NO_INVASIVES' 'X'
              'EPA01-0556' 'F' 'NO_INVASIVES' 'X'
              'EPA01-0556' 'G' 'NO_INVASIVES' 'X'
              'EPA01-0556' 'H' 'NO_INVASIVES' 'X'
              'EPA01-0556' 'I' 'NO_INVASIVES' 'X'
              'EPA01-0556' 'J' 'NO_INVASIVES' 'X'
              'EPA01-0556' 'K' 'NO_INVASIVES' 'X'
              "
              )
  testData <-read.table(fred, header=TRUE, stringsAsFactors=FALSE)
  close(fred)

  return(testData)
}

metsInvasiveSpecies.createMetrics <- function()
# creates dataframe of expected metrics results for unit test
{
  bob <- textConnection(
              "UID METRIC RESULT
              'EPA01-0124' 'f_lytsal' 0.181818182
              'EPA01-0124' 'f_none' 0.818181818
              'EPA01-0124' 'ip_score' 0.181818182
              'EPA01-0144' 'f_none' 1
              'EPA01-0144' 'ip_score' 0
              'EPA01-0147' 'f_none' 1
              'EPA01-0147' 'ip_score' 0
              'EPA01-0159' 'f_eiccra' 0.818181818
              'EPA01-0159' 'f_lytsal' 1
              'EPA01-0159' 'ip_score' 1.818181818
              'EPA01-0161' 'f_lytsal' 0.909090909
              'EPA01-0161' 'f_none' 0.090909091
              'EPA01-0161' 'ip_score' 0.909090909
              'EPA01-0167' 'f_none' 1
              'EPA01-0167' 'ip_score' 0
              'EPA01-0209' 'f_none' 1
              'EPA01-0209' 'ip_score' 0
              'EPA01-0210' 'f_none' 1
              'EPA01-0210' 'ip_score' 0
              'EPA01-0212' 'f_none' 1
              'EPA01-0212' 'ip_score' 0
              'EPA01-0214' 'f_none' 1
              'EPA01-0214' 'ip_score' 0
              'EPA01-0215' 'f_none' 1
              'EPA01-0215' 'ip_score' 0
              'EPA01-0238' 'f_eiccra' 1
              'EPA01-0238' 'f_butumb' 1
              'EPA01-0238' 'ip_score' 2
              'EPA01-0239' 'f_none' 1
              'EPA01-0239' 'ip_score' 0
              'EPA01-0240' 'f_none' 1
              'EPA01-0240' 'ip_score' 0
              'EPA01-0242' 'f_none' 1
              'EPA01-0242' 'ip_score' 0
              'EPA01-0246' 'f_none' 1
              'EPA01-0246' 'ip_score' 0
              'EPA01-0248' 'f_none' 1
              'EPA01-0248' 'ip_score' 0
              'EPA01-0302' 'f_none' 1
              'EPA01-0302' 'ip_score' 0
              'EPA01-0303' 'f_none' 1
              'EPA01-0303' 'ip_score' 0
              'EPA01-0305' 'f_none' 1
              'EPA01-0305' 'ip_score' 0
              'EPA01-0306' 'f_butumb' 0.272727273
              'EPA01-0306' 'f_none' 0.727272727
              'EPA01-0306' 'ip_score' 0.272727273
              'EPA01-0308' 'f_eiccra' 0.090909091
              'EPA01-0308' 'f_none' 0.909090909
              'EPA01-0308' 'ip_score' 0.090909091
              'EPA01-0314' 'f_eiccra' 1
              'EPA01-0314' 'f_butumb' 0.181818182
              'EPA01-0314' 'ip_score' 1.181818182
              'EPA01-0315' 'f_none' 1
              'EPA01-0315' 'ip_score' 0
              'EPA01-0421' 'f_none' 1
              'EPA01-0421' 'ip_score' 0
              'EPA01-0424' 'f_none' 1
              'EPA01-0424' 'ip_score' 0
              'EPA01-0432' 'f_none' 1
              'EPA01-0432' 'ip_score' 0
              'EPA01-0448' 'f_none' 1
              'EPA01-0448' 'ip_score' 0
              'EPA01-0450' 'f_none' 1
              'EPA01-0450' 'ip_score' 0
              'EPA01-0452' 'f_none' 1
              'EPA01-0452' 'ip_score' 0
              'EPA01-0457' 'f_none' 1
              'EPA01-0457' 'ip_score' 0
              'EPA01-0458' 'f_arudon' 0.818181818
              'EPA01-0458' 'f_none' 0.181818182
              'EPA01-0458' 'ip_score' 0.818181818
              'EPA01-0537' 'f_lytsal' 1
              'EPA01-0537' 'ip_score' 1
              'EPA01-0555' 'f_lytsal' 0.090909091
              'EPA01-0555' 'f_none' 0.909090909
              'EPA01-0555' 'ip_score' 0.090909091
              'EPA01-0556' 'f_none' 1
              'EPA01-0556' 'ip_score' 0
              "
              )

  testResults <-read.table(bob, header=TRUE, stringsAsFactors=FALSE)
  close(bob)

  return(testResults)

}