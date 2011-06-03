## metsLegacyTree.r
##
## Created SSR 01/29/2010
#  02/18/10 cws removed source() of NRSAValidation.r
#  03/22/10 cws added missing calls to checkEquals in unit test!
#   3/25/10 cws Changed diff() calls to dfCompare().

require(reshape)
source('L:/Priv/CORFiles/IM/Rwork/nrsa/code/assignTaxCat.r')

metsLegacyTree <- function()
## Creating mhtree.csv which contains the metrics:
# ltmxcnt -  Legacy number of largest trees
# ltmxdbh -  Legacy largest tree dbh
# ltmxdist -  Legacy largest tree distance
# ltmxht -  Legacy largest tree height
# ltmxsize -  Legacy largest tree size class (SMLX)
# ltmxspp -  Legacy largest tree species
# ltsplist -  Legacy tree species list comma delim.
# ltfracs - Legacy fraction of reach trees >= small
# ltfracm - Legacy fraction of reach trees >= medium
# ltfracl - Legacy fraction of reach trees >= large
# ltfracx - Legacy fraction of reach trees >= Xlarge
# ltmddist - Legacy mean dist of trees >= median size
# ltmddom - Legacy dominant sp.
# ltmddomn - Legacy dominant sp. Count
# ltmdsub -  Legacy subdominant sp. >= median size
# ltmdsubn -  Legacy subdominant sp. count

{
intermediateMessage('Legacy tree metrics calculations', loc='start')
intermediateMessage('.1 Read in data', loc='end')

##  Reading in data from tblINVASIVES2
chan <- odbcConnect('NRSA2')
tblIL <- fetchNRSATable(chan,'tblINVASIVELEGACY2')
close(chan)
df1 <- subset (tblIL, PARAMETER %in% c('TREE_TYP', 'SPECIES', 'DBH',
              'HEIGHT', 'DISTANCE'), 
              select=c('UID','TRANSECT','PARAMETER','RESULT'))

## Calculate the metrics
intermediateMessage('.2 call function metsLegacyTree.1', loc='end')
mets <- metsLegacyTree.1(df1)

## Write the results to metsLegacyTree.csv
intermediateMessage('.3 Write results', loc='end')
rc <- writeNRSACalcResults(mets, 'metsLegacyTree.csv')
        
intermediateMessage('  Done.', loc='end')
return(rc)

}


metsLegacyTree.1 <- function(df1)
{
##  Calculating metrics

##  Renaming PARAMETER and RESULT to variable and value
df1 <-rename(df1, c('PARAMETER', 'RESULT'), c('variable', 'value'))

# Casting (transforming) data using reshape package
lt <- as.data.frame(cast(df1, UID + TRANSECT ~ variable))
lt$DBH      <- as.character(lt$DBH)
#lt$NOT_VIS  <- as.character(lt$NOT_VIS)
lt$SPECIES  <- as.character(lt$SPECIES)
lt$TREE_TYP <- as.character(lt$TREE_TYP)
lt$HEIGHT   <- as.character(lt$HEIGHT)
lt$DISTANCE <- as.character(lt$DISTANCE)
lt$DISTANCE <- as.numeric(lt$DISTANCE)

lt$METRIC <- ''
lt$numdbh  <- NA 
lt$numht   <- NA 
lt$size    <- ''
lt$sizenum <- NA
lt$sizen   <- NA 

#  Creating values of DBH and HEIGHT with simpler codes
#  newDBH: 0-0.1 = 1; .1-.3 = 2; .3-.75 = 3; .75-2=4; >2=5
#  newHEIGHT: <5 = 1; 5-15 = 2; 15-30 = 3; >30 = 4
## Matrices for tree size
##
##  Determining sizen
##                        DBH
##              <5     5-15  15-30   >30
##   HEIGHT
##     0-0.1     1      2      3      4
##
##   0.1-0.3     2      3      4      5
##
##  0.3-0.75     3      4      5      6
##
##    0.75-2     4      5      6      7
##
##        >2     5      6      7      8
##
##
##  Determining size
##                        DBH
##              <5     5-15  15-30   >30
##   HEIGHT
##     0-0.1     S      S      M      M
##
##   0.1-0.3     S      M      M      M
##
##  0.3-0.75     M      M      L      L
##
##    0.75-2     L      L      L      X
##
##        >2     L      X      X      X
##

lt$numdbh <- ifelse(lt$DBH=='0-0.1', 1, lt$numdbh)
lt$numdbh <- ifelse(lt$DBH=='.1-.3', 2, lt$numdbh)
lt$numdbh <- ifelse(lt$DBH=='.3-.75', 3, lt$numdbh)
lt$numdbh <- ifelse(lt$DBH=='.75-2', 4, lt$numdbh)
lt$numdbh <- ifelse(lt$DBH=='>2', 5, lt$numdbh)

lt$numht <- ifelse(lt$HEIGHT=='<5', 1, lt$numht)
lt$numht <- ifelse(lt$HEIGHT=='5-15', 2, lt$numht)
lt$numht <- ifelse(lt$HEIGHT=='15-30', 3, lt$numht)
lt$numht <- ifelse(lt$HEIGHT=='>30', 4, lt$numht)

lt$size <- ifelse(lt$DBH=='0-0.1' & (lt$HEIGHT=='<5' | lt$HEIGHT=='5-15'), 'S', lt$size)
lt$size <- ifelse(lt$DBH=='.1-.3' & lt$HEIGHT=='<5', 'S', lt$size)
lt$size <- ifelse(lt$DBH=='0-0.1' & (lt$HEIGHT=='15-30' | lt$HEIGHT=='>30'), 'M', lt$size)
lt$size <- ifelse(lt$DBH=='.1-.3' & (lt$HEIGHT=='5-15' | lt$HEIGHT=='15-30' | lt$HEIGHT=='>30'), 'M', lt$size)
lt$size <- ifelse(lt$DBH=='.3-.75' & (lt$HEIGHT=='<5' | lt$HEIGHT=='5-15'), 'M', lt$size)
lt$size <- ifelse(lt$DBH=='.3-.75' & (lt$HEIGHT=='15-30' | lt$HEIGHT=='>30'), 'L', lt$size)
lt$size <- ifelse(lt$DBH=='.75-2' & (lt$HEIGHT=='<5' | lt$HEIGHT=='5-15' | lt$HEIGHT=='15-30'), 'L', lt$size)
lt$size <- ifelse(lt$DBH=='>2' & lt$HEIGHT=='5-15', 'L', lt$size)
lt$size <- ifelse(lt$DBH=='.75-2' & (lt$HEIGHT=='>30'), 'X', lt$size)
lt$size <- ifelse(lt$DBH=='>2' & (lt$HEIGHT=='5-15' | lt$HEIGHT=='15-30' | lt$HEIGHT=='>30'), 'X', lt$size)

lt$sizenum <- ifelse(lt$size=='S', 1, lt$sizenum)
lt$sizenum <- ifelse(lt$size=='M', 2, lt$sizenum)
lt$sizenum <- ifelse(lt$size=='L', 3, lt$sizenum)
lt$sizenum <- ifelse(lt$size=='X', 4, lt$sizenum)
 
lt$sizen <- ifelse(lt$DBH=='0-0.1' & lt$HEIGHT=='<5', 1, lt$sizen)
lt$sizen <- ifelse((lt$DBH=='0-0.1' & lt$HEIGHT=='5-15')
                 | (lt$DBH=='.1-.3' & lt$HEIGHT=='<5'), 2, lt$sizen)
lt$sizen <- ifelse((lt$DBH=='0-0.1' & lt$HEIGHT=='15-30')
                 | (lt$DBH=='.1-.3' & lt$HEIGHT=='5-15')
                 | (lt$DBH=='.3-.75' & lt$HEIGHT=='<5'), 3, lt$sizen)
lt$sizen <- ifelse((lt$DBH=='0-0.1' & lt$HEIGHT=='>30')
                 | (lt$DBH=='.1-.3' & lt$HEIGHT=='15-30') 
                 | (lt$DBH=='.3-.75' & lt$HEIGHT=='5-15') 
                 | (lt$DBH=='.75-2' & lt$HEIGHT=='<5'), 4, lt$sizen)
lt$sizen <- ifelse((lt$DBH=='.1-.3' & lt$HEIGHT=='>30')
                 | (lt$DBH=='.3-.75' & lt$HEIGHT=='15-30') 
                 | (lt$DBH=='.75-2' & lt$HEIGHT=='5-15') 
                 | (lt$DBH=='>2' & lt$HEIGHT=='<5'), 5, lt$sizen)
lt$sizen <- ifelse((lt$DBH=='.3-.75' & lt$HEIGHT=='>30') 
                 | (lt$DBH=='.75-2' & lt$HEIGHT=='15-30') 
                 | (lt$DBH=='>2' & lt$HEIGHT=='5-15'), 6, lt$sizen)
lt$sizen <- ifelse((lt$DBH=='.75-2' & lt$HEIGHT=='>30')
                 | (lt$DBH=='>2' & lt$HEIGHT=='15-30'), 7, lt$sizen)
lt$sizen <- ifelse(lt$DBH=='>2' & lt$HEIGHT=='>30', 8, lt$sizen)

###  Assigning taxonomic category based on crew entry
lt <- assignTaxCat(lt)

###  Determining largest tree
#  Getting max of sizen
aa <- subset(lt, !(is.na(sizen)), select=c('UID','sizen'))
##  This is where the warnings occur if there are no non-missing arguments to max 
bb <- aggregate(list(maxSizen=aa$sizen), list(UID=aa$UID), max)
cc <- subset(merge(bb, lt), maxSizen==sizen)
dd <- first(cc, 'UID', 'first.UID')
ee <- subset(dd, first.UID=='TRUE')

## NOTE:  Metrics for ltmxcnt procuced by SAS appear to count all trees
##        at a site, not just those with max sizen
# Getting counts of max sizen
ff <- aggregate(list(ltmxcnt=cc$sizen),list(UID=cc$UID),count)
gg <- merge(ee,ff)
ltMxMets <- rename(gg, c('DBH','HEIGHT','taxCat','size','DISTANCE'),
             c('ltmxdbh','ltmxht','ltmxspp','ltmxsize','ltmxdist'))
             
ltmxdbh  <- subset(ltMxMets, select=c('UID','METRIC','ltmxdbh'))
  ltmxdbh$METRIC <- 'ltmxdbh'
  ltmxdbh <- rename(ltmxdbh,'ltmxdbh','RESULT')
ltmxht  <- subset(ltMxMets, select=c('UID','METRIC','ltmxht'))             
  ltmxht$METRIC <- 'ltmxht'
  ltmxht <- rename(ltmxht,'ltmxht','RESULT')
ltmxspp  <- subset(ltMxMets, select=c('UID','METRIC','ltmxspp'))
  ltmxspp$METRIC <- 'ltmxspp'
  ltmxspp <- rename(ltmxspp,'ltmxspp','RESULT')
ltmxsize  <- subset(ltMxMets, select=c('UID','METRIC','ltmxsize'))
  ltmxsize$METRIC <- 'ltmxsize'
  ltmxsize <- rename(ltmxsize,'ltmxsize','RESULT')
ltmxdist  <- subset(ltMxMets, select=c('UID','METRIC','ltmxdist'))
  ltmxdist$METRIC <- 'ltmxdist'
  ltmxdist <- rename(ltmxdist,'ltmxdist','RESULT')
ltmxcnt  <- subset(ltMxMets, select=c('UID','METRIC','ltmxcnt'))
  ltmxcnt$METRIC <- 'ltmxcnt'
  ltmxcnt <- rename(ltmxcnt,'ltmxcnt','RESULT')

#### Determining median tree size and related metrics
# ltfracl - Legacy fraction of trees >= large
# ltfracm - Legacy fraction of trees >= medium
# ltfracs - Legacy fraction of trees >= small
# ltfracx - Legacy fraction of trees >= xlarge
# ltmddist - Legacy mean dist of trees >= median size

## NOTE:  SAS code used median DBH as median tree size.
## NOTE:  SAS code created counts for ltfracs, ltfracm, ltfracl, ltfracx.

## ltmddist
aa <- subset(aggregate(list(medianSize=lt$numdbh), list(UID=lt$UID),
             na.rm = T, median), !(is.na(medianSize)))
lt <- merge(lt, aa, all.x=T)
lt$medianDistance <- ifelse(lt$numdbh>=lt$medianSize, lt$DISTANCE, NA)
ltmddist <- aggregate(list(ltmddist=lt$medianDistance),
               list(UID=lt$UID,METRIC=lt$METRIC), mean, na.rm = T)
  ltmddist$METRIC <- 'ltmddist'
  ltmddist <- rename(ltmddist, 'ltmddist', 'RESULT')

## fractions of trees by size
# cntS - count of trees >= small -> s, m, l, x
# cntM - count of trees >= medium -> m, l, x
# cntL - count of trees >= large -> l, x
# cntX - count of trees >= xlarge  -> x

## Creating count of transects by UID
tranCnt <- subset(aggregate(list(tranCnt=lt$TRANSECT), by=list(UID=lt$UID), count), select=c('UID', 'tranCnt'))

sm <- aggregate(list(smCnt=subset(lt$size, lt$size %in% c('S','M','L','X'))),
        list(UID=subset(lt$UID,lt$size %in% c('S','M','L','X'))), count)
med <- aggregate(list(medCnt=subset(lt$size, lt$size %in% c('M','L','X'))),
        list(UID=subset(lt$UID,lt$size %in% c('M','L','X'))), count)
lg <- aggregate(list(lgCnt=subset(lt$size, lt$size %in% c('L','X'))),
        list(UID=subset(lt$UID,lt$size %in% c('L','X'))), count)
xl <- aggregate(list(xlCnt=subset(lt$size, lt$size %in% c('X'))),
        list(UID=subset(lt$UID,lt$size %in% c('X'))), count)

aa <- merge(sm, med, all=T)
bb <- merge(aa, lg, all=T)
cc <- merge(bb, xl, all=T)
treeCounts <- merge(tranCnt, cc, all=T)
treeCounts$ltfracs <- treeCounts$smCnt/treeCounts$tranCnt
treeCounts$ltfracm <- treeCounts$medCnt/treeCounts$tranCnt
treeCounts$ltfracl <- treeCounts$lgCnt/treeCounts$tranCnt
treeCounts$ltfracx <- treeCounts$xlCnt/treeCounts$tranCnt

treeCounts$ltfracs <- ifelse(is.na(treeCounts$ltfracs), 0, treeCounts$ltfracs)
treeCounts$ltfracm <- ifelse(is.na(treeCounts$ltfracm), 0, treeCounts$ltfracm)
treeCounts$ltfracl <- ifelse(is.na(treeCounts$ltfracl), 0, treeCounts$ltfracl)
treeCounts$ltfracx <- ifelse(is.na(treeCounts$ltfracx), 0, treeCounts$ltfracx)

treeCounts$METRIC <- ''

ltfracs <- subset(treeCounts, select=c('UID','METRIC','ltfracs'))
  ltfracs$METRIC <- 'ltfracs'
  ltfracs <- rename(ltfracs,'ltfracs','RESULT')
ltfracm <- subset(treeCounts, select=c('UID','METRIC','ltfracm'))
  ltfracm$METRIC <- 'ltfracm'
  ltfracm <- rename(ltfracm,'ltfracm','RESULT')
ltfracl <- subset(treeCounts, select=c('UID','METRIC','ltfracl'))
  ltfracl$METRIC <- 'ltfracl'
  ltfracl <- rename(ltfracl,'ltfracl','RESULT')
ltfracx <- subset(treeCounts, select=c('UID','METRIC','ltfracx'))
  ltfracx$METRIC <- 'ltfracx'
  ltfracx <- rename(ltfracx,'ltfracx','RESULT')

### And now for the piece de resistance - the list of dominant and subdominant species
# ltsplist - List of all speicies present
# ltmddom - Species occurring most commonly
# ltmddomn - Number of times dominant speices occurs
# ltmdsub - Second most commonly occurring species
# ltmdsubn - Number of tims subdominant species occurs

## NOTE:  SAS code miscounts ltmddomn and ltmdsubn by one tree in each metric.
                     
##  Creating ltmddom and ltmdsub
counts <- aggregate(list(count=lt$taxCat), list('UID'=lt$UID,'METRIC'=lt$METRIC, 'maxTaxCat'=lt$taxCat), count)
ltmddomn <- aggregate(list('ltmddomn'=counts$count), list('UID'=counts$UID, METRIC=counts$METRIC), max)
  ltmddomn$METRIC <- 'ltmddomn'
  ltmddomn <- rename(ltmddomn,'ltmddomn','RESULT')
  
aa <- subset(merge(counts, subset(ltmddomn,select=c('UID','RESULT')), by='UID'), count==RESULT)
ltmddom <- aggregate(list('ltmddom'=aa$maxTaxCat)
                        ,list('UID'=aa$UID,'METRIC'=aa$METRIC)
                        ,function(x) { paste(x, collapse=',') } )
  ltmddom$METRIC <- 'ltmddom'
  ltmddom <- rename(ltmddom,'ltmddom','RESULT')
                     
bb <- subset(merge(counts, subset(ltmddomn, select=c('UID','RESULT')),
        by='UID'), count!=RESULT, select=c('UID','METRIC','maxTaxCat','count'))                      
ltmdsubn <- aggregate(list('ltmdsubn'=bb$count), list('UID'=bb$UID, METRIC=bb$METRIC), max)
  ltmdsubn$METRIC <- 'ltmdsubn'
  ltmdsubn <- rename(ltmdsubn,'ltmdsubn','RESULT')
cc <- subset(merge(counts, subset(ltmdsubn,select=c('UID','RESULT')), by='UID'), count==RESULT)
ltmdsub <- aggregate(list('ltmdsub'=cc$maxTaxCat)
                        ,list('UID'=cc$UID,'METRIC'=cc$METRIC)
                        ,function(x) { paste(x, collapse=',')})
  ltmdsub$METRIC <- 'ltmdsub'
  ltmdsub <- rename(ltmdsub,'ltmdsub','RESULT')

##  List of all trees
hasTrees <- subset(lt, !(is.na(SPECIES)))                   
ltsplist <- aggregate(list('ltsplist'=hasTrees$SPECIES)
                        ,list('UID'=hasTrees$UID,'METRIC'=hasTrees$METRIC) 
                        ,function(x) { paste(x, collapse=',')})
  ltsplist$METRIC <- 'ltsplist'
  ltsplist <- rename(ltsplist,'ltsplist','RESULT')


####  Woo hoo! Let's rbind these puppies and put 'em to bed
mhtrees <- rbind(ltmxdbh,ltmxht,ltmxspp,ltmxsize,ltmxdist,ltmxcnt,ltfracs,ltfracm,
                 ltfracl,ltfracx,ltmddist,ltsplist,ltmddom,ltmddomn,ltmdsub,
                 ltmdsubn)

}


##  Testing metsLegacyTree
##  Data and metrics for/from EMAP SAS code are being used.

metsLegacyTreeTest <- function()
{
  testData <- metsLegacyTree.inputData()
  testResults <- metsLegacyTree.testResults()

  legMets <- metsLegacyTree.1(testData)

  ## Get both data frames in the same order
  testMets <- legMets[order(legMets$UID, legMets$METRIC),]
  sasMets <- testResults[order(testResults$UID, testResults$METRIC),]

  # Character and numeric metrics are checked separately
  testMets.1 <- subset(testMets
                      ,METRIC %in% c('ltfracl', 'ltfracm', 'ltfracs'
                                    ,'ltfracx', 'ltmddist', 'ltmddomn'
                                    ,'ltmxcnt', 'ltmxdist', 'ltmdsubn'
                                    )
                      )
  testMets.2 <- subset(testMets
                      ,!(METRIC %in% c('ltfracl', 'ltfracm', 'ltfracs'
                                      ,'ltfracx', 'ltmddist', 'ltmddomn'
                                      ,'ltmxcnt', 'ltmxdist', 'ltmdsubn'
                                      )
                        )
                      )

  sasMets.1 <- subset(sasMets
                     ,METRIC %in% c('ltfracl', 'ltfracm', 'ltfracs'
                                   ,'ltfracx', 'ltmddist', 'ltmddomn'
                                   ,'ltmxcnt', 'ltmxdist', 'ltmdsubn'
                                   )
                     )
  sasMets.2 <- subset(sasMets
                     ,!(METRIC %in% c('ltfracl', 'ltfracm', 'ltfracs'
                                     ,'ltfracx', 'ltmddist', 'ltmddomn'
                                     ,'ltmxcnt', 'ltmxdist', 'ltmdsubn'
                                     )
                       )
                     )

  testMets.1$RESULT <- as.numeric(testMets.1$RESULT)
  sasMets.1$RESULT <- as.numeric(sasMets.1$RESULT)

  aa <- dfCompare(testMets.1, sasMets.1, c('UID', 'METRIC'), zeroFudge=1e-9)
  checkEquals(NULL, aa, "Error: metsLegacyTree numeric metrics are broken")
  bb <- dfCompare(testMets.2, sasMets.2, c('UID', 'METRIC'))
  checkEquals(NULL, aa, "Error: metsLegacyTree character metrics are broken")

  #return(aa)
  #return(bb)
}


metsLegacyTree.inputData <- function()
# creates dataframe of legacy tree data for unit test
{
fred <- textConnection(
"UID TRANSECT PARAMETER RESULT
'EPA01-0124' 'A' 'DBH' '.3-.75'
'EPA01-0124' 'A' 'DISTANCE' '1'
'EPA01-0124' 'A' 'HEIGHT' '>30'
'EPA01-0124' 'A' 'SPECIES' 'FIR'
'EPA01-0124' 'A' 'TREE_TYP' 'Coniferous'
'EPA01-0124' 'B' 'DBH' '.3-.75'
'EPA01-0124' 'B' 'DISTANCE' '1'
'EPA01-0124' 'B' 'HEIGHT' '>30'
'EPA01-0124' 'B' 'SPECIES' 'FIR'
'EPA01-0124' 'B' 'TREE_TYP' 'Coniferous'
'EPA01-0124' 'C' 'DBH' '.3-.75'
'EPA01-0124' 'C' 'DISTANCE' '0'
'EPA01-0124' 'C' 'HEIGHT' '>30'
'EPA01-0124' 'C' 'SPECIES' 'FIR'
'EPA01-0124' 'C' 'TREE_TYP' 'Coniferous'
'EPA01-0124' 'D' 'DBH' '.3-.75'
'EPA01-0124' 'D' 'DISTANCE' '1'
'EPA01-0124' 'D' 'HEIGHT' '>30'
'EPA01-0124' 'D' 'SPECIES' 'FIR'
'EPA01-0124' 'D' 'TREE_TYP' 'Coniferous'
'EPA01-0124' 'E' 'DBH' '.3-.75'
'EPA01-0124' 'E' 'DISTANCE' '1'
'EPA01-0124' 'E' 'HEIGHT' '15-30'
'EPA01-0124' 'E' 'SPECIES' 'FIR'
'EPA01-0124' 'E' 'TREE_TYP' 'Coniferous'
'EPA01-0124' 'F' 'DBH' '.3-.75'
'EPA01-0124' 'F' 'DISTANCE' '3'
'EPA01-0124' 'F' 'HEIGHT' '>30'
'EPA01-0124' 'F' 'SPECIES' 'FIR'
'EPA01-0124' 'F' 'TREE_TYP' 'Coniferous'
'EPA01-0124' 'G' 'DBH' '.3-.75'
'EPA01-0124' 'G' 'DISTANCE' '0'
'EPA01-0124' 'G' 'HEIGHT' '>30'
'EPA01-0124' 'G' 'SPECIES' 'FIR'
'EPA01-0124' 'G' 'TREE_TYP' 'Coniferous'
'EPA01-0124' 'H' 'DBH' '.3-.75'
'EPA01-0124' 'H' 'DISTANCE' '0'
'EPA01-0124' 'H' 'HEIGHT' '>30'
'EPA01-0124' 'H' 'SPECIES' 'FIR'
'EPA01-0124' 'H' 'TREE_TYP' 'Coniferous'
'EPA01-0124' 'I' 'DBH' '.3-.75'
'EPA01-0124' 'I' 'DISTANCE' '1'
'EPA01-0124' 'I' 'HEIGHT' '>30'
'EPA01-0124' 'I' 'SPECIES' 'FIR'
'EPA01-0124' 'I' 'TREE_TYP' 'Coniferous'
'EPA01-0124' 'J' 'DBH' '.3-.75'
'EPA01-0124' 'J' 'DISTANCE' '2'
'EPA01-0124' 'J' 'HEIGHT' '>30'
'EPA01-0124' 'J' 'SPECIES' 'FIR'
'EPA01-0124' 'J' 'TREE_TYP' 'Coniferous'
'EPA01-0124' 'K' 'DBH' '.3-.75'
'EPA01-0124' 'K' 'DISTANCE' '1'
'EPA01-0124' 'K' 'HEIGHT' '>30'
'EPA01-0124' 'K' 'SPECIES' 'FIR'
'EPA01-0124' 'K' 'TREE_TYP' 'Coniferous'
'EPA01-0144' 'A' 'DBH' '.3-.75'
'EPA01-0144' 'A' 'DISTANCE' '1'
'EPA01-0144' 'A' 'HEIGHT' '>30'
'EPA01-0144' 'A' 'SPECIES' 'SPRUCE'
'EPA01-0144' 'A' 'TREE_TYP' 'Coniferous'
'EPA01-0144' 'B' 'DBH' '.3-.75'
'EPA01-0144' 'B' 'DISTANCE' '15'
'EPA01-0144' 'B' 'HEIGHT' '>30'
'EPA01-0144' 'B' 'SPECIES' 'LODGEPOLE PINE'
'EPA01-0144' 'B' 'TREE_TYP' 'Coniferous'
'EPA01-0144' 'C' 'DBH' '.3-.75'
'EPA01-0144' 'C' 'DISTANCE' '5'
'EPA01-0144' 'C' 'HEIGHT' '>30'
'EPA01-0144' 'C' 'SPECIES' 'SPRUCE'
'EPA01-0144' 'C' 'TREE_TYP' 'Coniferous'
'EPA01-0144' 'D' 'DBH' '.3-.75'
'EPA01-0144' 'D' 'DISTANCE' '4'
'EPA01-0144' 'D' 'HEIGHT' '>30'
'EPA01-0144' 'D' 'SPECIES' 'SPRUCE'
'EPA01-0144' 'D' 'TREE_TYP' 'Coniferous'
'EPA01-0144' 'E' 'DBH' '.3-.75'
'EPA01-0144' 'E' 'DISTANCE' '15'
'EPA01-0144' 'E' 'HEIGHT' '>30'
'EPA01-0144' 'E' 'SPECIES' 'SUBALPINE FIR'
'EPA01-0144' 'E' 'TREE_TYP' 'Coniferous'
'EPA01-0144' 'F' 'DBH' '.3-.75'
'EPA01-0144' 'F' 'DISTANCE' '5'
'EPA01-0144' 'F' 'HEIGHT' '>30'
'EPA01-0144' 'F' 'SPECIES' 'SUBALPINE FIR'
'EPA01-0144' 'F' 'TREE_TYP' 'Coniferous'
'EPA01-0144' 'G' 'DBH' '.3-.75'
'EPA01-0144' 'G' 'DISTANCE' '0.5'
'EPA01-0144' 'G' 'HEIGHT' '>30'
'EPA01-0144' 'G' 'SPECIES' 'SPRUCE'
'EPA01-0144' 'G' 'TREE_TYP' 'Coniferous'
'EPA01-0144' 'H' 'DBH' '.3-.75'
'EPA01-0144' 'H' 'DISTANCE' '2'
'EPA01-0144' 'H' 'HEIGHT' '>30'
'EPA01-0144' 'H' 'SPECIES' 'SPRUCE'
'EPA01-0144' 'H' 'TREE_TYP' 'Coniferous'
'EPA01-0144' 'I' 'DBH' '.3-.75'
'EPA01-0144' 'I' 'DISTANCE' '10'
'EPA01-0144' 'I' 'HEIGHT' '>30'
'EPA01-0144' 'I' 'SPECIES' 'SPRUCE'
'EPA01-0144' 'I' 'TREE_TYP' 'Coniferous'
'EPA01-0144' 'J' 'DBH' '.1-.3'
'EPA01-0144' 'J' 'DISTANCE' '15'
'EPA01-0144' 'J' 'HEIGHT' '15-30'
'EPA01-0144' 'J' 'SPECIES' 'SUBALPINE FIR'
'EPA01-0144' 'J' 'TREE_TYP' 'Coniferous'
'EPA01-0144' 'K' 'DBH' '.3-.75'
'EPA01-0144' 'K' 'DISTANCE' '1'
'EPA01-0144' 'K' 'HEIGHT' '>30'
'EPA01-0144' 'K' 'SPECIES' 'SPRUCE'
'EPA01-0144' 'K' 'TREE_TYP' 'Coniferous'
'EPA01-0147' 'A' 'DBH' '.3-.75'
'EPA01-0147' 'A' 'DISTANCE' '4'
'EPA01-0147' 'A' 'HEIGHT' '>30'
'EPA01-0147' 'A' 'SPECIES' 'WESTERN RED CEDAR'
'EPA01-0147' 'A' 'TREE_TYP' 'Coniferous'
'EPA01-0147' 'B' 'DBH' '.3-.75'
'EPA01-0147' 'B' 'DISTANCE' '1'
'EPA01-0147' 'B' 'HEIGHT' '15-30'
'EPA01-0147' 'B' 'SPECIES' 'WESTERN RED CEDAR'
'EPA01-0147' 'B' 'TREE_TYP' 'Coniferous'
'EPA01-0147' 'C' 'DBH' '.3-.75'
'EPA01-0147' 'C' 'DISTANCE' '3'
'EPA01-0147' 'C' 'HEIGHT' '>30'
'EPA01-0147' 'C' 'SPECIES' 'HEMLOCK'
'EPA01-0147' 'C' 'TREE_TYP' 'Coniferous'
'EPA01-0147' 'D' 'DBH' '.3-.75'
'EPA01-0147' 'D' 'DISTANCE' '0.5'
'EPA01-0147' 'D' 'HEIGHT' '>30'
'EPA01-0147' 'D' 'SPECIES' 'WESTERN RED CEDAR'
'EPA01-0147' 'D' 'TREE_TYP' 'Coniferous'
'EPA01-0147' 'E' 'DBH' '.1-.3'
'EPA01-0147' 'E' 'DISTANCE' '0'
'EPA01-0147' 'E' 'HEIGHT' '15-30'
'EPA01-0147' 'E' 'SPECIES' 'WESTERN RED CEDAR'
'EPA01-0147' 'E' 'TREE_TYP' 'Coniferous'
'EPA01-0147' 'F' 'DBH' '.3-.75'
'EPA01-0147' 'F' 'DISTANCE' '2'
'EPA01-0147' 'F' 'HEIGHT' '>30'
'EPA01-0147' 'F' 'SPECIES' 'HEMLOCK'
'EPA01-0147' 'F' 'TREE_TYP' 'Coniferous'
'EPA01-0147' 'G' 'DBH' '.75-2'
'EPA01-0147' 'G' 'DISTANCE' '2'
'EPA01-0147' 'G' 'HEIGHT' '>30'
'EPA01-0147' 'G' 'SPECIES' 'WESTERN RED CEDAR'
'EPA01-0147' 'G' 'TREE_TYP' 'Coniferous'
'EPA01-0147' 'H' 'DBH' '.1-.3'
'EPA01-0147' 'H' 'DISTANCE' '10'
'EPA01-0147' 'H' 'HEIGHT' '15-30'
'EPA01-0147' 'H' 'SPECIES' 'HEMLOCK'
'EPA01-0147' 'H' 'TREE_TYP' 'Coniferous'
'EPA01-0147' 'I' 'DBH' '.75-2'
'EPA01-0147' 'I' 'DISTANCE' '1'
'EPA01-0147' 'I' 'HEIGHT' '>30'
'EPA01-0147' 'I' 'SPECIES' 'WESTERN RED CEDAR'
'EPA01-0147' 'I' 'TREE_TYP' 'Coniferous'
'EPA01-0147' 'J' 'DBH' '.3-.75'
'EPA01-0147' 'J' 'DISTANCE' '5'
'EPA01-0147' 'J' 'HEIGHT' '>30'
'EPA01-0147' 'J' 'SPECIES' 'WESTERN RED CEDAR'
'EPA01-0147' 'J' 'TREE_TYP' 'Coniferous'
'EPA01-0147' 'K' 'DBH' '.75-2'
'EPA01-0147' 'K' 'DISTANCE' '2'
'EPA01-0147' 'K' 'HEIGHT' '>30'
'EPA01-0147' 'K' 'SPECIES' 'WESTERN RED CEDAR'
'EPA01-0147' 'K' 'TREE_TYP' 'Coniferous'
'EPA01-0159' 'A' 'DBH' '0-0.1'
'EPA01-0159' 'A' 'DISTANCE' '1'
'EPA01-0159' 'A' 'HEIGHT' '5-15'
'EPA01-0159' 'A' 'SPECIES' 'WILLOW'
'EPA01-0159' 'A' 'TREE_TYP' 'Deciduous'
'EPA01-0159' 'B' 'DBH' '.1-.3'
'EPA01-0159' 'B' 'DISTANCE' '15'
'EPA01-0159' 'B' 'HEIGHT' '5-15'
'EPA01-0159' 'B' 'SPECIES' 'ASPEN'
'EPA01-0159' 'B' 'TREE_TYP' 'Deciduous'
'EPA01-0159' 'C' 'DBH' '.1-.3'
'EPA01-0159' 'C' 'DISTANCE' '15'
'EPA01-0159' 'C' 'HEIGHT' '5-15'
'EPA01-0159' 'C' 'SPECIES' 'ASPEN'
'EPA01-0159' 'C' 'TREE_TYP' 'Deciduous'
'EPA01-0159' 'D' 'DBH' '0-0.1'
'EPA01-0159' 'D' 'DISTANCE' '10'
'EPA01-0159' 'D' 'HEIGHT' '<5'
'EPA01-0159' 'D' 'SPECIES' 'ASPEN'
'EPA01-0159' 'D' 'TREE_TYP' 'Deciduous'
'EPA01-0159' 'E' 'DBH' '.1-.3'
'EPA01-0159' 'E' 'DISTANCE' '8'
'EPA01-0159' 'E' 'HEIGHT' '5-15'
'EPA01-0159' 'E' 'SPECIES' 'ASPEN'
'EPA01-0159' 'E' 'TREE_TYP' 'Deciduous'
'EPA01-0159' 'F' 'DBH' '0-0.1'
'EPA01-0159' 'F' 'DISTANCE' '1'
'EPA01-0159' 'F' 'HEIGHT' '<5'
'EPA01-0159' 'F' 'SPECIES' 'WILLOW'
'EPA01-0159' 'F' 'TREE_TYP' 'Deciduous'
'EPA01-0159' 'G' 'DBH' '.3-.75'
'EPA01-0159' 'G' 'DISTANCE' '2'
'EPA01-0159' 'G' 'HEIGHT' '<5'
'EPA01-0159' 'G' 'SPECIES' 'WILLOW'
'EPA01-0159' 'G' 'TREE_TYP' 'Deciduous'
'EPA01-0159' 'H' 'DBH' '0-0.1'
'EPA01-0159' 'H' 'DISTANCE' '5'
'EPA01-0159' 'H' 'HEIGHT' '<5'
'EPA01-0159' 'H' 'SPECIES' 'WILLOW'
'EPA01-0159' 'H' 'TREE_TYP' 'Deciduous'
'EPA01-0159' 'I' 'DBH' '0-0.1'
'EPA01-0159' 'I' 'DISTANCE' '1'
'EPA01-0159' 'I' 'HEIGHT' '<5'
'EPA01-0159' 'I' 'SPECIES' 'WILLOW'
'EPA01-0159' 'I' 'TREE_TYP' 'Deciduous'
'EPA01-0159' 'J' 'DBH' '.1-.3'
'EPA01-0159' 'J' 'DISTANCE' '1'
'EPA01-0159' 'J' 'HEIGHT' '<5'
'EPA01-0159' 'J' 'SPECIES' 'WILLOW'
'EPA01-0159' 'J' 'TREE_TYP' 'Deciduous'
'EPA01-0159' 'K' 'DBH' '.1-.3'
'EPA01-0159' 'K' 'DISTANCE' '10'
'EPA01-0159' 'K' 'HEIGHT' '5-15'
'EPA01-0159' 'K' 'SPECIES' 'WILLOW'
'EPA01-0159' 'K' 'TREE_TYP' 'Deciduous'
'EPA01-0161' 'A' 'DBH' '.1-.3'
'EPA01-0161' 'A' 'DISTANCE' '5'
'EPA01-0161' 'A' 'HEIGHT' '5-15'
'EPA01-0161' 'A' 'SPECIES' 'ALDER/BIRCH'
'EPA01-0161' 'A' 'TREE_TYP' 'Deciduous'
'EPA01-0161' 'B' 'DBH' '.1-.3'
'EPA01-0161' 'B' 'DISTANCE' '5'
'EPA01-0161' 'B' 'HEIGHT' '5-15'
'EPA01-0161' 'B' 'SPECIES' 'POPLAR/COTTONWOOD'
'EPA01-0161' 'B' 'TREE_TYP' 'Deciduous'
'EPA01-0161' 'C' 'DBH' '.1-.3'
'EPA01-0161' 'C' 'DISTANCE' '5'
'EPA01-0161' 'C' 'HEIGHT' '5-15'
'EPA01-0161' 'C' 'SPECIES' 'POPLAR/COTTONWOOD'
'EPA01-0161' 'C' 'TREE_TYP' 'Deciduous'
'EPA01-0161' 'D' 'DBH' '.1-.3'
'EPA01-0161' 'D' 'DISTANCE' '5'
'EPA01-0161' 'D' 'HEIGHT' '5-15'
'EPA01-0161' 'D' 'SPECIES' 'POPLAR/COTTONWOOD'
'EPA01-0161' 'D' 'TREE_TYP' 'Deciduous'
'EPA01-0161' 'E' 'DBH' '.1-.3'
'EPA01-0161' 'E' 'DISTANCE' '5'
'EPA01-0161' 'E' 'HEIGHT' '5-15'
'EPA01-0161' 'E' 'SPECIES' 'POPLAR/COTTONWOOD'
'EPA01-0161' 'E' 'TREE_TYP' 'Deciduous'
'EPA01-0161' 'F' 'DBH' '.1-.3'
'EPA01-0161' 'F' 'DISTANCE' '2'
'EPA01-0161' 'F' 'HEIGHT' '5-15'
'EPA01-0161' 'F' 'SPECIES' 'POPLAR/COTTONWOOD'
'EPA01-0161' 'F' 'TREE_TYP' 'Deciduous'
'EPA01-0161' 'G' 'DBH' '.1-.3'
'EPA01-0161' 'G' 'DISTANCE' '1'
'EPA01-0161' 'G' 'HEIGHT' '5-15'
'EPA01-0161' 'G' 'SPECIES' 'POPLAR/COTTONWOOD'
'EPA01-0161' 'G' 'TREE_TYP' 'Deciduous'
'EPA01-0161' 'H' 'DBH' '.1-.3'
'EPA01-0161' 'H' 'DISTANCE' '1'
'EPA01-0161' 'H' 'HEIGHT' '5-15'
'EPA01-0161' 'H' 'SPECIES' 'POPLAR/COTTONWOOD'
'EPA01-0161' 'H' 'TREE_TYP' 'Deciduous'
'EPA01-0161' 'I' 'DBH' '.1-.3'
'EPA01-0161' 'I' 'DISTANCE' '1'
'EPA01-0161' 'I' 'HEIGHT' '5-15'
'EPA01-0161' 'I' 'SPECIES' 'POPLAR/COTTONWOOD'
'EPA01-0161' 'I' 'TREE_TYP' 'Deciduous'
'EPA01-0161' 'J' 'DBH' '.1-.3'
'EPA01-0161' 'J' 'DISTANCE' '1'
'EPA01-0161' 'J' 'HEIGHT' '5-15'
'EPA01-0161' 'J' 'SPECIES' 'POPLAR/COTTONWOOD'
'EPA01-0161' 'J' 'TREE_TYP' 'Deciduous'
'EPA01-0161' 'K' 'DBH' '.1-.3'
'EPA01-0161' 'K' 'DISTANCE' '2'
'EPA01-0161' 'K' 'HEIGHT' '5-15'
'EPA01-0161' 'K' 'SPECIES' 'POPLAR/COTTONWOOD'
'EPA01-0161' 'K' 'TREE_TYP' 'Deciduous'
'EPA01-0167' 'A' 'DBH' '.3-.75'
'EPA01-0167' 'A' 'DISTANCE' '16'
'EPA01-0167' 'A' 'HEIGHT' '15-30'
'EPA01-0167' 'A' 'SPECIES' 'FIR'
'EPA01-0167' 'A' 'TREE_TYP' 'Coniferous'
'EPA01-0167' 'B' 'DBH' '.3-.75'
'EPA01-0167' 'B' 'DISTANCE' '4'
'EPA01-0167' 'B' 'HEIGHT' '15-30'
'EPA01-0167' 'B' 'SPECIES' 'FIR'
'EPA01-0167' 'B' 'TREE_TYP' 'Coniferous'
'EPA01-0167' 'C' 'DBH' '.3-.75'
'EPA01-0167' 'C' 'DISTANCE' '2'
'EPA01-0167' 'C' 'HEIGHT' '15-30'
'EPA01-0167' 'C' 'SPECIES' 'FIR'
'EPA01-0167' 'C' 'TREE_TYP' 'Coniferous'
'EPA01-0167' 'D' 'DBH' '.3-.75'
'EPA01-0167' 'D' 'DISTANCE' '14'
'EPA01-0167' 'D' 'HEIGHT' '15-30'
'EPA01-0167' 'D' 'SPECIES' 'FIR'
'EPA01-0167' 'D' 'TREE_TYP' 'Coniferous'
'EPA01-0167' 'E' 'DBH' '.3-.75'
'EPA01-0167' 'E' 'DISTANCE' '12'
'EPA01-0167' 'E' 'HEIGHT' '15-30'
'EPA01-0167' 'E' 'SPECIES' 'FIR'
'EPA01-0167' 'E' 'TREE_TYP' 'Coniferous'
'EPA01-0167' 'F' 'DBH' '.3-.75'
'EPA01-0167' 'F' 'DISTANCE' '12'
'EPA01-0167' 'F' 'HEIGHT' '15-30'
'EPA01-0167' 'F' 'SPECIES' 'FIR'
'EPA01-0167' 'F' 'TREE_TYP' 'Coniferous'
'EPA01-0167' 'G' 'DBH' '.3-.75'
'EPA01-0167' 'G' 'DISTANCE' '10'
'EPA01-0167' 'G' 'HEIGHT' '15-30'
'EPA01-0167' 'G' 'SPECIES' 'FIR'
'EPA01-0167' 'G' 'TREE_TYP' 'Coniferous'
'EPA01-0167' 'H' 'DBH' '.3-.75'
'EPA01-0167' 'H' 'DISTANCE' '1'
'EPA01-0167' 'H' 'HEIGHT' '5-15'
'EPA01-0167' 'H' 'SPECIES' 'FIR'
'EPA01-0167' 'H' 'TREE_TYP' 'Coniferous'
'EPA01-0167' 'I' 'DBH' '.3-.75'
'EPA01-0167' 'I' 'DISTANCE' '7'
'EPA01-0167' 'I' 'HEIGHT' '15-30'
'EPA01-0167' 'I' 'SPECIES' 'FIR'
'EPA01-0167' 'I' 'TREE_TYP' 'Coniferous'
'EPA01-0167' 'J' 'DBH' '.3-.75'
'EPA01-0167' 'J' 'DISTANCE' '20'
'EPA01-0167' 'J' 'HEIGHT' '15-30'
'EPA01-0167' 'J' 'SPECIES' 'FIR'
'EPA01-0167' 'J' 'TREE_TYP' 'Coniferous'
'EPA01-0167' 'K' 'DBH' '.3-.75'
'EPA01-0167' 'K' 'DISTANCE' '5'
'EPA01-0167' 'K' 'HEIGHT' '15-30'
'EPA01-0167' 'K' 'SPECIES' 'FIR'
'EPA01-0167' 'K' 'TREE_TYP' 'Coniferous'"
)
testData <-read.table(fred, header=TRUE, stringsAsFactors=FALSE)
close(fred)
return(testData)
}


metsLegacyTree.testResults <- function()
# creates dataframe of legacy tree metrics calculation results for unit test
##  Test metrics from SAS are corrected for known errors.
{
bob <- textConnection(
"UID METRIC RESULT
'EPA01-0124' 'ltfracl' '1'
'EPA01-0124' 'ltfracm' '1'
'EPA01-0124' 'ltfracs' '1'
'EPA01-0124' 'ltfracx' '0'
'EPA01-0124' 'ltmddist' '1'
'EPA01-0124' 'ltmddom' 'Firs/Hemlock'
'EPA01-0124' 'ltmddomn' '11'
'EPA01-0124' 'ltmxcnt' '10'
'EPA01-0124' 'ltmxdbh' '.3-.75'
'EPA01-0124' 'ltmxdist' '1'
'EPA01-0124' 'ltmxht' '>30'
'EPA01-0124' 'ltmxsize' 'L'
'EPA01-0124' 'ltmxspp' 'Firs/Hemlock'
'EPA01-0124' 'ltsplist' 'FIR,FIR,FIR,FIR,FIR,FIR,FIR,FIR,FIR,FIR,FIR'
'EPA01-0144' 'ltfracl' '0.90909090909'
'EPA01-0144' 'ltfracm' '1'
'EPA01-0144' 'ltfracs' '1'
'EPA01-0144' 'ltfracx' '0'
'EPA01-0144' 'ltmddist' '5.85'
'EPA01-0144' 'ltmddom' 'Spruce'
'EPA01-0144' 'ltmddomn' '7'
'EPA01-0144' 'ltmdsub' 'Firs/Hemlock'
'EPA01-0144' 'ltmdsubn' '3'
'EPA01-0144' 'ltmxcnt' '10'
'EPA01-0144' 'ltmxdbh' '.3-.75'
'EPA01-0144' 'ltmxdist' '1'
'EPA01-0144' 'ltmxht' '>30'
'EPA01-0144' 'ltmxsize' 'L'
'EPA01-0144' 'ltmxspp' 'Spruce'
'EPA01-0144' 'ltsplist' 'SPRUCE,LODGEPOLE PINE,SPRUCE,SPRUCE,SUBALPINE FIR,SUBALPINE FIR,SPRUCE,SPRUCE,SPRUCE,SUBALPINE FIR,SPRUCE'
'EPA01-0147' 'ltfracl' '0.81818181818'
'EPA01-0147' 'ltfracm' '1'
'EPA01-0147' 'ltfracs' '1'
'EPA01-0147' 'ltfracx' '0.2727272727'
'EPA01-0147' 'ltmddist' '2.277777778'
'EPA01-0147' 'ltmddom' 'Cedar/Cypress/Sequoia'
'EPA01-0147' 'ltmddomn' '8'
'EPA01-0147' 'ltmdsub' 'Firs/Hemlock'
'EPA01-0147' 'ltmdsubn' '3'
'EPA01-0147' 'ltmxcnt' '3'
'EPA01-0147' 'ltmxdbh' '.75-2'
'EPA01-0147' 'ltmxdist' '2'
'EPA01-0147' 'ltmxht' '>30'
'EPA01-0147' 'ltmxsize' 'X'
'EPA01-0147' 'ltmxspp' 'Cedar/Cypress/Sequoia'
'EPA01-0147' 'ltsplist' 'WESTERN RED CEDAR,WESTERN RED CEDAR,HEMLOCK,WESTERN RED CEDAR,WESTERN RED CEDAR,HEMLOCK,WESTERN RED CEDAR,HEMLOCK,WESTERN RED CEDAR,WESTERN RED CEDAR,WESTERN RED CEDAR'
'EPA01-0159' 'ltfracl' '0'
'EPA01-0159' 'ltfracm' '0.454545454545'
'EPA01-0159' 'ltfracs' '1'
'EPA01-0159' 'ltfracx' '0'
'EPA01-0159' 'ltmddist' '8.5'
'EPA01-0159' 'ltmddom' 'Willow'
'EPA01-0159' 'ltmddomn' '7'
'EPA01-0159' 'ltmdsub' 'Poplar/Cottonwood'
'EPA01-0159' 'ltmdsubn' '4'
'EPA01-0159' 'ltmxcnt' '5'
'EPA01-0159' 'ltmxdbh' '.1-.3'
'EPA01-0159' 'ltmxdist' '15'
'EPA01-0159' 'ltmxht' '5-15'
'EPA01-0159' 'ltmxsize' 'M'
'EPA01-0159' 'ltmxspp' 'Poplar/Cottonwood'
'EPA01-0159' 'ltsplist' 'WILLOW,ASPEN,ASPEN,ASPEN,ASPEN,WILLOW,WILLOW,WILLOW,WILLOW,WILLOW,WILLOW'
'EPA01-0161' 'ltfracl' '0'
'EPA01-0161' 'ltfracm' '1'
'EPA01-0161' 'ltfracs' '1'
'EPA01-0161' 'ltfracx' '0'
'EPA01-0161' 'ltmddist' '3'
'EPA01-0161' 'ltmddom' 'Poplar/Cottonwood'
'EPA01-0161' 'ltmddomn' '10'
'EPA01-0161' 'ltmdsub' 'Alder/Birch'
'EPA01-0161' 'ltmdsubn' '1'
'EPA01-0161' 'ltmxcnt' '11'
'EPA01-0161' 'ltmxdbh' '.1-.3'
'EPA01-0161' 'ltmxdist' '5'
'EPA01-0161' 'ltmxht' '5-15'
'EPA01-0161' 'ltmxsize' 'M'
'EPA01-0161' 'ltmxspp' 'Alder/Birch'
'EPA01-0161' 'ltsplist' 'ALDER/BIRCH,POPLAR/COTTONWOOD,POPLAR/COTTONWOOD,POPLAR/COTTONWOOD,POPLAR/COTTONWOOD,POPLAR/COTTONWOOD,POPLAR/COTTONWOOD,POPLAR/COTTONWOOD,POPLAR/COTTONWOOD,POPLAR/COTTONWOOD,POPLAR/COTTONWOOD'
'EPA01-0167' 'ltfracl' '0.90909090909'
'EPA01-0167' 'ltfracm' '1'
'EPA01-0167' 'ltfracs' '1'
'EPA01-0167' 'ltfracx' '0'
'EPA01-0167' 'ltmddist' '9.363636364'
'EPA01-0167' 'ltmddom' 'Firs/Hemlock'
'EPA01-0167' 'ltmddomn' '11'
'EPA01-0167' 'ltmxcnt' '10'
'EPA01-0167' 'ltmxdbh' '.3-.75'
'EPA01-0167' 'ltmxdist' '16'
'EPA01-0167' 'ltmxht' '15-30'
'EPA01-0167' 'ltmxsize' 'L'
'EPA01-0167' 'ltmxspp' 'Firs/Hemlock'
'EPA01-0167' 'ltsplist' 'FIR,FIR,FIR,FIR,FIR,FIR,FIR,FIR,FIR,FIR,FIR'"
)

testResults <-read.table(bob, header=TRUE, stringsAsFactors=FALSE)
close(bob)
return(testResults)
}