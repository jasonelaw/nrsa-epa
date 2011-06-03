# valStructDischarge.r
#
# 09/24/09 cws Created valStructChancov
# 10/02/09  SSR adapted for tblDISCHARGE2
# 10/21/09 cws Added unit test.  Modified structure check to include tests for
#          nonunique UID-REP-LINE-PARAMETER-SAMPLE_TYPE combinations and a
#          check for unexpected SAMPLE_TYPE
#  2/25/2010 cws moved source() calls to NRSAvalidation.r
#

# Contains functions valStructDischarge 

require(RODBC)
#intermediateMessages <- TRUE

valStructDischarge <- function(df, test='all')
# Performs structure checks on the NRSA table tblDISCHARGE2.  Returns NULL if
# no errors have been found, or an Nx1 character matrix describing the detected
# errors.
#
# ARGUMENTS:
# df        dataframe of NRSA discharge data
# test      String describing which tests to perform.  May be one of the
#             following:
#             'all'       default, performs all tests
#             'vital'     performs only vital tests
#             'nonvital'  performs only nonvital tests
#             'synopsis'  performs all tests, returning counts of detected
#                           errors for all tests
#
# ASSUMPTIONS:
#
{
  intermediateMessage('Structure validation of stream discharge data ', loc='start')
  
  probs <- NULL

  # sanity checks
  if(!(test %in% c('all', 'vital', 'nonvital', 'synopsis'))) {
      probs<-"Error: test argument must be 'all', 'vital', 'nonvital' or 'synopsis'"
      return(as.matrix(probs))
  }
  
  # Check for correct column names no matter what, and stop early if there
  # are any errors, since nothing else will work without them
  intermediateMessage('.1')
  probs <- stValColumnPresence(df, c('UID', 'REP', 'LINE', 'METHOD'
                                    ,'PARAMETER', 'RESULT', 'UNITS', 'FLAG'
                                    ,'SAMPLE_TYPE'
                                    )
                              )
  if(!is.null(probs)) {
      colnames(probs)[1] <-'User.MUST.fix.these.errors.before.proceding'
      intermediateMessage('.  Done (terminated early).', loc='end')
      return(probs)
  }
  
  #  Perform 'vital' tests if requested
  if(test %in% c('all','vital','synopsis')) {

      # Check for missing UID values
      intermediateMessage('.2')
      pp <- stValMissingValues(df, 'UID')
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,ifelse(is.null(pp)
                               ,"No Missing UID values"
                               ,"Missing UID values exist (vital)"
                               )
                        )
      } else {
          probs <- rbind(probs, pp)
      }

      
      # Check for missing REP values
      intermediateMessage('.3')
      pp <- stValMissingValues(df, 'REP')
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,ifelse(is.null(pp)
                               ,"No Missing REP values"
                               ,"Missing REP values exist (vital)"
                               )
                        )
      } else {
          probs <- rbind(probs, pp)
      }


      # Check for missing LINE values
      intermediateMessage('.4')
      pp <- stValMissingValues(df, 'LINE')
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,ifelse(is.null(pp)
                               ,"No Missing LINE values"
                               ,"Missing LINE values exist (vital)"
                               )
                        )
      } else {
          probs <- rbind(probs, pp)
      }


      # Check for missing PARAMETER values
      intermediateMessage('.5')
      pp <- stValMissingValues(df, 'PARAMETER')
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,ifelse(is.null(pp)
                               ,"No Missing PARAMETER values"
                               ,"Missing PARAMETER values exist (vital)"
                               )
                        )
      } else {
          probs <- rbind(probs, pp)
      }


      # Check for missing SAMPLE_TYPE values
      intermediateMessage('.6')
      pp <- stValMissingValues(df, 'SAMPLE_TYPE')
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,ifelse(is.null(pp)
                               ,"No Missing SAMPLE_TYPE values"
                               ,"Missing SAMPLE_TYPE values exist (vital)"
                               )
                        )
      } else {
          probs <- rbind(probs, pp)
      }


      # Check for missing METHOD values
      intermediateMessage('.6')
      pp <- stValMissingValues(df, 'METHOD')
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,ifelse(is.null(pp)
                               ,"No Missing METHOD values"
                               ,"Missing METHOD values exist (vital)"
                               )
                        )
      } else {
          probs <- rbind(probs, pp)
      }


      # Check for unexpected REP values
      intermediateMessage('.7')
      pp <- stValUnexpectedValues(df, 'REP'
                                 ,c('1', '2', '3', '4', '5', '99')
                                 ,'UID'
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Unexpected REP values: %d (vital)"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }


      # Check for unexpected METHOD values
      intermediateMessage('.8')
      pp <- stValUnexpectedValues(df, 'METHOD'
                                 ,c('NBO','QVAL','TIMED FILLING','VELOCITY AREA')
                                 ,'UID'
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Unexpected METHOD values: %d (vital)"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }


      # Check for uniqueness of each UID-REP-LINE-PARAMETER
      intermediateMessage('.9')
      pp <- stValCountRows(df, c('UID','REP','LINE','PARAMETER'), 1)
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Duplicate UID-REP-LINE-PARAMETER values: %d (vital)"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }


  }
  
  # Perform nonvital tests if requested
  if(test %in% c('all','nonvital','synopsis')) {

      # Check for absent LINE values at each UID-REP : Velocity area
      intermediateMessage('.10')
      pp <- stValAbsentValues(subset(df, METHOD == 'VELOCITY AREA'), 'LINE'
                             ,c('1')
                             ,c('UID', 'REP')
                             )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Absent LINE values: %d"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }

      # Check for absent LINE values at each UID-REP : Timed fill and NBO
      intermediateMessage('.11')
      pp <- stValAbsentValues(subset(df, METHOD != 'VELOCITY AREA'), 'LINE'
                             ,c('999')
                             ,c('UID', 'REP')
                             )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Absent LINE values: %d"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }
      

      # Check for absent REP values at each UID-LINE: Velocity area
      intermediateMessage('.12')
      pp <- stValAbsentValues(subset(df, METHOD == 'VELOCITY AREA'), 'REP'
                             ,c('99')
                             ,c('UID', 'LINE')
                             )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Absent REP values: %d"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }


      # Check for absent REP values at each UID-LINE: Timed fill
      intermediateMessage('.13')
      pp <- stValAbsentValues(subset(df, METHOD == 'TIMED FILLING'), 'REP'
                             ,c('1', '2', '3', '4', '5')
                             ,c('UID', 'LINE')
                             )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Absent REP values: %d"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }


      # Check for absent REP values at each UID-LINE: NBO
      intermediateMessage('.14')
      pp <- stValAbsentValues(subset(df, METHOD == 'NBO'), 'REP'
                             ,c('1', '2', '3')
                             ,c('UID', 'LINE')
                             )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Absent REP values: %d"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }


      # Check for absent REP values at each UID-LINE: QVal
      intermediateMessage('.15')
      pp <- stValAbsentValues(subset(df, METHOD == 'QVAL'), 'REP'
                             ,c('99')
                             ,c('UID', 'LINE')
                             )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Absent REP values: %d"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }


      # Check for unexpected PARAMETER values
      intermediateMessage('.16')
      pp <- stValUnexpectedValues(subset(df, METHOD=='NBO')
                                 ,'PARAMETER'
                                 ,c('AVGWIDTH', 'DEPTH_1','DEPTH_2', 'DEPTH_3'
                                    ,'DEPTH_4', 'DEPTH_5','FLOAT', 'TOTTIME'
                                   )
                                 ,c('UID', 'REP')
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Unexpected PARAMETER values for NBO method: %d"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }

      intermediateMessage('.17')
      pp <- stValUnexpectedValues(subset(df, METHOD=='QVAL')
                                 ,'PARAMETER'
                                 ,'QVAL'
                                 ,'UID'
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Unexpected PARAMETER values for QVAL method: %d"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }

      intermediateMessage('.18')
      pp <- stValUnexpectedValues(subset(df, METHOD=='TIMED FILLING')
                                 ,'PARAMETER'
                                 ,c('TIME', 'VOLUME')
                                 ,c('UID', 'REP', 'LINE')
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Unexpected PARAMETER values for TIMED FILLING method: %d"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }

      intermediateMessage('.19')
      pp <- stValUnexpectedValues(subset(df, METHOD=='VELOCITY AREA')
                                 ,'PARAMETER'
                                 ,c('DEPTH', 'DISTBANK', 'VELOCITY')
                                 ,c('UID', 'REP', 'LINE')
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Unexpected PARAMETER values for VELOCITY AREA method: %d"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }

      # Check for absent PARAMETER values: Velocity area
      intermediateMessage('.20')
      pp <- stValAbsentValues(subset(df, METHOD == "VELOCITY AREA")
                                 ,'PARAMETER'
                                 ,c('DEPTH')#, 'DISTBANK', 'VELOCITY')
                                 ,c('UID', 'REP', 'LINE')
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Absent PARAMETER values: %d"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }

      # Check for absent PARAMETER values: Timed filling
      intermediateMessage('.21')
      pp <- stValAbsentValues(subset(df, METHOD == "TIMED FILLING")
                                 ,'PARAMETER'
                                 ,c('VOLUME', 'TIME')
                                 ,c('UID', 'REP', 'LINE')
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Absent PARAMETER values: %d"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }

      # Check for absent PARAMETER values: NBO
      intermediateMessage('.22')
      pp <- stValAbsentValues(subset(df, METHOD == "NBO")
                                 ,'PARAMETER'
                                 ,c('AVGWIDTH', 'DEPTH_1',
                                    'DEPTH_2', 'DEPTH_3', 'DEPTH_4', 'DEPTH_5',
                                    'FLOAT', 'TOTTIME')
                                 ,c('UID', 'REP', 'LINE')
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Absent PARAMETER values: %d"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }

      # Check for unexpected values of SAMPLE_TYPE.
      intermediateMessage('.23')
      pp <- stValUnexpectedValues(df
                                 ,'SAMPLE_TYPE'
                                 ,c('FLOW')
                                 ,c('UID', 'REP', 'LINE')
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Unexpected SAMPLE_TYPE values: %d"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }


  }

  intermediateMessage('.  Done', loc='end')
  return(probs)

}

valStructDischargeTest <- function ()
# Tests valStructDischarge
{
  # Create dataframe with no errors.  Since different discharge measurement
  # methods use different parameters and data structures, these data are built
  # up separately and then combined:
  #   NBO:           UID run from 1 to 10
  #   QVAL:          UID run from 11 to 20
  #   TIMED FILLING: UID run from 21 to 30
  #   VELOCITY AREA: UID run from 31 to 40, and will have LINE range from 1 to
  #                    UID - 20, where UID is 31-40, as allowed in the protocol.
  #
  # Even UID use metric, odd UID use imperial dimensions when allowed.
  #
  baseNBO <- expand.grid(UID = 1:10
                        ,REP = 1:3
                        ,PARAMETER = c('AVGWIDTH','DEPTH_1','DEPTH_2','DEPTH_3'
                                       ,'DEPTH_4','DEPTH_5','FLOAT','TOTTIME'
                                       )
                        )
  attr(baseNBO,'out.attrs') <- NULL
  baseNBO$UID <- as.character(baseNBO$UID)
  baseNBO$PARAMETER <- as.character(baseNBO$PARAMETER)
  baseNBO$LINE <- as.integer(999)
  baseNBO$METHOD <- 'NBO'
  baseNBO[baseNBO$PARAMETER == 'AVGWIDTH','RESULT'] <- 5
  baseNBO[baseNBO$PARAMETER == 'AVGWIDTH' & as.integer(baseNBO$UID) %% 2 == 0
         ,'UNITS'] <-'m'
  baseNBO[baseNBO$PARAMETER == 'AVGWIDTH' & as.integer(baseNBO$UID) %% 2 == 1
         ,'UNITS'] <-'ft'
  baseNBO[substr(baseNBO$PARAMETER,1,5) =='DEPTH','RESULT'] <- 1
  baseNBO[substr(baseNBO$PARAMETER,1,5) =='DEPTH' & as.integer(baseNBO$UID) %% 2 == 0
         ,'UNITS'] <-'m'
  baseNBO[substr(baseNBO$PARAMETER,1,5) =='DEPTH' & as.integer(baseNBO$UID) %% 2 == 1
         ,'UNITS'] <-'ft'
  baseNBO[substr(baseNBO$PARAMETER,1,5) =='FLOAT','RESULT'] <- 20
  baseNBO[baseNBO$PARAMETER == 'FLOAT' & as.integer(baseNBO$UID) %% 2 == 0
         ,'UNITS'] <-'m'
  baseNBO[baseNBO$PARAMETER == 'FLOAT' & as.integer(baseNBO$UID) %% 2 == 1
         ,'UNITS'] <-'ft'
  baseNBO[baseNBO$PARAMETER =='TOTTIME','RESULT'] <- 60
  baseNBO[baseNBO$PARAMETER =='TOTTIME','UNITS'] <- 'SEC'
  baseNBO$FLAG <- as.character(NA)
  attr(baseNBO,'out.attrs') <- NULL
  baseNBO <- baseNBO[c(1,2,4,5,3,6,7,8)]
  
  baseQVAL <- data.frame(UID = as.character(11:20), stringsAsFactors=FALSE)
  baseQVAL$REP <- as.integer(99)
  baseQVAL$LINE <- as.integer(999)
  baseQVAL$METHOD <- 'QVAL'
  baseQVAL$PARAMETER <- 'QVAL'
  baseQVAL$RESULT <-  3
  baseQVAL$UNITS <- 'cfs'
  baseQVAL$FLAG <- as.character(NA)
  
  baseTF <- expand.grid(UID = 21:30
                       ,REP = 1:5
                       ,PARAMETER = c('TIME','VOLUME')
                       )
  attr(baseTF,'out.attrs') <- NULL
  baseTF$UID <- as.character(baseTF$UID)
  baseTF$PARAMETER <- as.character(baseTF$PARAMETER)
  baseTF$LINE <- as.integer(999)
  baseTF$METHOD <- 'TIMED FILLING'
  baseTF[baseTF$PARAMETER == 'TIME', 'RESULT'] <- 2
  baseTF[baseTF$PARAMETER == 'TIME', 'UNITS'] <- 'SEC'
  baseTF[baseTF$PARAMETER == 'TIME', 'RESULT'] <- 1
  baseTF[baseTF$PARAMETER == 'TIME', 'UNITS'] <- 'L'
  baseTF$FLAG <- as.character(NA)
  baseTF <- baseTF[c(1,2,4,5,3,6,7,8)]

  baseVA <- expand.grid(UID = 31:40
                       ,LINE = 1:20
                       ,PARAMETER = c('DEPTH','DISTBANK','VELOCITY')
                       )
  attr(baseVA,'out.attrs') <- NULL
  baseVA$UID <- as.character(baseVA$UID)
  baseVA$PARAMETER <- as.character(baseVA$PARAMETER)
  baseVA$REP <- as.integer(99)
  baseVA$METHOD <- 'VELOCITY AREA'
  baseVA[baseVA$PARAMETER == 'DEPTH','RESULT'] <- 5
  baseVA[baseVA$PARAMETER == 'DEPTH' & as.integer(baseVA$UID) %% 2 == 0
         ,'UNITS'] <-'m'
  baseVA[baseVA$PARAMETER == 'DEPTH' & as.integer(baseVA$UID) %% 2 == 1
         ,'UNITS'] <-'ft'
  baseVA[baseVA$PARAMETER == 'DISTBANK','RESULT'] <- 20
  baseVA[baseVA$PARAMETER == 'DISTBANK' & as.integer(baseVA$UID) %% 2 == 0
         ,'UNITS'] <-'m'
  baseVA[baseVA$PARAMETER == 'DISTBANK' & as.integer(baseVA$UID) %% 2 == 1
         ,'UNITS'] <-'ft'
  baseVA[baseVA$PARAMETER == 'VELOCITY','RESULT'] <- 0.4
  baseVA[baseVA$PARAMETER == 'VELOCITY' & as.integer(baseVA$UID) %% 2 == 0
         ,'UNITS'] <-'m/s'
  baseVA[baseVA$PARAMETER == 'VELOCITY' & as.integer(baseVA$UID) %% 2 == 1
         ,'UNITS'] <-'ft/s'
  baseVA$FLAG <- as.character(NA)
  baseVA <- subset(baseVA, LINE <= as.integer(UID) - 20)
  baseVA <- baseVA[c(1,4,2,5,3,6,7,8)]

  baseTest <- rbind(baseNBO, baseQVAL, baseTF, baseVA)
  baseTest$SAMPLE_TYPE <- 'FLOW'


  # Create dataframe with structural problems based on correct data
  realTest <- baseTest
  
  #    Make some keys missing, which will also turn up as unexpected and absent
  realTest[1,]$UID <- ''
  realTest[2,]$REP <- ''
  realTest[3,]$LINE <- ''
  realTest[4,]$PARAMETER <- ''
  realTest[5,]$SAMPLE_TYPE <- ''
  realTest[6,]$METHOD <- ''

  #    Make some unexpected keys, which will make some absent as well.
  realTest[realTest$METHOD == 'NBO',][1,]$PARAMETER <- 'DEPTH'
  realTest[realTest$METHOD == 'QVAL',][1,]$PARAMETER <- 'AVGWIDTH'
  realTest[realTest$METHOD == 'TIMED FILLING',][1,]$PARAMETER <- 'TOTTIME'
  realTest[realTest$METHOD == 'VELOCITY AREA',][1,]$PARAMETER <- 'DEPTH_1'

  realTest[realTest$METHOD == 'NBO',][1,]$LINE <- -1
  realTest[realTest$METHOD == 'QVAL',][1,]$LINE <- -1
  realTest[realTest$METHOD == 'TIMED FILLING',][1,]$LINE <- -1
  realTest[realTest$METHOD == 'VELOCITY AREA',][1,]$LINE <- -21

  realTest[100,]$SAMPLE_TYPE <- 'WRONG'
  realTest[101,]$METHOD <- 'GUESSING'

  #    Make some keys absent, which will also turn up as duplicates
  realTest[realTest$METHOD == 'NBO' &
           realTest$UID == 3 &
           realTest$REP == 1 &
           realTest$PARAMETER == 'DEPTH_5'
          ,]$REP <- 2
  realTest[realTest$METHOD == 'QVAL' &
           realTest$UID == 13
          ,]$UID <- 12
  realTest[realTest$METHOD == 'TIMED FILLING' &
           realTest$UID == 23 &
           realTest$REP == 1 &
           realTest$PARAMETER == 'VOLUME'
          ,]$REP <- 2
  realTest[realTest$METHOD == 'VELOCITY AREA' &
           realTest$UID == 33 &
           realTest$LINE == 3 &
           realTest$PARAMETER == 'VELOCITY'
          ,]$LINE <- 2


  # Test correct data to look for false positives
  rr <- valStructDischarge(baseTest, test='all')
  checkEquals(NULL, rr
             ,"Error: Structure check of correct data turned up errors"
             )

  # Test incorrect data to look for false negatives
  rr <- valStructDischarge(realTest, test='all')
  ee <- rbind("Column UID has 1 missing values"
             ,"Column REP has 1 missing values"
             ,"Column LINE has 1 missing values"
             ,"Column PARAMETER has 1 missing values"
             ,"Column SAMPLE_TYPE has 1 missing values"
             ,"Column METHOD has 1 missing values"
             ,"Unexpected value REP=() at (UID)=(2)"
             ,"Unexpected value METHOD=() at (UID)=(6)"
             ,"Unexpected value METHOD=(GUESSING) at (UID)=(1)"
             ,"Unexpected row count at (UID,REP,LINE,PARAMETER)=(3,2,999,DEPTH_5), n=2"
             ,"Unexpected row count at (UID,REP,LINE,PARAMETER)=(12,99,999,QVAL), n=2"
             ,"Unexpected row count at (UID,REP,LINE,PARAMETER)=(33,99,2,VELOCITY), n=2"
             ,"Unexpected row count at (UID,REP,LINE,PARAMETER)=(23,2,999,VOLUME), n=2"
             ,"Absent REP=(2,3,4,5) value at UID,LINE=(21,-1) "
             ,"Absent REP=(2,3) value at UID,LINE=(,-1) "
             ,"Absent REP=(2,3) value at UID,LINE=(3,) "
             ,"Unexpected value PARAMETER=(DEPTH) at (UID,REP)=(,1)"
             ,"Unexpected value PARAMETER=() at (UID,REP)=(4,1)"
             ,"Unexpected value PARAMETER=(AVGWIDTH) at (UID)=(11)"
             ,"Unexpected value PARAMETER=(TOTTIME) at (UID,REP,LINE)=(21,1,-1)"
             ,"Unexpected value PARAMETER=(DEPTH_1) at (UID,REP,LINE)=(31,99,-21)"
             ,"Absent PARAMETER=(DEPTH) value at UID,REP,LINE=(31,99,-21) "
             ,"Absent PARAMETER=(DEPTH) value at UID,REP,LINE=(31,99,1) "
             ,"Absent PARAMETER=(VOLUME,TIME) value at UID,REP,LINE=(21,1,-1) "
             ,"Absent PARAMETER=(AVGWIDTH,DEPTH_1,DEPTH_2,DEPTH_3,DEPTH_4,DEPTH_5,FLOAT,TOTTIME) value at UID,REP,LINE=(,1,-1) "
             ,"Absent PARAMETER=(DEPTH_1,DEPTH_2,DEPTH_3,DEPTH_4,DEPTH_5,FLOAT,TOTTIME) value at UID,REP,LINE=(3,1,) "
             ,"Unexpected value SAMPLE_TYPE=() at (UID,REP,LINE)=(5,1,999)"
             ,"Unexpected value SAMPLE_TYPE=(WRONG) at (UID,REP,LINE)=(10,1,999)"
             )
  checkEquals(ee, rr
             ,"Error: Structure check of correct data turned up errors"
             )
}

# end of file