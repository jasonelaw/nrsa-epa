# valStructFishCov.r
#
# 09/24/09 cws Created valStructChancov
# 10/02/09  SSR adapted for tblFISHCOVER2
# 10/21/09 cws Added unit test.  Modified structure check to fill holes and add
#          timing argument to stVal*() calls.
#  2/25/10 cws moved source() calls to NRSAvalidation.r
#  5/03/10 cws Changed boatable UNDERCUT to UNDCUT to match wadeable values and
#          parameter metadata: removed UNDERCUT from check for unexpected
#          parameters in entire table, changed UNDERCUT to UNDCUT in check
#          for absent parameters in boatable portion of data.  Updated unit
#          test accordingly.

# Contains functions valStructFishCov 

require(RODBC)
#intermediateMessages <- TRUE

valStructFishCov <- function(df, test='all')
# Performs structure checks on the NRSA table tblCHANCOV2.  Returns NULL if
# no errors have been found, or an Nx1 character matrix describing the detected
# errors.
#
# ARGUMENTS:
# df        dataframe of NRSA fish cover data
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
  intermediateMessage('Structure validation of fish cover data ', loc='start')

  timeThis <- FALSE
  probs <- NULL

  # sanity checks
  if(!(test %in% c('all', 'vital', 'nonvital', 'synopsis'))) {
      probs<-"Error: test argument must be 'all', 'vital', 'nonvital' or 'synopsis'"
      return(as.matrix(probs))
  }
  
  # Check for correct column names no matter what, and stop early if there
  # are any errors, since nothing else will work without them
  intermediateMessage('.1')
  probs <- stValColumnPresence(df, c('UID', 'TRANSECT'
                                    ,'PARAMETER', 'RESULT', 'FLAG'
                                    ,'SAMPLE_TYPE'
                                    )
                              ,timing=timeThis
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
      pp <- stValMissingValues(df, 'UID', timing=timeThis)
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

      
      # Check for missing TRANSECT values
      intermediateMessage('.3')
      pp <- stValMissingValues(df, 'TRANSECT', timing=timeThis)
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,ifelse(is.null(pp)
                               ,"No Missing TRANSECT values"
                               ,"Missing TRANSECT values exist (vital)"
                               )
                        )
      } else {
          probs <- rbind(probs, pp)
      }


      # Check for missing PARAMETER values
      intermediateMessage('.4')
      pp <- stValMissingValues(df, 'PARAMETER', timing=timeThis)
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
      intermediateMessage('.5')
      pp <- stValMissingValues(df, 'SAMPLE_TYPE', timing=timeThis)
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


      # Check for unexpected TRANSECT values
      intermediateMessage('.6')
      pp <- stValUnexpectedValues(df,'TRANSECT'
                                 ,c('A','B','C','D','E','F','G','H','I','J','K'
                                   ,'XA','XB','XC','XD','XE','XF','XG','XH'
                                   ,'XI','XJ','XK'
                                   )
                                 ,'UID'
                                 ,timing=timeThis
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Unexpected TRANSECT values: %d (vital)"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }


      # Check for uniqueness of each UID-TRANSECT-PARAMETER
      intermediateMessage('.7')
      pp <- stValCountRows(df, c('UID','TRANSECT', 'PARAMETER'), 1, timing=timeThis)
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Duplicate UID-TRANSECT-PARAMETER values: %d (vital)"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }


  }
  
  # Perform nonvital tests if requested
  if(test %in% c('all','nonvital','synopsis')) {

      # Check for absent TRANSECT values at each UID
      intermediateMessage('.8')
      pp <- stValAbsentValues(df, 'TRANSECT'
                             ,c('A','B','C','D','E','F','G','H','I','J','K')
                             ,'UID'
                             ,timing=timeThis
                             )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Absent TRANSECT values: %d"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }

      # Check for unexpected PARAMETER values
      intermediateMessage('.9')
      pp <- stValUnexpectedValues(df
                                 ,'PARAMETER'
                                 ,c('ALGAE', 'BOULDR', 'BRUSH', 'LVTREE',
                                    'MACPHY', 'OVRHNG', 'STRUCT', 'UNDCUT',
                                    'WOODY'
                                   )
                                 ,c('UID', 'TRANSECT')
                                 ,timing=timeThis
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Unexpected PARAMETER values: %d"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }

      # Check for absent PARAMETER values - streams
      intermediateMessage('.10')
      pp <- stValAbsentValues(subset(df, SAMPLE_TYPE == 'PHAB_CHANW')
                             ,'PARAMETER'
                             ,c('ALGAE', 'BOULDR', 'BRUSH', 'LVTREE',
                                'MACPHY', 'OVRHNG', 'STRUCT', 'UNDCUT',
                                'WOODY')
                            ,c('UID', 'TRANSECT')
                            ,timing=timeThis
                            )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Absent stream PARAMETER values: %d"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }

      # Check for absent PARAMETER values - rivers
      intermediateMessage('.11')
      pp <- stValAbsentValues(subset(df, SAMPLE_TYPE == 'PHAB_CHANB')
                             ,'PARAMETER'
                             ,c('ALGAE', 'BOULDR', 'BRUSH', 'LVTREE',
                                'MACPHY', 'OVRHNG', 'STRUCT',
                                'UNDCUT', 'WOODY')
                            ,c('UID', 'TRANSECT')
                            ,timing=timeThis
                            )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Absent river PARAMETER values: %d"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }

      # Check for unexpected values of SAMPLE_TYPE.
      intermediateMessage('.12')
      pp <- stValUnexpectedValues(df
                                 ,'SAMPLE_TYPE'
                                 ,c('PHAB_CHANW', 'PHAB_CHANB')
                                 ,c('UID', 'TRANSECT')
                                 ,timing=timeThis
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




valStructFishCovTest <- function()
# Tests valStructFishCov()
{

  # Create correctly formated test data
  baseData <- rbind(expand.grid(UID = 1:10
                               ,TRANSECT = LETTERS[1:11]
                               ,SAMPLE_TYPE = 'PHAB_CHANB'
                               ,PARAMETER = c('ALGAE', 'BOULDR', 'BRUSH'
                                             ,'LVTREE', 'MACPHY', 'OVRHNG'
                                             ,'STRUCT', 'UNDCUT', 'WOODY'
                                             )
                               )
                   ,expand.grid(UID = 11:20
                               ,TRANSECT = c(LETTERS[1:11]
                                            ,paste('X', LETTERS[1:10], sep='')
                                            )
                               ,SAMPLE_TYPE = 'PHAB_CHANW'
                               ,PARAMETER = c('ALGAE', 'BOULDR', 'BRUSH'
                                             ,'LVTREE', 'MACPHY', 'OVRHNG'
                                             ,'STRUCT', 'UNDCUT', 'WOODY'
                                             )
                               )
                   )
  baseData$UID <- as.character(baseData$UID)
  baseData$TRANSECT <- as.character(baseData$TRANSECT)
  baseData$SAMPLE_TYPE <- as.character(baseData$SAMPLE_TYPE)
  baseData$PARAMETER <- as.character(baseData$PARAMETER)
  baseData$RESULT <- rep(as.character(0:4), length.out=nrow(baseData))
  baseData$FLAG <- as.character(NA)
  
  # Create incorrectly formated test data base based on the good data
  realData <- baseData
  
  #    Make values missing, which will also make other values appear as absent
  #    and unexpected.
  realData[realData$UID==1,][1,]$UID <- ''
  realData[realData$UID==1,][2,]$TRANSECT <- ''
  realData[realData$UID==1,][3,]$PARAMETER <- ''
  realData[realData$UID==1,][4,]$SAMPLE_TYPE <- ''
  
  #    Make values absent, which will cause duplicates to appear
  realData[realData$UID == 2 & realData$TRANSECT=='A' & realData$PARAMETER=='ALGAE',]$TRANSECT<-'C'
  realData[realData$UID == 2 & realData$TRANSECT=='B' & realData$PARAMETER=='ALGAE',]$PARAMETER<-'BOULDR'
  realData[realData$UID == 12 & realData$TRANSECT=='A' & realData$PARAMETER=='ALGAE',]$PARAMETER<-'WOODY'

  #    Make unexpected values, which will cause some to be absent
  realData[realData$UID == 3 & realData$TRANSECT=='A' & realData$PARAMETER=='ALGAE',]$TRANSECT<-'XB'
  realData[realData$UID == 3 & realData$TRANSECT=='B' & realData$PARAMETER=='ALGAE',]$PARAMETER<-'WRONG'

  # Look for structure errors where there are none
  rr <- valStructFishCov(baseData, test='all')
  checkEquals(NULL, rr
             ,"Error: Detected structure errors where there are none"
             )
  
  # Look for structure errors where known ones exist.
  rr <- valStructFishCov(realData, test='all')
  ee <- rbind("Column UID has 1 missing values"
             ,"Column TRANSECT has 1 missing values"
             ,"Column PARAMETER has 1 missing values"
             ,"Column SAMPLE_TYPE has 1 missing values"
             ,"Unexpected value TRANSECT=() at (UID)=(1)"
             ,"Unexpected row count at (UID,TRANSECT,PARAMETER)=(2,C,ALGAE), n=2"
             ,"Unexpected row count at (UID,TRANSECT,PARAMETER)=(2,B,BOULDR), n=2"
             ,"Unexpected row count at (UID,TRANSECT,PARAMETER)=(12,A,WOODY), n=2"
             ,"Absent TRANSECT=(B,C,D,E,F,G,H,I,J,K) value at UID=() "
             ,"Unexpected value PARAMETER=(WRONG) at (UID,TRANSECT)=(3,B)"
             ,"Unexpected value PARAMETER=() at (UID,TRANSECT)=(1,D)"
             ,"Absent PARAMETER=(ALGAE) value at UID,TRANSECT=(12,A) "
             ,"Absent PARAMETER=(BOULDR,BRUSH,LVTREE,MACPHY,OVRHNG,STRUCT,UNDCUT,WOODY) value at UID,TRANSECT=(,A) "
             ,"Absent PARAMETER=(BOULDR,BRUSH,LVTREE,MACPHY,OVRHNG,STRUCT,UNDCUT,WOODY) value at UID,TRANSECT=(1,) "
             ,"Absent PARAMETER=(ALGAE) value at UID,TRANSECT=(1,A) "
             ,"Absent PARAMETER=(ALGAE) value at UID,TRANSECT=(1,C) "
             ,"Absent PARAMETER=(ALGAE) value at UID,TRANSECT=(1,D) "
             ,"Absent PARAMETER=(ALGAE) value at UID,TRANSECT=(1,E) "
             ,"Absent PARAMETER=(ALGAE) value at UID,TRANSECT=(2,A) "
             ,"Absent PARAMETER=(ALGAE) value at UID,TRANSECT=(2,B) "
             ,"Absent PARAMETER=(ALGAE) value at UID,TRANSECT=(3,A) "
             ,"Absent PARAMETER=(ALGAE) value at UID,TRANSECT=(3,B) "
             ,"Absent PARAMETER=(BOULDR,BRUSH,LVTREE,MACPHY,OVRHNG,STRUCT,UNDCUT,WOODY) value at UID,TRANSECT=(3,XB) "
             ,"Unexpected value SAMPLE_TYPE=() at (UID,TRANSECT)=(1,E)"
             )
  checkEquals(ee, rr
             ,"Error: Failed to detect structure errors where they exist"
             )

}

# end of file