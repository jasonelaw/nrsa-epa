# valStructChanDepth.r
#
# 10/19/09 cws Created
#  3/25/10 cws removed source() for nlaSupport.r (now sharedSupport.r) and
#          validation.r.
#
#source('l:/Priv/CORFiles/IM/Rwork/SharedCode/validation.r')
#source('l:/Priv/CORFiles/IM/Rwork/SharedCode/nlaSupport.r')
require(RODBC)
#intermediateMessages <- TRUE

valStructChanDepth <- function(df, test='all')
# Performs structure checks on the NRSA table tblCHANDEPTH2.  Returns NULL if
# no errors have been found, or an Nx1 character matrix describing the detected
# errors.
#
# ARGUMENTS:
# df        dataframe of NRSA canopy cover data
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
  intermediateMessage('Structure validation of channel depth data ', loc='start')

  probs <- NULL

  # sanity checks
  if(!(test %in% c('all', 'vital', 'nonvital', 'synopsis'))) {
      probs<-"Error: test argument must be 'all', 'vital', 'nonvital' or 'synopsis'"
      return(as.matrix(probs))
  }

  # Check for correct column names no matter what, and stop early if there
  # are any errors, since nothing else will work without them
  intermediateMessage('.1')
  probs <- stValColumnPresence(df, c('UID', 'TRANSECT', 'LINE', 'SAMPLE_TYPE'
                                    ,'PARAMETER', 'RESULT', 'UNITS', 'FLAG'
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


      # Check for missing TRANSECT values
      intermediateMessage('.3')
      pp <- stValMissingValues(df, 'TRANSECT')
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


      # Check for unexpected TRANSECT values
      intermediateMessage('.7')
      pp <- stValUnexpectedValues(df
                                 ,'TRANSECT'
                                 ,c('A','B','C','D','E','F','G','H','I','J','K')
                                 ,'UID'
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


      # Check for unexpected LINE values
      intermediateMessage('.8')
      pp <- stValUnexpectedValues(df
                                 ,'LINE'
                                 ,1:5
                                 ,c('UID', 'TRANSECT','SAMPLE_TYPE','PARAMETER')
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Unexpected LINE values: %d (vital)"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }


      # Check for uniqueness of each UID-TRANSECT-SAMPLE_TYPE-LINE-PARAMETER
      # where RESULT is not NA, so there is only one PARAMETER per line,
      # regardless of the value of PARAMETER.  Data is expected to look like this:
      #
      #       UID TRANSECT LINE     SAMPLE_TYPE PARAMETER RESULT UNITS FLAG
      #   1  9928        A    1 PHAB_CHANBFRONT      POLE     NA     m <NA>
      #   2  9928        A    1 PHAB_CHANBFRONT     SONAR    3.3     m <NA>
      #   3  9928        A    2 PHAB_CHANBFRONT      POLE     NA     m <NA>
      #   4  9928        A    2 PHAB_CHANBFRONT     SONAR    2.1     m <NA>
      #   5  9928        A    3 PHAB_CHANBFRONT      POLE     NA     m <NA>
      #   6  9928        A    3 PHAB_CHANBFRONT     SONAR    1.3     m <NA>
      #   7  9928        A    4 PHAB_CHANBFRONT      POLE     NA     m <NA>
      #   8  9928        A    4 PHAB_CHANBFRONT     SONAR    3.5     m <NA>

      intermediateMessage('.9')
      pp <- stValCountRows(subset(df, !is.na(RESULT))
                          ,c('UID','TRANSECT','SAMPLE_TYPE','LINE')
                          ,1
                          )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Duplicate UID-TRANSECT-TRANSDIR values: %d (vital)"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }


  }

  # Perform nonvital tests if requested
  if(test %in% c('all','nonvital','synopsis')) {

      # Check for unexpected SAMPLE_TYPE values
      intermediateMessage('.10')
      pp <- stValUnexpectedValues(df
                                 ,'SAMPLE_TYPE'
                                 ,c('PHAB_CHANBFRONT')
                                 ,c('UID', 'TRANSECT','LINE','PARAMETER')
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


      # Check for unexpected PARAMETER values
      intermediateMessage('.11')
      pp <- stValUnexpectedValues(df
                                 ,'PARAMETER'
                                 ,c('POLE','SONAR')
                                 ,c('UID', 'TRANSECT', 'SAMPLE_TYPE','LINE')
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

      # Check for absent TRANSECT values at each UID
      intermediateMessage('.12')
      pp <- stValAbsentValues(df, 'TRANSECT'
                             ,c('A','B','C','D','E','F','G','H','I','J','K')
                             ,'UID'
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

      # Check for absent PARAMETER values
      intermediateMessage('.13')
      pp <- stValAbsentValues(df
                                 ,'PARAMETER'
                                 ,c('POLE','SONAR')
                                 ,c('UID', 'TRANSECT')
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

  }

  intermediateMessage('.  Done', loc='end')
  return(probs)

}


valStructChanDepthTest <- function()
# Tests valStructChanDepth()
{
  # Create test data frame, odd UIDs done by pole, even UIDs done by sonar
  baseTest <- rbind(expand.grid(UID=as.character(1:10)
                               ,TRANSECT=LETTERS[1:11]
                               ,LINE=1:5
                               ,PARAMETER=c('POLE','SONAR')
                               )
                   )
  baseTest$UID <- as.character(baseTest$UID)
  baseTest$TRANSECT <- as.character(baseTest$TRANSECT)
  baseTest$PARAMETER <- as.character(baseTest$PARAMETER)
  baseTest$SAMPLE_TYPE='PHAB_CHANBFRONT'
  baseTest$RESULT <- floor(runif(nrow(baseTest), 0, 10))
  baseTest[(baseTest$PARAMETER == 'POLE' &
            as.integer(baseTest$UID) %% 2 == 0
           ) |
           (baseTest$PARAMETER == 'SONAR' &
            as.integer(baseTest$UID) %% 2 == 1
           )
          ,]$RESULT <- NA

  baseTest$UNITS <- 'm'
  baseTest$FLAG <- as.character(NA)
  attr(baseTest,'out.attrs') <- NULL  # get rid of odd attributes

  # Create imperfect/real dataframe to check for false negatives
  realTest <- baseTest

  #    Change some keys to NA, and make unexpected transect, parameter and
  #    sample_type values.
  realTest[realTest$UID=='1' & realTest$TRANSECT=='A',][1,]$UID <- ''
  realTest[realTest$UID=='1' & realTest$TRANSECT=='B',][1,]$TRANSECT <- ''
  realTest[realTest$UID=='1' & realTest$TRANSECT=='C',][1,]$LINE <- NA
  realTest[realTest$UID=='1' & realTest$TRANSECT=='D',][1,]$PARAMETER <- ''
  realTest[realTest$UID=='1' & realTest$TRANSECT=='E',][1,]$SAMPLE_TYPE <- ''

  #    Create some unexpected values, and absent transect & parameter values
  realTest[realTest$UID=='2' & realTest$TRANSECT=='A',][1,]$TRANSECT <- 'XA'
  realTest[realTest$UID=='2' & realTest$TRANSECT=='B',][1,]$LINE <- 12
  realTest[realTest$UID=='2' & realTest$TRANSECT=='C',][1,]$SAMPLE_TYPE <- 'PHAB'
  realTest[realTest$UID=='2' & realTest$TRANSECT=='D',][1,]$PARAMETER <- 'GUESS'

  #    Create duplicate rows, by one by duplicating a key, and by recording
  #    depths using more than one method i.e. PARAMETER.
  realTest[realTest$UID=='3' & realTest$TRANSECT=='A' & realTest$LINE==1
          ,]$TRANSECT <- 'K'
  realTest[realTest$UID=='3' & realTest$TRANSECT=='B' &
           realTest$LINE==1 & realTest$PARAMETER=='SONAR'
          ,]$RESULT <- 5
  realTest[realTest$UID=='4' & realTest$TRANSECT=='B' &
           realTest$LINE==1 & realTest$PARAMETER=='POLE'
          ,]$RESULT <- 5


  # Test perfect dataframe to look for false positives
  rr <- valStructChanDepth(baseTest, test='all')
  checkEquals(NULL, rr
             ,'Error: Detected structure problems in perfect Channel cover dataframe'
             )

  # Test real dataframe to look for false negatives
  rr <- valStructChanDepth(realTest, test='all')
  ee <- as.matrix(rbind(
            "Column UID has 1 missing values"
           ,"Column TRANSECT has 1 missing values"
           ,"Column LINE has 1 missing values"
           ,"Column PARAMETER has 1 missing values"
           ,"Column SAMPLE_TYPE has 1 missing values"
           ,"Unexpected value TRANSECT=(XA) at (UID)=(2)"
           ,"Unexpected value TRANSECT=() at (UID)=(1)"
           ,"Unexpected value LINE=(12) at (UID,TRANSECT,SAMPLE_TYPE,PARAMETER)=(2,B,PHAB_CHANBFRONT,POLE)"
           ,"Unexpected value LINE=(NA) at (UID,TRANSECT,SAMPLE_TYPE,PARAMETER)=(1,C,PHAB_CHANBFRONT,POLE)"
           ,"Unexpected row count at (UID,TRANSECT,SAMPLE_TYPE,LINE)=(3,B,PHAB_CHANBFRONT,1), n=2"
           ,"Unexpected row count at (UID,TRANSECT,SAMPLE_TYPE,LINE)=(4,B,PHAB_CHANBFRONT,1), n=2"
           ,"Unexpected row count at (UID,TRANSECT,SAMPLE_TYPE,LINE)=(3,K,PHAB_CHANBFRONT,1), n=2"
           ,"Unexpected value SAMPLE_TYPE=(PHAB) at (UID,TRANSECT,LINE,PARAMETER)=(2,C,1,POLE)"
           ,"Unexpected value SAMPLE_TYPE=() at (UID,TRANSECT,LINE,PARAMETER)=(1,E,1,POLE)"
           ,"Unexpected value PARAMETER=() at (UID,TRANSECT,SAMPLE_TYPE,LINE)=(1,D,PHAB_CHANBFRONT,1)"
           ,"Unexpected value PARAMETER=(GUESS) at (UID,TRANSECT,SAMPLE_TYPE,LINE)=(2,D,PHAB_CHANBFRONT,1)"
           ,"Absent TRANSECT=(B,C,D,E,F,G,H,I,J,K) value at UID=() "
           ,"Absent PARAMETER=(SONAR) value at UID,TRANSECT=(,A) "
           ,"Absent PARAMETER=(SONAR) value at UID,TRANSECT=(1,) "
           ,"Absent PARAMETER=(SONAR) value at UID,TRANSECT=(2,XA) "
           ))
  checkEquals(ee, rr
             ,'Error: Did not correctly find structure problems in imperfect Channel cover data'
             )

}

# end of file