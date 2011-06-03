# valStructFishCollection.r
#
# 10/19/2009 mrc Created
# 10/21/2009 added timeThis
# 12/8/09    mrc changed sample_type to reflect W, B
# 12/23/09 cws Did not update unit test to reflect current SAMPLE_TYPE values
# 02/25/10 cws creation of unit test data is now done separately for each
#          SAMPLE_TYPE.  Changed structure checks to take SAMPLE_TYPE into
#          account.
#
# first must open a channel to desired db

require(RODBC)

#intermediateMessages <- TRUE


#get and fix the fishcollection table




valStructFishCollection <- function(df, test='all')

# Performs structure checks on the NRSA table tblFishCollection2.  Returns NULL if
# no errors have been found, or an Nx1 character matrix describing the detected
# errors.
#
# ARGUMENTS:
# df        dataframe of NRSA FishCollection data
# test      String describing which tests to perform.  May be one of the
#             following:
#             'all'       default, performs all tests
#             'vital'     performs only vital tests
#             'nonvital'  performs only nonvital tests
#             'synopsis'  performs all tests, returning counts of detected
#                           errors for all tests
#
# ASSUMPTIONS: have a open channel to the database
#

{
  intermediateMessage('Structure validation of fish collection data ', loc='end')
  timeThis <- FALSE
  probs <- NULL

  # sanity checks
  if(!(test %in% c('all', 'vital', 'nonvital', 'synopsis'))) {
      probs<-"Error: test argument must be 'all', 'vital', 'nonvital' or 'synopsis'"
      return(as.matrix(probs))
  }



  # Check for correct column names no matter what, and stop early if there
  # are any errors, since nothing else will work without them
  intermediateMessage('.1correctColumnNames', loc='end')
  probs <- stValColumnPresence(df, c('UID', 'TRANSECT','PAGE','LINE'
                                    ,'PARAMETER', 'RESULT'
                                    ,'SAMPLE_TYPE', 'FLAG'
                                    ) , timing=timeThis
                              )
  if(!is.null(probs)) {
      colnames(probs)[1] <-'User.MUST.fix.these.errors.before.proceding'
      intermediateMessage('.  Done (terminated early).', loc='end')
      return(probs)
  }

#-----------------------------MISSING VALUES-----------------------------------#

  #  Perform 'vital' tests if requested
  if(test %in% c('all','vital','synopsis')) {

      # Check for missing UID values
      intermediateMessage('.2missingUIDValues', loc='end')
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
      intermediateMessage('.3MissingTransectValues', loc='end')
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

  # Check for missing PAGE values
      intermediateMessage('.4MissingPageValues', loc='end')
      pp <- stValMissingValues(df, 'PAGE', timing=timeThis)
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,ifelse(is.null(pp)
                               ,"No Missing PAGE values"
                               ,"Missing PAGE values exist (vital)"
                               )
                        )
      } else {
          probs <- rbind(probs, pp)
      }
       # Check for missing LINE values
      intermediateMessage('.5MissingLineValues', loc='end')
      pp <- stValMissingValues(df, 'LINE', timing=timeThis)
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
      intermediateMessage('.6missingParameterValues', loc='end')
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
      intermediateMessage('.7missingSampleType', loc='end')
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

#------------------------------------------------------------------------------#
      # Separate structure checks by SAMPLE_TYPE
      fishb <- subset(df, SAMPLE_TYPE=='FISHB')
      fishw <- subset(df, SAMPLE_TYPE=='FISHW')
      lmsamp <- subset(df, SAMPLE_TYPE=='LMSAMP')

#-------------------------UNEXPECTED VALUES------------------------------------#
  
      # Check for unexpected TRANSECT values
      intermediateMessage('.8unexpectedTransect', loc='end')
      if(nrow(fishb)>0) {
          pp <- stValUnexpectedValues (fishb,'TRANSECT'
                                      ,c(LETTERS[1:10], 'NOT MARKED')
                                      ,c('UID'), timing=timeThis
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
      }
      if(nrow(fishw)>0) {
          pp <- stValUnexpectedValues (fishw,'TRANSECT'
                                      ,c('NONE')
                                      ,c('UID'), timing=timeThis
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
      }
      if(nrow(lmsamp)>0) {
          pp <- stValUnexpectedValues (lmsamp,'TRANSECT'
                                      ,c(LETTERS[1:10])
                                      ,c('UID'), timing=timeThis
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
      }

      # Check for unexpected PARAMETER values
      intermediateMessage('.9unexpectedParameter', loc='end')
      if(nrow(fishb)>0) {
          pp <- stValAbsentValues(fishb, 'PARAMETER'
                                 ,c('ACTUAL_DATE', 'ANOM_CT', 'BANK'
                                   ,'BANK_OTHER', 'COUNT', 'COVER'
                                   ,'COVER_OTHER', 'DEPTH', 'DIST_SAMPLED'
                                   ,'FINAL_CT', 'MORT_CT', 'NAME_COM'
                                   ,'NO_FISH', 'PHOTO', 'SHOCK_TIME'
                                   ,'SUB_OTHER', 'SUBSTRATE', 'TAG'
                                   ,'TL_MAX', 'TL_MIN', 'VOUCH_CT'
                                   )
                                 ,c('UID', 'TRANSECT') , timing=timeThis
                                 )
          if(test=='synopsis') {
              probs <- rbind(probs
                            ,sprintf("Unexpected PARAMETER values (SAMPLE_TYPE=FISHB): %d"
                                    ,ifelse(is.null(pp), 0, nrow(pp))
                                    )
                            )
          } else {
              probs <- rbind(probs, pp)
          }
      }
      if(nrow(fishw)>0) {
          pp <- stValAbsentValues(fishw, 'PARAMETER'
                                 ,c('ACTUAL_DATE', 'ANOM_CT', 'COUNT'
                                   ,'FINAL_CT', 'MORT_CT', 'NAME_COM'
                                   ,'PHOTO', 'TAG', 'TL_MAX', 'TL_MIN'
                                   ,'TRANA', 'TRANB', 'TRANC', 'TRAND'
                                   ,'TRANE', 'TRANF', 'TRANG', 'TRANH'
                                   ,'TRANI', 'TRANJ', 'VOUCH_CT'
                                   )
                                 ,c('UID') , timing=timeThis
                                 )
          if(test=='synopsis') {
              probs <- rbind(probs
                            ,sprintf("Unexpected PARAMETER values (SAMPLE_TYPE=FISHW): %d"
                                    ,ifelse(is.null(pp), 0, nrow(pp))
                                    )
                            )
          } else {
              probs <- rbind(probs, pp)
          }
      }
      if(nrow(lmsamp)>0) {
          pp <- stValAbsentValues(lmsamp, 'PARAMETER'
                                 ,c('AC_COUNT', 'AC_SIDE')
                                 ,c('UID', 'TRANSECT') , timing=timeThis
                                 )
          if(test=='synopsis') {
              probs <- rbind(probs
                            ,sprintf("Unexpected PARAMETER values (SAMPLE_TYPE=LMSAMP): %d"
                                    ,ifelse(is.null(pp), 0, nrow(pp))
                                    )
                            )
          } else {
              probs <- rbind(probs, pp)
          }
      }

      # Check for unexpected values of SAMPLE_TYPE.
         
      intermediateMessage('.10unexpectedSampleType', loc='end')
      pp <- stValUnexpectedValues(df
                                 ,'SAMPLE_TYPE'
                                 ,c('FISHW','FISHB', 'LMSAMP')
                                 ,c('UID', 'TRANSECT'), timing=timeThis
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
#---------------------------UNIQUE VALUES--------------------------------------#
 #requires too much memory to run at this time   
      # Check for uniqueness of each UID-TRANSECT-PAGE-LINE-PARAMETER
 #     intermediateMessage('.11uniqueUID*Transect*Page*Line*Parameter', loc='end')
 #     pp <- stValCountRows(df, c('UID','TRANSECT', 'PAGE','LINE', 'PARAMETER'), 1)
 #     if(test=='synopsis') {
 #         probs <- rbind(probs
 #                       ,sprintf("Duplicate UID-TRANSECT-PAGE_LINE-PARAMETER values: %d (vital)"
 #                               ,ifelse(is.null(pp), 0, nrow(pp))
 #                               )
 #                       )
 #     } else {
 #         probs <- rbind(probs, pp)
 #     }

  }

#----------------------------ABSENT VALUES-------------------------------------#
  # Perform nonvital tests if requested
  if(test %in% c('all','nonvital','synopsis')) {
      # Check for absent TRANSECT values at each UID
      intermediateMessage('.12absentTransectValues', loc = 'end')
      if(nrow(fishb) > 0) {
          pp <- stValAbsentValues(fishb, 'TRANSECT'
                                 ,c(LETTERS[1:10], 'NOT MARKED')
                                 ,c('UID') , timing=timeThis
                                 )
          if(test=='synopsis') {
              probs <- rbind(probs
                            ,sprintf("Absent TRANSECT values (SAMPLE_TYPE=FISHB) : %d"
                                    ,ifelse(is.null(pp), 0, nrow(pp))
                                    )
                            )
          } else {
              probs <- rbind(probs, pp)
          }
      }
      if(nrow(fishw) > 0) {
          pp <- stValAbsentValues(fishw, 'TRANSECT'
                                 ,c('NONE')
                                 ,c('UID') , timing=timeThis
                                 )
          if(test=='synopsis') {
              probs <- rbind(probs
                            ,sprintf("Absent TRANSECT values (SAMPLE_TYPE=FISHW) : %d"
                                    ,ifelse(is.null(pp), 0, nrow(pp))
                                    )
                            )
          } else {
              probs <- rbind(probs, pp)
          }
      }
      if(nrow(lmsamp) > 0) {
          pp <- stValAbsentValues(lmsamp, 'TRANSECT'
                                 ,c(LETTERS[1:10])
                                 ,c('UID') , timing=timeThis
                                 )
          if(test=='synopsis') {
              probs <- rbind(probs
                            ,sprintf("Absent TRANSECT values (SAMPLE_TYPE=LMSAMP) : %d"
                                    ,ifelse(is.null(pp), 0, nrow(pp))
                                    )
                            )
          } else {
              probs <- rbind(probs, pp)
          }
      }

      # Check for absent PARAMETER values (parameters NO_FISH and PHOTO may
      # be absent at a transect.
      intermediateMessage('.13absentParameter', loc='end')
      if(nrow(fishb) > 0) {
          pp <- stValAbsentValues(fishb, 'PARAMETER'
                                 ,c('ACTUAL_DATE', 'ANOM_CT', 'BANK'
                                   ,'BANK_OTHER', 'COUNT', 'COVER'
                                   ,'COVER_OTHER', 'DEPTH', 'DIST_SAMPLED'
                                   ,'FINAL_CT', 'MORT_CT', 'NAME_COM'
                                   ,'SHOCK_TIME'
                                   ,'SUB_OTHER', 'SUBSTRATE', 'TAG'
                                   ,'TL_MAX', 'TL_MIN', 'VOUCH_CT'
                                   )
                                 ,c('UID', 'TRANSECT') , timing=timeThis
                                 )
          if(test=='synopsis') {
              probs <- rbind(probs
                            ,sprintf("Absent PARAMETER values (SAMPLE_TYPE=FISHB): %d"
                                    ,ifelse(is.null(pp), 0, nrow(pp))
                                    )
                            )
          } else {
              probs <- rbind(probs, pp)
          }
      }
      if(nrow(fishw) > 0) {
          pp <- stValAbsentValues(fishw, 'PARAMETER'
                                 ,c('ACTUAL_DATE', 'ANOM_CT', 'COUNT'
                                   ,'FINAL_CT', 'MORT_CT', 'NAME_COM'
                                   ,'PHOTO', 'TAG', 'TL_MAX', 'TL_MIN'
                                   ,'TRANA', 'TRANB', 'TRANC', 'TRAND'
                                   ,'TRANE', 'TRANF', 'TRANG', 'TRANH'
                                   ,'TRANI', 'TRANJ', 'VOUCH_CT'
                                   )
                                 ,c('UID', 'TRANSECT') , timing=timeThis
                                 )
          if(test=='synopsis') {
              probs <- rbind(probs
                            ,sprintf("Absent PARAMETER values (SAMPLE_TYPE=FISHW): %d"
                                    ,ifelse(is.null(pp), 0, nrow(pp))
                                    )
                            )
          } else {
              probs <- rbind(probs, pp)
          }
      }
      if(nrow(lmsamp) > 0) {
          pp <- stValAbsentValues(lmsamp, 'PARAMETER'
                                 ,c('AC_COUNT', 'AC_SIDE')
                                 ,c('UID', 'TRANSECT') , timing=timeThis
                                 )
          if(test=='synopsis') {
              probs <- rbind(probs
                            ,sprintf("Absent PARAMETER values (SAMPLE_TYPE=LMSAMP): %d"
                                    ,ifelse(is.null(pp), 0, nrow(pp))
                                    )
                            )
          } else {
              probs <- rbind(probs, pp)
          }
      }
  }


  intermediateMessage('.  Done', loc='end')
  return(probs)

}



valStructFishCollectionTest <- function()
# Tests valStructFishCollection()
{
  # Create test data frame by combining dataframes for FISHW, FISHB and LMSAMP
  fishb <- rbind(expand.grid(UID=as.character(1000 + 1:4)
                            ,TRANSECT=c(LETTERS[1:10], 'NOT MARKED')
                            ,SAMPLE_TYPE='FISHB'
                            ,PAGE=as.character(1:5)
                            ,LINE=as.character(1:2)
                            ,PARAMETER=c('ACTUAL_DATE', 'ANOM_CT', 'BANK'
                                        ,'BANK_OTHER', 'COUNT', 'COVER'
                                        ,'COVER_OTHER', 'DEPTH', 'DIST_SAMPLED'
                                        ,'FINAL_CT', 'MORT_CT', 'NAME_COM'
                                        ,'NO_FISH', 'PHOTO', 'SHOCK_TIME'
                                        ,'SUB_OTHER', 'SUBSTRATE', 'TAG'
                                        ,'TL_MAX', 'TL_MIN', 'VOUCH_CT'
                                        )
                            )
                 )
  fishb$UID <- as.character(fishb$UID)
  fishb$PAGE <- as.character(fishb$PAGE)
  fishb$LINE <- as.character(fishb$LINE)
  fishb$TRANSECT <- as.character(fishb$TRANSECT)
  fishb$SAMPLE_TYPE <- as.character(fishb$SAMPLE_TYPE)
  fishb$PARAMETER <- as.character(fishb$PARAMETER)
  fishb$RESULT <- floor(runif(nrow(fishb), 0, 18))
  fishb$FLAG <- ''
  fishb <- subset(fishb
                 ,!(
#                    TRANSECT %in% c(LETTERS(A:J)) &
#                    PARAMETER %in% c('AC_COUNT', 'AC_SIDE', 'BANK', 'BANK_OTHER'
#                                    ,'COVER', 'COVER_OTHER', 'DEPTH'
#                                    ,'DIST_SAMPLED', 'NO_FISH', 'SHOCK_TIME'
#                                    ,'SUB_OTHER', 'SUBSTRATE'
#                                    )
#                    |
                    TRANSECT=='NOT_MARKED' & PARAMETER=='NO_FISH'
#                    PARAMETER %in% c('AC_COUNT', 'AC_SIDE', 'NO_FISH', 'TRANA'
#                                    ,'TRANB', 'TRANC', 'TRAND', 'TRANE', 'TRANF'
#                                    ,'TRANG', 'TRANH', 'TRANI', 'TRANJ'
#                                    )
                    |
                    LINE != 1 & PARAMETER=='NO_FISH'
                   )
                 )

  fishw <- rbind(expand.grid(UID=as.character(1100 + 1:4)
                            ,TRANSECT='NONE'
                            ,SAMPLE_TYPE='FISHW'
                            ,PAGE=as.character(1:5)
                            ,LINE=as.character(1:2)
                            ,PARAMETER=c('ACTUAL_DATE', 'ANOM_CT', 'COUNT'
                                        ,'FINAL_CT', 'MORT_CT', 'NAME_COM'
                                        ,'PHOTO', 'TAG', 'TL_MAX', 'TL_MIN'
                                        ,'TRANA', 'TRANB', 'TRANC', 'TRAND'
                                        ,'TRANE', 'TRANF', 'TRANG', 'TRANH'
                                        ,'TRANI', 'TRANJ', 'VOUCH_CT'
                                        )
                            )
                 )
  fishw$UID <- as.character(fishw$UID)
  fishw$PAGE <- as.character(fishw$PAGE)
  fishw$LINE <- as.character(fishw$LINE)
  fishw$TRANSECT <- as.character(fishw$TRANSECT)
  fishw$SAMPLE_TYPE <- as.character(fishw$SAMPLE_TYPE)
  fishw$PARAMETER <- as.character(fishw$PARAMETER)
  fishw$RESULT <- floor(runif(nrow(fishw), 0, 18))
  fishw$FLAG <- ''

  lmsamp <- expand.grid(UID=as.character(1000 + 1:2)
                       ,TRANSECT=c(LETTERS[1:10])
                       ,SAMPLE_TYPE='LMSAMP'
                       ,PAGE='999'
                       ,LINE='99'
                       ,PARAMETER=c('AC_COUNT', 'AC_SIDE')
                       )
  lmsamp$UID <- as.character(lmsamp$UID)
  lmsamp$PAGE <- as.character(lmsamp$PAGE)
  lmsamp$LINE <- as.character(lmsamp$LINE)
  lmsamp$TRANSECT <- as.character(lmsamp$TRANSECT)
  lmsamp$SAMPLE_TYPE <- as.character(lmsamp$SAMPLE_TYPE)
  lmsamp$PARAMETER <- as.character(lmsamp$PARAMETER)
  lmsamp$RESULT <- floor(runif(nrow(lmsamp), 0, 18))
  lmsamp$FLAG <- ''

  baseTest <- rbind(fishb, fishw, lmsamp)
  
  # Create imperfect/real dataframe to check for false negatives
  realTest <- baseTest

  # Make some transects absent by removing them
  realTest<-subset(realTest, !(UID=='1002' & TRANSECT=='A' &
                   SAMPLE_TYPE=='FISHB' & PAGE=='1' & LINE=='1' &
                   PARAMETER=='ACTUAL_DATE')
                  )
  realTest<-subset(realTest, !(UID=='1102' & realTest$TRANSECT=='NONE' &
                   SAMPLE_TYPE=='FISHW' & PAGE=='1' & LINE=='1' &
                   PARAMETER=='ACTUAL_DATE')
                  )
  realTest<-subset(realTest, !(UID=='1002' & realTest$TRANSECT=='A' &
                   SAMPLE_TYPE=='LMSAMP' & PAGE=='999' & LINE=='99' &
                   PARAMETER=='AC_COUNT')
                  )

  # remove transect by substituting it with legal values.  Note FISHW data
  # has only NONE valued transects, so there are no substitutions possible
  realTest[realTest$UID=='1001' & realTest$TRANSECT=='B' &
           realTest$SAMPLE_TYPE=='FISHB' & realTest$PAGE=='1' &
           realTest$LINE=='1' & realTest$PARAMETER=='ACTUAL_DATE',]$TRANSECT <- 'C'
  realTest[realTest$UID=='1001' & realTest$TRANSECT=='B' &
           realTest$SAMPLE_TYPE=='LMSAMP' & realTest$PAGE=='999' &
           realTest$LINE=='99' & realTest$PARAMETER=='AC_COUNT',]$TRANSECT <- 'C'

  # make key values missing
  realTest[realTest$UID=='1001' & realTest$TRANSECT=='A' &
           realTest$SAMPLE_TYPE=='FISHB' & realTest$PAGE=='1' &
           realTest$LINE=='1' & realTest$PARAMETER=='ACTUAL_DATE',]$UID <- NA
  realTest[realTest$UID=='1101' & realTest$TRANSECT=='NONE' &
           realTest$SAMPLE_TYPE=='FISHW' & realTest$PAGE=='1' &
           realTest$LINE=='1' & realTest$PARAMETER=='ACTUAL_DATE',]$UID <- NA
  realTest[realTest$UID=='1001' & realTest$TRANSECT=='A' &
           realTest$SAMPLE_TYPE=='LMSAMP' & realTest$PAGE=='999' &
           realTest$LINE=='99' & realTest$PARAMETER=='AC_COUNT',]$UID <- NA

  realTest[realTest$UID=='1001' & realTest$TRANSECT=='D' &
           realTest$SAMPLE_TYPE=='FISHB' & realTest$PAGE=='1' &
           realTest$LINE=='1' & realTest$PARAMETER=='ACTUAL_DATE',]$PARAMETER <- NA
  realTest[realTest$UID=='1101' & realTest$TRANSECT=='NONE' &
           realTest$SAMPLE_TYPE=='FISHW' & realTest$PAGE=='1' &
           realTest$LINE=='2' & realTest$PARAMETER=='ACTUAL_DATE',]$PARAMETER <- NA
  realTest[realTest$UID=='1001' & realTest$TRANSECT=='D' &
           realTest$SAMPLE_TYPE=='LMSAMP' & realTest$PAGE=='999' &
           realTest$LINE=='99' & realTest$PARAMETER=='AC_COUNT',]$PARAMETER <- NA

  realTest[realTest$UID=='1001' & realTest$TRANSECT=='E' &
           realTest$SAMPLE_TYPE=='FISHB' & realTest$PAGE=='1' &
           realTest$LINE=='1' & realTest$PARAMETER=='ACTUAL_DATE',]$LINE <- NA
  realTest[realTest$UID=='1101' & realTest$TRANSECT=='NONE' &
           realTest$SAMPLE_TYPE=='FISHW' & realTest$PAGE=='1' &
           realTest$LINE=='1' & realTest$PARAMETER=='ANOM_CT',]$LINE <- NA
  realTest[realTest$UID=='1001' & realTest$TRANSECT=='E' &
           realTest$SAMPLE_TYPE=='LMSAMP' & realTest$PAGE=='999' &
           realTest$LINE=='99' & realTest$PARAMETER=='AC_COUNT',]$LINE <- NA

  realTest[realTest$UID=='1001' & realTest$TRANSECT=='F' &
           realTest$SAMPLE_TYPE=='FISHB' & realTest$PAGE=='1' &
           realTest$LINE=='1' & realTest$PARAMETER=='ACTUAL_DATE',]$TRANSECT <- NA
  realTest[realTest$UID=='1101' & realTest$TRANSECT=='NONE' &
           realTest$SAMPLE_TYPE=='FISHW' & realTest$PAGE=='1' &
           realTest$LINE=='1' & realTest$PARAMETER=='FINAL_CT',]$TRANSECT <- NA
  realTest[realTest$UID=='1001' & realTest$TRANSECT=='F' &
           realTest$SAMPLE_TYPE=='LMSAMP' & realTest$PAGE=='999' &
           realTest$LINE=='99' & realTest$PARAMETER=='AC_COUNT',]$TRANSECT <- NA

  realTest[realTest$UID == '1001' & realTest$TRANSECT %in% 'G' &
           realTest$SAMPLE_TYPE == 'FISHB' & realTest$PAGE == '1' &
           realTest$LINE == '1' & realTest$PARAMETER == 'ACTUAL_DATE',]$PAGE <- NA
  realTest[realTest$UID=='1101' & realTest$TRANSECT=='NONE' &
           realTest$SAMPLE_TYPE=='FISHW' & realTest$PAGE=='1' &
           realTest$LINE=='1' & realTest$PARAMETER=='TL_MAX',]$PAGE <- NA
  realTest[realTest$UID=='1001' & realTest$TRANSECT %in% 'G' &
           realTest$SAMPLE_TYPE=='LMSAMP' & realTest$PAGE=='999' &
           realTest$LINE=='99' & realTest$PARAMETER=='AC_COUNT',]$PAGE <- NA

  #    unexpected values of keys
  realTest[realTest$UID=='1001' & realTest$TRANSECT %in% 'H' &
           realTest$SAMPLE_TYPE=='FISHB' & realTest$PAGE=='1' &
           realTest$LINE=='1' & realTest$PARAMETER=='ACTUAL_DATE',]$TRANSECT <- 'L'
  realTest[realTest$UID=='1101' & realTest$TRANSECT=='NONE' &
           realTest$SAMPLE_TYPE=='FISHW' & realTest$PAGE=='1' &
           realTest$LINE=='1' & realTest$PARAMETER=='MORT_CT',]$TRANSECT <- 'L'
  realTest[realTest$UID=='1001' & realTest$TRANSECT %in% 'H' &
           realTest$SAMPLE_TYPE=='LMSAMP' & realTest$PAGE=='999' &
           realTest$LINE=='99' & realTest$PARAMETER=='AC_COUNT',]$TRANSECT <- 'L'

  realTest[realTest$UID=='1001' & realTest$TRANSECT %in% 'I' &
           realTest$SAMPLE_TYPE=='FISHB' & realTest$PAGE=='1' &
           realTest$LINE=='1' & realTest$PARAMETER=='ACTUAL_DATE',]$SAMPLE_TYPE <- 'WRONG'
  realTest[realTest$UID=='1101' & realTest$TRANSECT=='NONE' &
           realTest$SAMPLE_TYPE=='FISHW' & realTest$PAGE=='1' &
           realTest$LINE=='1' & realTest$PARAMETER=='TAG',]$SAMPLE_TYPE <- 'WRONG'
  realTest[realTest$UID=='1001' & realTest$TRANSECT %in% 'I' &
           realTest$SAMPLE_TYPE=='LMSAMP' & realTest$PAGE=='999' &
           realTest$LINE=='99' & realTest$PARAMETER=='AC_COUNT',]$SAMPLE_TYPE <- 'WRONG'

  realTest[realTest$UID=='1001' & realTest$TRANSECT %in% 'J' &
           realTest$SAMPLE_TYPE=='FISHB' & realTest$PAGE=='1' &
           realTest$LINE=='1' & realTest$PARAMETER=='ACTUAL_DATE',]$PARAMETER <- 'WRONG'
  realTest[realTest$UID=='1101' & realTest$TRANSECT=='NONE' &
           realTest$SAMPLE_TYPE=='FISHW' & realTest$PAGE=='1' &
           realTest$LINE=='1' & realTest$PARAMETER=='PHOTO',]$PARAMETER <- 'WRONG'
  realTest[realTest$UID=='1001' & realTest$TRANSECT %in% 'J' &
           realTest$SAMPLE_TYPE=='LMSAMP' & realTest$PAGE=='999' &
           realTest$LINE=='99' & realTest$PARAMETER=='AC_COUNT',]$PARAMETER <- 'WRONG'


  # Test perfect dataframe to look for false positives
  rr <- valStructFishCollection(baseTest, test='all')
  checkEquals(NULL, rr
             ,'Error: Detected structure problems in perfect FishCollection dataframe'
             )

  # Test real dataframe to look for false negatives
  rr <- valStructFishCollection(realTest, test='all')
  ee <- as.matrix(rbind(
       "Column UID has 3 missing values"
      ,"Column TRANSECT has 3 missing values"
      ,"Column PAGE has 3 missing values"
      ,"Column LINE has 3 missing values"
      ,"Column PARAMETER has 3 missing values"
      ,"Unexpected value TRANSECT=(NA) at (UID)=(1001)"
      ,"Unexpected value TRANSECT=(L) at (UID)=(1001)"
      ,"Unexpected value TRANSECT=(NA) at (UID)=(1101)"
      ,"Unexpected value TRANSECT=(L) at (UID)=(1101)"
      ,"Unexpected value TRANSECT=(NA) at (UID)=(1001)"
      ,"Unexpected value TRANSECT=(L) at (UID)=(1001)"
      ,"Absent PARAMETER=(ANOM_CT,BANK,BANK_OTHER,COUNT,COVER,COVER_OTHER,DEPTH,DIST_SAMPLED,FINAL_CT,MORT_CT,NAME_COM,NO_FISH,PHOTO,SHOCK_TIME,SUB_OTHER,SUBSTRATE,TAG,TL_MAX,TL_MIN,VOUCH_CT) value at UID,TRANSECT=(1001,L) "
      ,"Absent PARAMETER=(ANOM_CT,BANK,BANK_OTHER,COUNT,COVER,COVER_OTHER,DEPTH,DIST_SAMPLED,FINAL_CT,MORT_CT,NAME_COM,NO_FISH,PHOTO,SHOCK_TIME,SUB_OTHER,SUBSTRATE,TAG,TL_MAX,TL_MIN,VOUCH_CT) value at UID,TRANSECT=(1001,NA) "
      ,"Absent PARAMETER=(ANOM_CT,BANK,BANK_OTHER,COUNT,COVER,COVER_OTHER,DEPTH,DIST_SAMPLED,FINAL_CT,MORT_CT,NAME_COM,NO_FISH,PHOTO,SHOCK_TIME,SUB_OTHER,SUBSTRATE,TAG,TL_MAX,TL_MIN,VOUCH_CT) value at UID,TRANSECT=(NA,A) "
      ,"Absent PARAMETER=(ANOM_CT,COUNT,FINAL_CT,MORT_CT,NAME_COM,PHOTO,TAG,TL_MAX,TL_MIN,TRANA,TRANB,TRANC,TRAND,TRANE,TRANF,TRANG,TRANH,TRANI,TRANJ,VOUCH_CT) value at UID=(NA) "
      ,"Absent PARAMETER=(AC_COUNT) value at UID,TRANSECT=(1001,A) "
      ,"Absent PARAMETER=(AC_COUNT) value at UID,TRANSECT=(1001,B) "
      ,"Absent PARAMETER=(AC_COUNT) value at UID,TRANSECT=(1001,D) "
      ,"Absent PARAMETER=(AC_COUNT) value at UID,TRANSECT=(1001,F) "
      ,"Absent PARAMETER=(AC_COUNT) value at UID,TRANSECT=(1001,H) "
      ,"Absent PARAMETER=(AC_COUNT) value at UID,TRANSECT=(1001,I) "
      ,"Absent PARAMETER=(AC_COUNT) value at UID,TRANSECT=(1001,J) "
      ,"Absent PARAMETER=(AC_SIDE) value at UID,TRANSECT=(1001,L) "
      ,"Absent PARAMETER=(AC_SIDE) value at UID,TRANSECT=(1001,NA) "
      ,"Absent PARAMETER=(AC_COUNT) value at UID,TRANSECT=(1002,A) "
      ,"Absent PARAMETER=(AC_SIDE) value at UID,TRANSECT=(NA,A) "
      ,"Unexpected value SAMPLE_TYPE=(WRONG) at (UID,TRANSECT)=(1001,I)"
      ,"Unexpected value SAMPLE_TYPE=(WRONG) at (UID,TRANSECT)=(1101,NONE)"
      ,"Unexpected value SAMPLE_TYPE=(WRONG) at (UID,TRANSECT)=(1001,I)"
      ,"Absent TRANSECT=(B,C,D,E,F,G,H,I,J,NOT MARKED) value at UID=(NA) "
      ,"Absent TRANSECT=(B,C,D,E,F,G,H,I,J) value at UID=(NA) "
      ,"Absent PARAMETER=(ANOM_CT,BANK,BANK_OTHER,COUNT,COVER,COVER_OTHER,DEPTH,DIST_SAMPLED,FINAL_CT,MORT_CT,NAME_COM,SHOCK_TIME,SUB_OTHER,SUBSTRATE,TAG,TL_MAX,TL_MIN,VOUCH_CT) value at UID,TRANSECT=(1001,L) "
      ,"Absent PARAMETER=(ANOM_CT,BANK,BANK_OTHER,COUNT,COVER,COVER_OTHER,DEPTH,DIST_SAMPLED,FINAL_CT,MORT_CT,NAME_COM,SHOCK_TIME,SUB_OTHER,SUBSTRATE,TAG,TL_MAX,TL_MIN,VOUCH_CT) value at UID,TRANSECT=(1001,NA) "
      ,"Absent PARAMETER=(ANOM_CT,BANK,BANK_OTHER,COUNT,COVER,COVER_OTHER,DEPTH,DIST_SAMPLED,FINAL_CT,MORT_CT,NAME_COM,SHOCK_TIME,SUB_OTHER,SUBSTRATE,TAG,TL_MAX,TL_MIN,VOUCH_CT) value at UID,TRANSECT=(NA,A) "
      ,"Absent PARAMETER=(ACTUAL_DATE,ANOM_CT,COUNT,FINAL_CT,NAME_COM,PHOTO,TAG,TL_MAX,TL_MIN,TRANA,TRANB,TRANC,TRAND,TRANE,TRANF,TRANG,TRANH,TRANI,TRANJ,VOUCH_CT) value at UID,TRANSECT=(1101,L) "
      ,"Absent PARAMETER=(ACTUAL_DATE,ANOM_CT,COUNT,MORT_CT,NAME_COM,PHOTO,TAG,TL_MAX,TL_MIN,TRANA,TRANB,TRANC,TRAND,TRANE,TRANF,TRANG,TRANH,TRANI,TRANJ,VOUCH_CT) value at UID,TRANSECT=(1101,NA) "
      ,"Absent PARAMETER=(AC_COUNT) value at UID,TRANSECT=(1001,A) "
      ,"Absent PARAMETER=(AC_COUNT) value at UID,TRANSECT=(1001,B) "
      ,"Absent PARAMETER=(AC_COUNT) value at UID,TRANSECT=(1001,D) "
      ,"Absent PARAMETER=(AC_COUNT) value at UID,TRANSECT=(1001,F) "
      ,"Absent PARAMETER=(AC_COUNT) value at UID,TRANSECT=(1001,H) "
      ,"Absent PARAMETER=(AC_COUNT) value at UID,TRANSECT=(1001,I) "
      ,"Absent PARAMETER=(AC_COUNT) value at UID,TRANSECT=(1001,J) "
      ,"Absent PARAMETER=(AC_SIDE) value at UID,TRANSECT=(1001,L) "
      ,"Absent PARAMETER=(AC_SIDE) value at UID,TRANSECT=(1001,NA) "
      ,"Absent PARAMETER=(AC_COUNT) value at UID,TRANSECT=(1002,A) "
      ,"Absent PARAMETER=(AC_SIDE) value at UID,TRANSECT=(NA,A) "
      )
    ,ncol=1) # end of as.matrix
  checkEquals(ee, rr
             ,'Error: Did not correctly find structure problems in imperfect FishCollection data'
             )

}

# end of file