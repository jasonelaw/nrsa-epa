# valStructThalweg.r
#
# 09/24/09 cws Created valStructChancov
# 10/02/09  SSR adapted for tblTHALWEG2
# 10/22/09 cws Added unit test.  Modified structure check to include uniqueness
#          check, only allow main channel transects, separate absent/unexpected
#          parameter checks by SAMPLE_TYPE.  Commented out absent parameter
#          check for thalweg data since there is no way to discern wadeable
#          and boatable reaches using SAMPLE_TYPE.  NOTE: Will need to add lines
#          of expected problems to unit test if/when this changes.
# 11/6/09 mrc - fixed parameter SIZE_CLS to reflect change in parameter
# 12/02/09 cws Removed code checking for SAMPLE_TYPE 'PHAB_CHANF' and parameters
#          'ACTRANSP' and 'INTDTRAN', which are in tblCHANNELGEOMETRY2.  Updated
#          unit test accordingly.
#  2/25/10 cws moved source() calls to NRSAvalidation.r
#  4/19/10 ssr added 'NOT MARKED' as possible transect
# 05/28/10 ssr added check for non-numeric values

# Contains functions valStructBankGeometry, fixBankGeometry 

require(RODBC)
#intermediateMessages <- TRUE

valStructThalweg <- function(df, test='all')
# Performs structure checks on the NRSA table tblTHALWEG2.  Returns NULL if
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
  intermediateMessage('Structure validation of thalweg data ', loc='start')
  
  probs <- NULL

  # sanity checks
  if(!(test %in% c('all', 'vital', 'nonvital', 'synopsis'))) {
      probs<-"Error: test argument must be 'all', 'vital', 'nonvital' or 'synopsis'"
      return(as.matrix(probs))
  }
  
  # Check for correct column names no matter what, and stop early if there
  # are any errors, since nothing else will work without them
  intermediateMessage('.1')
  probs <- stValColumnPresence(df, c('UID', 'TRANSECT', 'STATION'
                                    ,'PARAMETER', 'RESULT', 'FLAG'
                                    ,'SAMPLE_TYPE', 'UNITS'
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


      # Check for missing STATION values
      intermediateMessage('.4')
      pp <- stValMissingValues(df, 'STATION')
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,ifelse(is.null(pp)
                               ,"No Missing STATION values"
                               ,"Missing STATION values exist (vital)"
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


      # Check for unexpected STATION values in Streams
      intermediateMessage('.7')
      pp <- stValUnexpectedValues(subset(df, SAMPLE_TYPE=='PHAB_THAL')
                                 , 'STATION'
                                 ,c('0', '1', '2', '3', '4', '5', '6', '7', '8',
                                 '9', '10', '11', '12', '13', '14')
                                 ,c('UID', 'TRANSECT')
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Unexpected STATION values: %d (vital)"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }


      # Check for unexpected STATION values in Rivers
      intermediateMessage('.8')
      pp <- stValUnexpectedValues(subset(df, SAMPLE_TYPE=='PHAB_CHANF')
                                 ,'STATION'
                                 ,c('NONE')
                                 ,c('UID', 'TRANSECT')
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Unexpected STATION values: %d (vital)"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }


      # Check for unexpected TRANSECT values in Streams
      intermediateMessage('.9')
      pp <- stValUnexpectedValues(subset(df, SAMPLE_TYPE=='PHAB_THAL')
                                 ,'TRANSECT'
                                 ,c('A','B','C','D','E','F','G','H','I','J','NOT MARKED')
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


      # Check for unexpected TRANSECT values in Rivers
      intermediateMessage('.10')
      pp <- stValUnexpectedValues(subset(df, SAMPLE_TYPE=='PHAB_CHANF')
                                 ,'TRANSECT'
                                 ,c('A','B','C','D','E','F','G','H','I','J')
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


      # Check for uniqueness of each UID-TRANSECT-STATION-PARAMETER
      intermediateMessage('.11')
      pp <- stValCountRows(df, c('UID','TRANSECT','STATION','PARAMETER'), 1)
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Duplicate UID-TRANSECT-STATION values: %d (vital)"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }

      #  Check that numeric fields contain numeric data
      intermediateMessage('.11a')
      pp <- stValNonNumericValues(subset(df, PARAMETER %in% c('DEPTH','INCREMNT'
                                         , 'WETWIDTH', 'BARWIDTH','DEP_SONR'
                                         , 'DEP_POLE'))
                                  ,'RESULT'
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Non-numeric values: %d (vital)"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }
      



  }
  
  # Perform nonvital tests if requested
  if(test %in% c('all','nonvital','synopsis')) {

      # Check for absent STATION values at each UID-TRANSECT in Streams
      intermediateMessage('.12')
      pp <- stValAbsentValues(subset(df, SAMPLE_TYPE=='PHAB_THAL')
                             ,'STATION'
                             ,c(as.character(0:9))
                             ,c('UID', 'TRANSECT')
                             )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Absent STATION values with SAMPLE_TYPE=='PHAB_THAL': %d"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }


      # Check for absent STATION values at each UID-TRANSECT in boatable
      # channel/riparian data.
      intermediateMessage('.13')
      pp <- stValAbsentValues(subset(df, SAMPLE_TYPE=='PHAB_CHANF')
                             ,'STATION'
                             ,c('NONE')
                             ,c('UID', 'TRANSECT')
                             )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Absent STATION with values SAMPLE_TYPE='PHAB_CHANF': %d"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }



      # Check for absent TRANSECT values at each UID
      intermediateMessage('.14')
      pp <- stValAbsentValues(df, 'TRANSECT'
                             ,c('A','B','C','D','E','F','G','H','I','J')
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

      # Check for unexpected PARAMETER values
      intermediateMessage('.15')
      pp <- stValUnexpectedValues(subset(df, SAMPLE_TYPE=='PHAB_THAL')
                                 ,'PARAMETER'
                                 ,c('BACKWATER', 'BAR_PRES', 'CHANUNCD', 'DEPTH'
                                   ,'INCREMNT', 'POOLFMCD', 'SEDIMENT', 'SIDCHN'
                                   ,'WETWIDTH', 'BARWIDTH', 'DEP_SONR', 'OFF_CHAN'
                                   ,'SIZE_CLS', 'SNAG', 'DEP_POLE'
                                   )
                                 ,c('UID', 'TRANSECT', 'STATION')
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

#      intermediateMessage('.16')
#      pp <- stValUnexpectedValues(subset(df, SAMPLE_TYPE=='PHAB_CHANF')
#                                 ,'PARAMETER'
#                                 ,c('ACTRANSP', 'INTDTRAN')
#                                 ,c('UID', 'TRANSECT')
#                                 )
#      if(test=='synopsis') {
#          probs <- rbind(probs
#                        ,sprintf("Unexpected PARAMETER values: %d"
#                                ,ifelse(is.null(pp), 0, nrow(pp))
#                                )
#                        )
#      } else {
#          probs <- rbind(probs, pp)
#      }

      # Check for absent PARAMETER values
#      intermediateMessage('.17')
#      pp <- stValAbsentValues(subset(df, SAMPLE_TYPE == 'PHAB_THAL')
#                                 ,'PARAMETER'
#                                 ,c('BACKWATER', 'BAR_PRES', 'CHANUNCD', 'DEPTH',
#                                    'INCREMNT', 'POOLFMCD', 'SEDIMENT', 'SIDCHN',
#                                    'WETWIDTH', 'BARWIDTH', 'DEP_SONR', 'OFF_CHAN',
#                                    'SIZ_CLS', 'SNAG', 'DEP_POLE', 'ACTRANSP', 'INTDTRAN')
#                                 ,c('UID', 'TRANSECT', 'STATION')
#                                 )
#      if(test=='synopsis') {
#          probs <- rbind(probs
#                        ,sprintf("Absent PARAMETER values with SAMPLE_TYPE='PHAB_THAL': %d"
#                                ,ifelse(is.null(pp), 0, nrow(pp))
#                                )
#                        )
#     } else {
#          probs <- rbind(probs, pp)
#      }

      # Check for absent PARAMETER values
#      intermediateMessage('.18')
#      pp <- stValAbsentValues(subset(df, SAMPLE_TYPE == 'PHAB_CHANF')
#                                 ,'PARAMETER'
#                                 ,c('ACTRANSP', 'INTDTRAN')
#                                 ,c('UID', 'TRANSECT')
#                                 )
#      if(test=='synopsis') {
#          probs <- rbind(probs
#                        ,sprintf("Absent PARAMETER values with SAMPLE_TYPE='PHAB_CHANF': %d"
#                                ,ifelse(is.null(pp), 0, nrow(pp))
#                                )
#                        )
#     } else {
#          probs <- rbind(probs, pp)
#      }


      # Check for unexpected values of SAMPLE_TYPE.
      intermediateMessage('.19')
      pp <- stValUnexpectedValues(df
                                 ,'SAMPLE_TYPE'
#                                 ,c('PHAB_THAL','PHAB_CHANF', 'PHAB_CHANBFRONT')
                                 ,c('PHAB_THAL','PHAB_THALW')
                                 ,c('UID', 'TRANSECT', 'STATION')
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

valStructThalwegTest <- function()
# Tests valStructThalweg()
{
  # Create well formed data.  This table contains data from both wadeable
  # and boatable thalweg forms, as well as two items from the slope & bearing
  # portion of the channel/Riparian transect form.  As such, the construction
  # is somewhat complicated.  UIDs that are even will have 10 stations per
  # transect, UIDs that are odd will have 15 or 12, depending on whether they
  # are wadeable or boatable, respectively.  Even boatable UIDs will have depth
  # units in meters, odd UIDs in ft.
  baseWT <- expand.grid(UID = 1:10                # Wadeable thalweg
                       ,TRANSECT = LETTERS[1:10]
                       ,STATION = 0:14
                       ,SAMPLE_TYPE = 'PHAB_THAL'
                       ,PARAMETER = c('BACKWATER', 'BAR_PRES', 'CHANUNCD'
                                     ,'DEPTH', 'INCREMNT', 'POOLFMCD'
                                     ,'SEDIMENT', 'SIDCHN', 'WETWIDTH'
                                     , 'BARWIDTH'
                                     )
                       )
  baseWT[baseWT$PARAMETER == 'BACKWATER', 'RESULT'] <- rep(c('Y','N')
                                                          ,length.out=10*10*15
                                                          )
  baseWT[baseWT$PARAMETER == 'BAR_PRES', 'RESULT'] <- rep(c('Y','N')
                                                         ,length.out=10*10*15
                                                         )
  baseWT[baseWT$PARAMETER == 'CHANUNCD' &
         baseWT$UID %in% as.character(1:10)
        ,'RESULT'] <- rep(c('PP','PT','PL','PB','PD','GL','RI','RA','CA','FA','DR')
                         ,length.out=10*10*15
                         )
  baseWT[baseWT$PARAMETER == 'DEPTH', 'RESULT'] <- rep(12,length.out=10*10*15)
  baseWT[baseWT$PARAMETER == 'INCREMNT', 'RESULT'] <- rep(2,length.out=10*10*15)
  baseWT[baseWT$PARAMETER == 'POOLFMCD', 'RESULT'] <- rep(c('N','W','W'
                                                           ,'R','B','F'
                                                           )
                                                         ,length.out=10*10*15
                                                         )
  baseWT[baseWT$PARAMETER == 'SEDIMENT', 'RESULT'] <- rep(c('Y','N')
                                                         ,length.out=10*10*15
                                                         )
  baseWT[baseWT$PARAMETER == 'SIDCHN', 'RESULT'] <- rep(c('Y','N')
                                                         ,length.out=10*10*15
                                                         )
  baseWT[baseWT$PARAMETER == 'WETWIDTH', 'RESULT'] <- rep(5.5
                                                         ,length.out=10*10*15
                                                         )
  baseWT[baseWT$PARAMETER == 'BARWIDTH', 'RESULT'] <- rep(5.5
                                                         ,length.out=10*10*15
                                                         )
  baseWT <- subset(baseWT
                  ,!(as.integer(UID) %% 2 == 0 & as.integer(STATION) > 9)
                  )

  baseBT <- expand.grid(UID=11:20                 # Boatable thalweg
                       ,TRANSECT = LETTERS[1:10]
                       ,STATION = 0:11
                       ,SAMPLE_TYPE = 'PHAB_THAL'
                       ,PARAMETER = c('CHANUNCD', 'DEP_POLE', 'DEP_SONR'
                                     ,'OFF_CHAN', 'SIZE_CLS', 'SNAG'
                                     )
                       )
  baseBT[baseBT$PARAMETER == 'CHANUNCD', 'RESULT'] <- rep(c('PO','GL','RI','RA','CA','FA','DR')
                                                         ,length.out=10*10*12
                                                         )
  baseBT[baseBT$PARAMETER == 'DEP_POLE', 'RESULT'] <- rep(c(17,NA)
                                                         ,length.out=10*10*12/2
                                                         )
  baseBT[baseBT$PARAMETER == 'DEP_SONR', 'RESULT'] <- rep(c(NA,18)
                                                         ,length.out=10*10*12/2
                                                         )
  baseBT[baseBT$PARAMETER == 'OFF_CHAN', 'RESULT'] <- rep(c('Y','N')
                                                         ,length.out=10*10*12
                                                         )
  baseBT[baseBT$PARAMETER == 'SIZE_CLS', 'RESULT'] <- rep(c('BH','BL','CP','GR'
                                                          ,'SA','FN','OT'
                                                          )
                                                          ,length.out=10*10*12
                                                          )
  baseBT[baseWT$PARAMETER == 'SNAG', 'RESULT'] <- rep(c('Y','N')
                                                     ,length.out=10*10*12
                                                     )
  baseBT <- subset(baseBT
                  ,!(as.integer(UID) %% 2 == 0 & as.integer(STATION) > 9)
                  )

#  baseBC <- expand.grid(UID=11:20                 # Boatable chan form
#                       ,TRANSECT = LETTERS[1:10]
#                       ,STATION = 'NONE'
#                       ,SAMPLE_TYPE = 'PHAB_CHANF'
#                       ,PARAMETER = c('ACTRANSP', 'INTDTRAN')
#                       )
#  baseBC[baseBC$PARAMETER == 'ACTRANSP', 'RESULT'] <- rep(c(NA,190)
#                                                         ,length.out=10*10*1
#                                                         )
#  baseBC[baseBC$PARAMETER == 'INTDTRAN', 'RESULT'] <- rep(c(NA,200)
#                                                         ,length.out=10*10*1
#                                                         )

  baseTest <- rbind(baseWT, baseBT)#, baseBC)
  baseTest$UID <- as.character(baseTest$UID)
  baseTest$TRANSECT <- as.character(baseTest$TRANSECT)
  baseTest$STATION <- as.character(baseTest$STATION)
  baseTest$SAMPLE_TYPE <- as.character(baseTest$SAMPLE_TYPE)
  baseTest$PARAMETER <- as.character(baseTest$PARAMETER)
  baseTest$FLAG <- as.character(NA)
  baseTest$UNITS <- as.character(NA)
  baseTest[baseTest$PARAMETER %in% c('DEP_POLE','DEP_SONR') &
           as.integer(baseTest$UID) %% 2 == 0,]$UNITS <- 'm'
  baseTest[baseTest$PARAMETER %in% c('DEP_POLE','DEP_SONR') &
           as.integer(baseTest$UID) %% 2 != 0,]$UNITS <- 'ft'



  # Create poorly formed data based on correct data
  realTest <- baseTest
  
  #    Make some values missing
  realTest[realTest$UID=='1' & realTest$TRANSECT=='A' &
           realTest$STATION=='2' & realTest$PARAMETER=='BACKWATER'
          ,]$UID <- ''
  realTest[realTest$UID=='1' & realTest$TRANSECT=='B' &
           realTest$STATION=='2' & realTest$PARAMETER=='BACKWATER'
          ,]$TRANSECT <- ''
  realTest[realTest$UID=='1' & realTest$TRANSECT=='C' &
           realTest$STATION=='2' & realTest$PARAMETER=='BACKWATER'
          ,]$STATION <- ''
  realTest[realTest$UID=='1' & realTest$TRANSECT=='D' &
           realTest$STATION=='2' & realTest$PARAMETER=='BACKWATER'
          ,]$SAMPLE_TYPE <- ''
  realTest[realTest$UID=='1' & realTest$TRANSECT=='E' &
           realTest$STATION=='2' & realTest$PARAMETER=='BACKWATER'
          ,]$PARAMETER <- ''

  #    Make some numeric values non-numeric
  realTest[realTest$UID=='10' & realTest$TRANSECT=='A' &
           realTest$STATION=='2' & realTest$PARAMETER=='WETWIDTH'
          ,]$RESULT <- 'BOB'
  realTest[realTest$UID=='10' & realTest$TRANSECT=='B' &
           realTest$STATION=='2' & realTest$PARAMETER=='BARWIDTH'
          ,]$RESULT <- ' '
  realTest[realTest$UID=='10' & realTest$TRANSECT=='C' &
           realTest$STATION=='2' & realTest$PARAMETER=='INCREMNT'
          ,]$RESULT <- '.'
  realTest[realTest$UID=='15' & realTest$TRANSECT=='D' &
           realTest$STATION=='2' & realTest$PARAMETER=='DEP_POLE'
          ,]$RESULT <- '$'
  realTest[realTest$UID=='15' & realTest$TRANSECT=='E' &
           realTest$STATION=='2' & realTest$PARAMETER=='DEP_SONR'
          ,]$RESULT <- ','

  #    Make some values absent, which will also create duplication errors
  realTest[realTest$UID=='2' & realTest$TRANSECT=='B' &
           realTest$STATION=='2' & realTest$PARAMETER=='BACKWATER'
          ,]$TRANSECT <- 'C'
  realTest[realTest$UID=='3' & realTest$TRANSECT=='C' &
           realTest$STATION=='3' & realTest$PARAMETER=='BACKWATER'
          ,]$STATION <- '4'
  realTest[realTest$UID=='4' & realTest$TRANSECT=='D' &
           realTest$STATION=='3' & realTest$PARAMETER=='BACKWATER'
          ,]$PARAMETER <- 'BAR_PRES'

  #    Make some unexpected values
  realTest[realTest$UID=='5' & realTest$TRANSECT=='E' &
           realTest$STATION=='5' & realTest$PARAMETER=='BACKWATER'
          ,]$PARAMETER <- 'DEP_POLE'
  realTest[realTest$UID=='15' & realTest$TRANSECT=='E' &
           realTest$STATION=='5' & realTest$PARAMETER=='DEP_POLE'
          ,]$PARAMETER <- 'DEPTH'
  realTest[realTest$UID=='6' & realTest$TRANSECT=='F' &
           realTest$STATION=='6' & realTest$PARAMETER=='BACKWATER'
          ,]$STATION <- '15'
  realTest[realTest$UID=='16' & realTest$TRANSECT=='F' &
           realTest$STATION=='6' & realTest$PARAMETER=='DEP_POLE'
          ,]$PARAMETER <- '15'

  
  # Check for false positives with good data
  rr <- valStructThalweg(baseTest, test='all')
  checkEquals(NULL, rr
             ,"Error: Detected errors where there are none"
             )
  
  # Check for false negatives with bad data
  rr <- valStructThalweg(realTest, test='all')
  ee <- rbind("Column UID has 1 missing values"
             ,"Column TRANSECT has 1 missing values"
             ,"Column STATION has 1 missing values"
             ,"Column PARAMETER has 1 missing values"
             ,"Column SAMPLE_TYPE has 1 missing values"
             ,"Unexpected value STATION=() at (UID,TRANSECT)=(1,C)"
             ,"Unexpected value STATION=(15) at (UID,TRANSECT)=(6,F)"
             ,"Unexpected value TRANSECT=() at (UID)=(1)"
             ,"Unexpected row count at (UID,TRANSECT,STATION,PARAMETER)=(2,C,2,BACKWATER), n=2"
             ,"Unexpected row count at (UID,TRANSECT,STATION,PARAMETER)=(3,C,4,BACKWATER), n=2"
             ,"Unexpected row count at (UID,TRANSECT,STATION,PARAMETER)=(4,D,3,BAR_PRES), n=2"
             ,"Column RESULT has 6 non-numeric values"
             ,"Absent STATION=(0,1,3,4,5,6,7,8,9) value at UID,TRANSECT=(,A) "
             ,"Absent STATION=(0,1,3,4,5,6,7,8,9) value at UID,TRANSECT=(1,) "
             ,"Absent TRANSECT=(B,C,D,E,F,G,H,I,J) value at UID=() "
             ,"Unexpected value PARAMETER=() at (UID,TRANSECT,STATION)=(1,E,2)"
             ,"Unexpected value PARAMETER=(15) at (UID,TRANSECT,STATION)=(16,F,6)"
             ,"Unexpected value SAMPLE_TYPE=() at (UID,TRANSECT,STATION)=(1,D,2)"
             )
  checkEquals(ee, rr
             ,"Error: Did not properly detect errors where they exist"
             )

}

# end of file