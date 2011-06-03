# valStructLittoral.r
#
# 09/24/09 cws Created valStructChancov
# 10/06/09  SSR adapted for tblLITTORAL2
# 10/22/09 cws Added unit test.  Modified structure check to correct uniqueness
#          check and only allow main channel transects.
# 11/06/09  mrc - changed parameter values BOTTOMDOM and BOTTOMSEC to reflect
#           a real change in parameter values
#  2/22/10 cws Removed UNITS column from dataframe in unit test.
#  2/25/10 cws moved source() calls to NRSAvalidation.r
#

# Contains functions valStructLittoral 

require(RODBC)
#intermediateMessages <- TRUE

valStructLittoral <- function(df, test='all')
# Performs structure checks on the NRSA table tblLITTORAL2.  Returns NULL if
# no errors have been found, or an Nx1 character matrix describing the detected
# errors.
#
# ARGUMENTS:
# df        dataframe of NRSA littoral data
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
  intermediateMessage('Structure validation of littoral data ', loc='start')
  
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


      # Check for missing PARAMETER values
      intermediateMessage('.4')
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
      intermediateMessage('.5')
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


      # Check for unexpected TRANSECT values in Rivers
      intermediateMessage('.6')
      pp <- stValUnexpectedValues(df, 'TRANSECT'
                                 ,c('A','B','C','D','E','F','G','H','I','J','K'
                                   )
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


      # Check for uniqueness of each UID-TRANSECT-PARAMETER
      intermediateMessage('.7')
      pp <- stValCountRows(df, c('UID','TRANSECT','PARAMETER'), 1)
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Duplicate UID-TRANSECT values: %d (vital)"
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
                                 ,c('BOTTOMDOM', 'BOTTOMSEC', 'SHOREDOM',
                                    'SHORESEC', 'SUBOBS')
                                 ,c('UID', 'TRANSECT')
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

      # Check for absent PARAMETER values
      intermediateMessage('.10')
      pp <- stValAbsentValues(df
                                 ,'PARAMETER'
                                 ,c('BOTTOMDOM', 'BOTTOMSEC', 'SHOREDOM',
                                    'SHORESEC', 'SUBOBS')
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

      # Check for unexpected values of SAMPLE_TYPE.
      intermediateMessage('.11')
      pp <- stValUnexpectedValues(df
                                 ,'SAMPLE_TYPE'
                                 ,c('PHAB_CHANBFRONT')
                                 ,c('UID', 'TRANSECT','PARAMETER')
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

valStructLittoralTest <- function()
# Tests valStructLittoral()
{
  # Create example of perfect data
  baseTest <- expand.grid(UID=1:10
                         ,TRANSECT=LETTERS[1:11]
                         ,PARAMETER=c('BOTTOMDOM', 'BOTTOMSEC', 'SHOREDOM'
                                     ,'SHORESEC', 'SUBOBS'
                                     )
                         )
  baseTest$UID <- as.character(baseTest$UID)
  baseTest$TRANSECT <- as.character(baseTest$TRANSECT)
  baseTest$PARAMETER <- as.character(baseTest$PARAMETER)
  baseTest$SAMPLE_TYPE <- 'PHAB_CHANBFRONT'
  baseTest[baseTest$PARAMETER == 'SUBOBS','RESULT'] <- rep(c('JUDG','OBSV')
                                                          ,length.out=10*11
                                                          )
  baseTest[baseTest$PARAMETER != 'SUBOBS','RESULT'] <- rep(c('CB','FN','GC','GF'
                                                          ,'HP','OT','RR','RS'
                                                          ,'SA','SB','WD','XB'
                                                          )
                                                          ,length.out=10*11*4
                                                          )
  baseTest$FLAG <- as.character(NA)

  # Create imperfectly structured data based on perfect data.
  realTest <- baseTest
  
  #    Make some values missing, which will cause some absences and unexpected
  #    values appear as well
  realTest[realTest$UID=='1' & realTest$TRANSECT=='A',][1,]$UID <- ''
  realTest[realTest$UID=='2' & realTest$TRANSECT=='B',][1,]$TRANSECT <- ''
  realTest[realTest$UID=='3' & realTest$TRANSECT=='C',][1,]$PARAMETER <- ''
  realTest[realTest$UID=='4' & realTest$TRANSECT=='D',][1,]$SAMPLE_TYPE <- ''

  #    Make some values absent, which will cause some duplicates to appear too
  realTest[realTest$UID=='5' & realTest$TRANSECT=='E' &
           realTest$PARAMETER=='BOTTOMDOM',]$TRANSECT <- 'D'
  realTest[realTest$UID=='6' & realTest$TRANSECT=='F' &
           realTest$PARAMETER=='BOTTOMDOM',]$PARAMETER <- 'SHOREDOM'

  #    Make some unexpected values
  realTest[realTest$UID=='7' & realTest$TRANSECT=='G' &
           realTest$PARAMETER=='BOTTOMDOM',]$PARAMETER <- 'WRONG'
  realTest[realTest$UID=='8' & realTest$TRANSECT=='H' &
           realTest$PARAMETER=='BOTTOMDOM',]$TRANSECT <- 'XH'
  realTest[realTest$UID=='9' & realTest$TRANSECT=='I' &
           realTest$PARAMETER=='BOTTOMDOM',]$SAMPLE_TYPE <- 'ODD TYPE'

  
  # Check for false positives with perfect data
  rr <- valStructLittoral(baseTest, test='all')
  checkEquals(NULL, rr
             ,"Error: Detected errors where there are none"
             )
  
  
  # Check for false negatives with imperfect data
  rr <- valStructLittoral(realTest, test='all')
  ee <- rbind("Column UID has 1 missing values"
             ,"Column TRANSECT has 1 missing values"
             ,"Column PARAMETER has 1 missing values"
             ,"Column SAMPLE_TYPE has 1 missing values"
             ,"Unexpected value TRANSECT=() at (UID)=(2)"
             ,"Unexpected value TRANSECT=(XH) at (UID)=(8)"
             ,"Unexpected row count at (UID,TRANSECT,PARAMETER)=(5,D,BOTTOMDOM), n=2"
             ,"Unexpected row count at (UID,TRANSECT,PARAMETER)=(6,F,SHOREDOM), n=2"
             ,"Absent TRANSECT=(B,C,D,E,F,G,H,I,J,K) value at UID=() "
             ,"Unexpected value PARAMETER=() at (UID,TRANSECT)=(3,C)"
             ,"Unexpected value PARAMETER=(WRONG) at (UID,TRANSECT)=(7,G)"
             ,"Absent PARAMETER=(BOTTOMSEC,SHOREDOM,SHORESEC,SUBOBS) value at UID,TRANSECT=(2,) "
             ,"Absent PARAMETER=(BOTTOMDOM) value at UID,TRANSECT=(2,B) "
             ,"Absent PARAMETER=(BOTTOMDOM) value at UID,TRANSECT=(3,C) "
             ,"Absent PARAMETER=(BOTTOMDOM) value at UID,TRANSECT=(5,E) "
             ,"Absent PARAMETER=(BOTTOMDOM) value at UID,TRANSECT=(6,F) "
             ,"Absent PARAMETER=(BOTTOMDOM) value at UID,TRANSECT=(7,G) "
             ,"Absent PARAMETER=(BOTTOMDOM) value at UID,TRANSECT=(8,H) "
             ,"Absent PARAMETER=(BOTTOMSEC,SHOREDOM,SHORESEC,SUBOBS) value at UID,TRANSECT=(8,XH) "
             ,"Unexpected value SAMPLE_TYPE=() at (UID,TRANSECT,PARAMETER)=(4,D,BOTTOMDOM)"
             ,"Unexpected value SAMPLE_TYPE=(ODD TYPE) at (UID,TRANSECT,PARAMETER)=(9,I,BOTTOMDOM)"
             )
  checkEquals(ee, rr
             ,"Error: Detected errors where there are none"
             )


}

# end of file