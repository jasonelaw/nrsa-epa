# valStructChanChar.r
#
# 09/24/09 cws Created valStructChancov
# 10/05/09  SSR adapted for tblCHANNELCHAR2
# 10/27/2009  mrc Changed expected value of TRANSECT from "XX" to "NONE"
# 10/28/2009 cws Updated unit test to use NONE as well.
#  2/25/2010 cws moved source() calls to NRSAvalidation.r

# Contains functions valStructChanChar

require(RODBC)
#intermediateMessages <- TRUE

valStructChanChar <- function(df, test='all')
# Performs structure checks on the NRSA table tblCHANCHAR2.  Returns NULL if
# no errors have been found, or an Nx1 character matrix describing the detected
# errors.
#
# ARGUMENTS:
# df        dataframe of NRSA channel cover data
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
  intermediateMessage('Structure validation of channel cover data ', loc='start')
  
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


      # Check for unexpected TRANSECT values
      intermediateMessage('.6')
      pp <- stValUnexpectedValues(df, 'TRANSECT', c('NONE'), 'UID'
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
  
  # Perform nonvital tests if requested
  if(test %in% c('all','nonvital','synopsis')) {

      # Check for absent TRANSECT values at each UID
      intermediateMessage('.8')
      pp <- stValAbsentValues(df, 'TRANSECT'
                             ,c('NONE')
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
                                 ,c('BANKFULL', 'CONSTRNT', 'FEATURES', 'PATTERN',
                                    'PERCENT', 'VALLEY', 'VALLYBOX')
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
                                 ,c('BANKFULL', 'CONSTRNT', 'FEATURES', 'PATTERN',
                                    'PERCENT', 'VALLEY', 'VALLYBOX')
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
                                 ,c('PHAB_CHCON')
                                 ,c('UID', 'TRANSECT')
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



valStructChanCharTest <- function()
# Tests valStructChanChar()
{
  # Create test data frame
  baseTest <- rbind(expand.grid(UID=as.character(1000 + 1:10)
                               ,TRANSECT='NONE'
                               ,SAMPLE_TYPE='PHAB_CHCON'
                               ,PARAMETER=c('BANKFULL','CONSTRNT','FEATURES',
                               'PATTERN','PERCENT','VALLEY','VALLYBOX')
                               )
                   )
  baseTest$UID <- as.character(baseTest$UID)
  baseTest$TRANSECT <- as.character(baseTest$TRANSECT)
  baseTest$SAMPLE_TYPE <- as.character(baseTest$SAMPLE_TYPE)
  baseTest$PARAMETER <- as.character(baseTest$PARAMETER)
  baseTest$RESULT <- floor(runif(nrow(baseTest), 0, 18))
  baseTest$FLAG <- ''


  # Create imperfect/real dataframe to check for false negatives
  realTest <- baseTest

  #    make key values missing
  is.na(realTest$UID) <- c(13)
  is.na(realTest$TRANSECT) <- c(24)
  is.na(realTest$PARAMETER) <- c(46)
  is.na(realTest$SAMPLE_TYPE) <- c(02)

  #    unexpected values of keys
  realTest[38,]$TRANSECT <- 'A'
  realTest[5,]$SAMPLE_TYPE <- 'PHAB_WRONG'
  realTest[19,]$PARAMETER <- 'WRONG'

  # Test perfect dataframe to look for false positives
  rr <- valStructChanChar(baseTest, test='all')
  checkEquals(NULL, rr
             ,'Error: Detected structure problems in perfect Channel cover dataframe'
             )
             
  # Test real datafram to look for false negatives
  rr <- valStructChanChar(realTest, test='all')
  
  ee <- as.matrix(rbind(
   "Column UID has 1 missing values"                                                                      
  ,"Column TRANSECT has 1 missing values"                                                                 
  ,"Column PARAMETER has 1 missing values"                                                                
  ,"Column SAMPLE_TYPE has 1 missing values"                                                              
  ,"Unexpected value TRANSECT=(NA) at (UID)=(1004)"                                                       
  ,"Unexpected value TRANSECT=(A) at (UID)=(1008)"                                                        
  ,"Unexpected value PARAMETER=(WRONG) at (UID,TRANSECT)=(1009,NONE)"
  ,"Unexpected value PARAMETER=(NA) at (UID,TRANSECT)=(1006,NONE)"
  ,"Absent PARAMETER=(BANKFULL,CONSTRNT,PATTERN,PERCENT,VALLEY,VALLYBOX) value at UID,TRANSECT=(1004,NA) "
  ,"Absent PARAMETER=(BANKFULL,CONSTRNT,FEATURES,PERCENT,VALLEY,VALLYBOX) value at UID,TRANSECT=(1008,A) "
  ,"Unexpected value SAMPLE_TYPE=(NA) at (UID,TRANSECT)=(1002,NONE)"
  ,"Unexpected value SAMPLE_TYPE=(PHAB_WRONG) at (UID,TRANSECT)=(1005,NONE)"
                ))
  checkEquals(ee, rr
             ,'Error: Did not correctly find structure problems in imperfect Channel cover data'
             )

}

# end of file