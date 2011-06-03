# valStructFieldMeasure.r
#
# 10/01/09 cws Created but incomplete.  Appears that SAMPLE_ID and BANK columns
#          may be truncated.  Also there may be multiple parameter spellings:
#          TEMP & TEMPERATURE, COND & CONDUCTIVITY.
# 10/30/2009 mrc.  Changed parameter values to only include TEMPERATURE and CONDUCTIVITY
#          BANK column was widened to include full value
#          SAMPLE_TYPE is as intended FIELDMEAS
#          Added valStructFieldMeasureTest
#  2/25/2010 cws moved source() calls to NRSAvalidation.r

#
require(RODBC)


valStructFieldMeasure <- function(df, test='all')
# Performs structure checks on the NRSA table tblFIELDMEASURE2.  Returns
# NULL if no errors have been found, or an Nx1 character matrix describing the
# detected errors.
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
  intermediateMessage('Structure validation of Field Measurements data ', loc='start')

  probs <- NULL

  # sanity checks
  if(!(test %in% c('all', 'vital', 'nonvital', 'synopsis'))) {
      probs<-"Error: test argument must be 'all', 'vital', 'nonvital' or 'synopsis'"
      return(as.matrix(probs))
  }

  # Check for correct column names no matter what, and stop early if there
  # are any errors, since nothing else will work without them
  intermediateMessage('.1', loc='end')
  probs <- stValColumnPresence(df, c('UID', 'TRANSECT', 'BANK', 'SAMPLE_TYPE'
                                    ,'PARAMETER', 'RESULT', 'FLAG'
                                    )
                              )
  if(!is.null(probs)) {
      colnames(probs)[1] <-'User.MUST.fix.these.errors.before.proceding'
      intermediateMessage('.  Done (terminated early).', loc='end')
      return(probs)
  }

  #  Perform 'vital' tests if requested
  if(test %in% c('all','vital','synopsis')) {
  
  #-----------------MISSING VALUES---------------------------------------------#
      # Check for missing UID values
      intermediateMessage('.2', loc='end')
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

      # Check for missing PARAMETER values
      intermediateMessage('.3', loc='end')
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
      intermediateMessage('.4', loc='end')
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
     #----------------------------UNIQUE RECORDS---------------------#

     #  Check for uniqueness of each UID-PARAMETER
      intermediateMessage('.5uniqueUID*Transect*Bank*Parameter', loc='end')
      pp <- stValCountRows(df, c('UID','PARAMETER', 'TRANSECT', 'BANK'), 1)
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Duplicate UID-PARAMETER values: %d (vital)"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }
  }
  
  # Perform nonvital tests if requested
  if(test %in% c('all','nonvital','synopsis')) {

      # Check for unexpected PARAMETER values
      intermediateMessage('.6', loc='end')
      pp <- stValUnexpectedValues(df
                                 ,'PARAMETER'
                                 ,c('CONDUCTIVITY','CORRECTED','DO','pH'
                                   ,'TEMPERATURE','TIME'
                                   ,'DISAPPEARS','REAPPEARS', 'CLEAR_TO_BOTTOM'
                                   )
                                 ,'UID'
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
      intermediateMessage('.7', loc='end')
      pp <- stValAbsentValues(df
                             ,'PARAMETER'
                             ,c('CONDUCTIVITY','CORRECTED','DO','pH'
                               ,'TEMPERATURE','TIME'
                               ,'DISAPPEARS','REAPPEARS', 'CLEAR_TO_BOTTOM'
                               )
                             ,'UID'
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

valStructFieldMeasureTest <- function()
# Tests valStructFieldMeasure()

{
  # Create test data frame
  baseTest <- rbind(expand.grid(UID=as.character(1000 + 1:10)
                               ,SAMPLE_TYPE='FIELDMEAS'
                               ,PARAMETER= c('CONDUCTIVITY','CORRECTED','DO','pH'
                                   ,'TEMPERATURE','TIME'
                                   ,'DISAPPEARS','REAPPEARS', 'CLEAR_TO_BOTTOM'
                                   )
                               ,TRANSECT  = LETTERS[1:11]
                               ,BANK =  c('CENTER', 'NOT MARKED', 'OTHER', 'LB'
                                   ,'MID-CHANNEL', 'RC', 'RB', 'LC') 
                           )
                        )
                               
  baseTest$UID <- as.character(baseTest$UID)
  baseTest$SAMPLE_TYPE <- as.character(baseTest$SAMPLE_TYPE)
  baseTest$PARAMETER <- as.character(baseTest$PARAMETER)
  baseTest$TRANSECT <- as.character(baseTest$TRANSECT)
  baseTest$BANK <- as.character(baseTest$BANK)
  baseTest$RESULT <- floor(runif(nrow(baseTest), 0, 18))
  baseTest$FLAG <- ''
 
  
  

  # Create imperfect/real dataframe to check for false negatives
  realTest <- baseTest

 
 
  #    make key values missing
  is.na(realTest$UID) <- c(103)
  is.na(realTest$PARAMETER) <- c(106)

  #    unexpected values of keys
  realTest[c(110),]$SAMPLE_TYPE <- 'BADSAMP'
  realTest[c(111),]$PARAMETER <- 'WRONG'

  #    non-unique values of combinations of UID--PARAMETER
  realTest[c(113),]$PARAMETER <- ifelse(realTest[c(113),]$PARAMETER=='TIME'
                                              ,'TIME'
                                              ,'TIME'
                                              )
  

  # Test perfect dataframe to look for false positives
  rr <- valStructFieldMeasure(baseTest, test='all')
  checkEquals(NULL, rr
             ,'Error: Detected structure problems in perfect Field Measure dataframe'
             )
             
  # Test real dataframe to look for false negatives
  rr <- valStructFieldMeasure(realTest, test='all')
  ee <- as.matrix(rbind(
             "Column UID has 1 missing values"                                                                               
             ,"Column PARAMETER has 1 missing values"                                                                         
             ,"Unexpected row count at (UID,PARAMETER,TRANSECT,BANK)=(1003,TIME,B,CENTER), n=2"                               
             ,"Unexpected value PARAMETER=(NA) at (UID)=(1006)"                                                               
             ,"Unexpected value PARAMETER=(WRONG) at (UID)=(1001)"                                                            
             ,"Absent PARAMETER=(CONDUCTIVITY,DO,pH,TEMPERATURE,TIME,DISAPPEARS,REAPPEARS,CLEAR_TO_BOTTOM) value at UID=(NA) "

                  ))
  checkEquals(ee, rr
             ,'Error: Did not correctly find structure problems in imperfect Field Measure data'
             )

}

# end of file