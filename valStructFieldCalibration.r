# valStructFieldCalibration.r
#
# 10/08/2009 mrc Created
# 10/21/2009 added timeThis
#  2/25/2010 cws moved source() calls to NRSAvalidation.r
# first must open a channel to desired db

require(RODBC)

#intermediateMessages <- TRUE


#get and fix the fieldcalibration table





valStructFieldCalibration <- function(df, test='all')

# Performs structure checks on the NRSA table tblFieldCalibration2.  Returns NULL if
# no errors have been found, or an Nx1 character matrix describing the detected
# errors.
#
# ARGUMENTS:
# df        dataframe of NRSA Field Calibration data
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
  intermediateMessage('Structure validation of channel cross section data ', loc='start')
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
  probs <- stValColumnPresence(df, c('UID', 'UNITS'
                                    ,'PARAMETER', 'RESULT' 
                                    ,'SAMPLE_TYPE', 'FLAG'
                                    )
                                    , timing=timeThis
                              )
  if(!is.null(probs)) {
      colnames(probs)[1] <-'User.MUST.fix.these.errors.before.proceding'
      intermediateMessage('.  Done (terminated early).', loc='end')
      return(probs)
  }
  
  #  Perform 'vital' tests if requested
  if(test %in% c('all','vital','synopsis')) {
#---------------------------------MISSING VALUES---------------------------------#
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

      # Check for missing PARAMETER values
      intermediateMessage('.3missingParameterValues', loc='end')
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
      intermediateMessage('.4missingSampleType', loc='end')
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
          # Check for missing UNIT values
      intermediateMessage('.5missingUnits', loc='end')
      pp <- stValMissingValues(df, 'UNITS', timing=timeThis)
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,ifelse(is.null(pp)
                               ,"No Missing UNITS values"
                               ,"Missing UNITS values exist (vital)"
                               )
                        )
      } else {
          probs <- rbind(probs, pp)
      }
      
 #-------------------------UNEXPECTED VALUES-------------------------------#     
 
      
     # Check for unexpected Sample_type values
        intermediateMessage('.6unexpectedSampleType', loc = 'end')
      pp <- stValUnexpectedValues (df,'SAMPLE_TYPE'
                                 ,c('CALI'
                                 )
                                 ,c('UID') , timing=timeThis
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Unexpected Sample_Type values: %d (vital)"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }

  # Check for unexpected Parameter values
        intermediateMessage('.7unexpectedParameter', loc='end')
      pp <- stValUnexpectedValues (df,'PARAMETER'
                                 ,c('TEMP_SENSOR', 'DO_ELEVATION', 'PH_STD2_DESCRIPTION'
                                 ,'DO_BARO_PRESSURE', 'CAL_INST_OPERATOR', 'PH_QCS_MEASURED'
                                 ,'PH_QCS_TRUE', 'COND_QCS_DESC', 'CAL_INST_MODEL'
                                 ,'DO_DISPLAYED_VALUE','COND_STD1_VALUE','PH_STD1_DESCRIPTION'
                                 ,'TEMP_THERMOMETER','COND_STD1_DESC','COND_STD2_VALUE'
                                 ,'PH_QCS_DESC','COND_QCS_MEASURED','COND_QCS_TRUE'
                                 ,'DO_CALIBRATION_VALUE','PH_STD2_VALUE','PH_STD1_VALUE'
                                 ,'DO_PRESSURE','COND_STD2_DESC','CAL_INST_ID'
                                 )
                                 ,c('UID'), timing=timeThis
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Unexpected Parameter values: %d (vital)"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }



#----------------------------UNIQUE RECORDS---------------------#

     #  Check for uniqueness of each UID-PARAMETER
      intermediateMessage('.8uniqueUID*Parameter', loc='end')
      pp <- stValCountRows(df, c('UID','PARAMETER'), 1, timing=timeThis)
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
#---------------------------------Absent tests---------------------------------------#

  # Perform nonvital tests if requested
     if(test %in% c('all','nonvital','synopsis')) {


      # Check for absent PARAMETER values
      intermediateMessage('.9absentParameter', loc='end')
      pp <- stValAbsentValues(df
                                 ,'PARAMETER'
                                ,c('TEMP_SENSOR', 'DO_ELEVATION', 'PH_STD2_DESCRIPTION'
                                 ,'DO_BARO_PRESSURE', 'CAL_INST_OPERATOR', 'PH_QCS_MEASURED'
                                 ,'PH_QCS_TRUE', 'COND_QCS_DESC', 'CAL_INST_MODEL'
                                 ,'DO_DISPLAYED_VALUE','COND_STD1_VALUE','PH_STD1_DESCRIPTION'
                                 ,'TEMP_THERMOMETER','COND_STD1_DESC','COND_STD2_VALUE'
                                 ,'PH_QCS_DESC','COND_QCS_MEASURED','COND_QCS_TRUE'
                                 ,'DO_CALIBRATION_VALUE','PH_STD2_VALUE','PH_STD1_VALUE'
                                 ,'DO_PRESSURE','COND_STD2_DESC','CAL_INST_ID'
                                 )
                                 ,c('UID'), timing=timeThis
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



valStructFieldCalibrationTest <- function()
# Tests valStructFieldCalibration()
{
  # Create test data frame
  baseTest <- rbind(expand.grid(UID=as.character(1000 + 1:10)
                               ,SAMPLE_TYPE='CALI'
                               ,PARAMETER=c('TEMP_SENSOR', 'DO_ELEVATION', 'PH_STD2_DESCRIPTION'
                                 ,'DO_BARO_PRESSURE', 'CAL_INST_OPERATOR', 'PH_QCS_MEASURED'
                                 ,'PH_QCS_TRUE', 'COND_QCS_DESC', 'CAL_INST_MODEL'
                                 ,'DO_DISPLAYED_VALUE','COND_STD1_VALUE','PH_STD1_DESCRIPTION'
                                 ,'TEMP_THERMOMETER','COND_STD1_DESC','COND_STD2_VALUE'
                                 ,'PH_QCS_DESC','COND_QCS_MEASURED','COND_QCS_TRUE'
                                 ,'DO_CALIBRATION_VALUE','PH_STD2_VALUE','PH_STD1_VALUE'
                                 ,'DO_PRESSURE','COND_STD2_DESC','CAL_INST_ID'
                                 )
                           )
                        )
                               
  baseTest$UID <- as.character(baseTest$UID)
  baseTest$SAMPLE_TYPE <- as.character(baseTest$SAMPLE_TYPE)
  baseTest$PARAMETER <- as.character(baseTest$PARAMETER)
  baseTest$RESULT <- floor(runif(nrow(baseTest), 0, 18))
  baseTest$FLAG <- ''
  baseTest$UNITS <- ' '
  
  

  # Create imperfect/real dataframe to check for false negatives
  realTest <- baseTest

 
 
  #    make key values missing
  is.na(realTest$UID) <- c(103)
  is.na(realTest$PARAMETER) <- c(106)

  #    unexpected values of keys
  realTest[c(110),]$SAMPLE_TYPE <- 'BADSAMP'
  realTest[c(111),]$PARAMETER <- 'WRONG'

  #    non-unique values of combinations of UID--PARAMETER
  realTest[c(113),]$PARAMETER <- ifelse(realTest[c(113),]$PARAMETER=='TEMP_THERMOMETER'
                                              ,'TEMP_THERMOMETER'
                                              ,'TEMP_THERMOMETER'
                                              )
  

  # Test perfect dataframe to look for false positives
  rr <- valStructFieldCalibration(baseTest, test='all')
  checkEquals(NULL, rr
             ,'Error: Detected structure problems in perfect FieldCalibration dataframe'
             )
             
  # Test real dataframe to look for false negatives
  rr <- valStructFieldCalibration(realTest, test='all')
  ee <- as.matrix(rbind(
            "Column UID has 1 missing values"                                                                                                                                                                                                                                                                                                                                                                          
            ,"Column PARAMETER has 1 missing values"                                                                                                                                                                                                                                                                                                                                                                    
            ,"Unexpected value SAMPLE_TYPE=(BADSAMP) at (UID)=(1010)"                                                                                                                                                                                                                                                                                                                                                   
            ,"Unexpected value PARAMETER=(NA) at (UID)=(1006)"                                                                                                                                                                                                                                                                                                                                                          
            ,"Unexpected value PARAMETER=(WRONG) at (UID)=(1001)"                                                                                                                                                                                                                                                                                                                                                       
            ,"Unexpected row count at (UID,PARAMETER)=(1003,TEMP_THERMOMETER), n=2"                                                                                                                                                                                                                                                                                                                                     
            ,"Absent PARAMETER=(PH_STD1_DESCRIPTION) value at UID=(1001) "                                                                                                                                                                                                                                                                                                                                              
            ,"Absent PARAMETER=(COND_STD1_VALUE,PH_STD1_DESCRIPTION) value at UID=(1003) "                                                                                                                                                                                                                                                                                                                              
            ,"Absent PARAMETER=(COND_STD1_VALUE) value at UID=(1006) "                                                                                                                                                                                                                                                                                                                                                  
            ,"Absent PARAMETER=(TEMP_SENSOR,DO_ELEVATION,PH_STD2_DESCRIPTION,DO_BARO_PRESSURE,CAL_INST_OPERATOR,PH_QCS_MEASURED,PH_QCS_TRUE,COND_QCS_DESC,CAL_INST_MODEL,DO_DISPLAYED_VALUE,PH_STD1_DESCRIPTION,TEMP_THERMOMETER,COND_STD1_DESC,COND_STD2_VALUE,PH_QCS_DESC,COND_QCS_MEASURED,COND_QCS_TRUE,DO_CALIBRATION_VALUE,PH_STD2_VALUE,PH_STD1_VALUE,DO_PRESSURE,COND_STD2_DESC,CAL_INST_ID) value at UID=(NA) "

                  ))
  checkEquals(ee, rr
             ,'Error: Did not correctly find structure problems in imperfect Field Calibration data'
             )

}

# end of file
