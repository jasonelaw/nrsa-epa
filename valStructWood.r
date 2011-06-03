# valStructWood.r
#
# 10/09/2009 mrc Created
#  2/25/2010 cws moved source() calls to NRSAvalidation.r
# first must open a channel to desired db

require(RODBC)

#intermediateMessages <- TRUE


#get and fix the Wood table






valStructWood <- function(df, test='all')

# Performs structure checks on the NRSA table tblWood2.  Returns NULL if
# no errors have been found, or an Nx1 character matrix describing the detected
# errors.
#
# ARGUMENTS:
# df        dataframe of NRSA Wood data
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
  intermediateMessage('Structure validation of Wood data ', loc='start')
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
  probs <- stValColumnPresence(df, c('UID', 'TRANSECT', 'PARAMETER'
                                    ,'RESULT', 'FLAG' 
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
#---------------------------------MISSING VALUES---------------------------------#
      # Check for missing UID values
      intermediateMessage('.2missingUIDValues', loc='end')
      pp <- stValMissingValues(df, 'UID' ,timing=timeThis)
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
      intermediateMessage('.3MissingTransectValues')
      pp <- stValMissingValues(df, 'TRANSECT' ,timing=timeThis)
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
      intermediateMessage('.4missingParameterValues', loc='end')
      pp <- stValMissingValues(df, 'PARAMETER' ,timing=timeThis)
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
      intermediateMessage('.5missingSampleType', loc='end')
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
          
      
 #-------------------------UNEXPECTED VALUES-------------------------------#     
 
      
     # Check for unexpected Sample_type values
        intermediateMessage('.6unexpectedSampleType', loc='end')
      pp <- stValUnexpectedValues (df,'SAMPLE_TYPE'
                                 ,c('PHAB_THALW', 'PHAB_CHANBFRONT'
                                 )
                                 ,c('UID')  ,timing=timeThis
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

   # Check for unexpected PARAMETER values
      intermediateMessage('.7unexpectedPARAMETER', loc='end')
        pp <- stValUnexpectedValues(df
                                 ,'PARAMETER'
                                 ,c('DLDLL', 'DLDML','DLDSL'
                                    ,'DMDLL', 'DMDML','DMDSL'
                                    ,'DSDLL','DSDML','DSDSL'
                                    ,'DXDLL','DXDML','DXDSL'
                                    ,'WLDLL','WLDML','WLDSL'
                                    ,'WMDLL','WMDML','WMDSL'
                                    ,'WSDLL','WSDML','WSDSL'
                                    ,'WXDLL', 'WXDML','WXDSL'
                                    )
                                 ,c('UID', 'TRANSECT')  ,timing=timeThis
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Unexpected PARAMETER values: %d (vital)"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }

    # Check for unexpected TRANSECT values
      intermediateMessage('.8unexpectedTransect', loc='end')
      pp <- stValUnexpectedValues (df,'TRANSECT'
                                 ,c('A','B','C','D','E','F','G','H','I','J','K', 'X'
                                 ,'XA', 'XB', 'XC', 'XD', 'XE', 'XF', 'XG', 'XH', 'XI', 'XJ','XK'
                                 )
                                 ,c('UID')  ,timing=timeThis
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

#----------------------------UNIQUE RECORDS---------------------#

     #  Check for uniqueness of each UID-TRANSECT-SAMPLE_TYPE-PARAMETER
      intermediateMessage('.9uniqueUID*TRANSECT*SAMPLE_TYPE*PARAMETER')
      pp <- stValCountRows(df, c('UID','TRANSECT', 'SAMPLE_TYPE','PARAMETER'), 1 ,timing=timeThis)
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Duplicate UID-TRANSECT-SAMPLE_TYPE-PARAMETER values: %d (vital)"
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

#--------------------END OF valStructWood------------------------------------#

valStructWoodTest <- function()
# Tests valStructWood()

#cseq = function(from, to, by=1){
#       from=charToInt (from)
#       to=charToInt (to)
#       intToChar(seq(from,to,by))
#       }
       


{
  # Create test data frame
  baseTest <- rbind(expand.grid(UID=as.character(1000 + 1:10)
                               ,TRANSECT=LETTERS[1:11]
                               ,SAMPLE_TYPE=c('PHAB_THALW','PHAB_CHANBFRONT')
                               ,PARAMETER=c('DLDLL', 'DLDML','DLDSL'
                                    ,'DMDLL','DMDML','DMDSL'
                                    ,'DSDLL','DSDML','DSDSL'
                                    ,'DXDLL','DXDML','DXDSL'
                                    ,'WLDLL','WLDML','WLDSL'
                                    ,'WMDLL','WMDML','WMDSL'
                                    ,'WSDLL','WSDML','WSDSL'
                                    ,'WXDLL','WXDML','WXDSL'
                                    )
                                 )
                                )                                       
                                
                
  baseTest$UID <- as.character(baseTest$UID)
  baseTest$SAMPLE_TYPE <- as.character(baseTest$SAMPLE_TYPE)
  baseTest$PARAMETER <- as.character(baseTest$PARAMETER)
  baseTest$RESULT <- floor(runif(nrow(baseTest), 0, 18))
  baseTest$FLAG <- ''
 
  

  # Create imperfect/real dataframe to check for false negatives
  realTest <- baseTest

  
  #    change parameter by substituting it with illegal values
  realTest[c(201,1000,506),]$PARAMETER <- paste('X'
                                          ,realTest[c(50,510,1090),]$PARAMETER
                                          ,sep=''
                                          )

  #    make key values missing
  is.na(realTest$UID) <- c(10,22)
  is.na(realTest$PARAMETER) <- c(11,21)
 

  #    unexpected values of keys
  
  realTest[c(11,31,31),]$SAMPLE_TYPE <- 'DODO'
  realTest[c(11,31,51),]$PARAMETER <- 'WRONG'

  #    non-unique values of combinations of UID-PARAMETER
  realTest[c(11,41,45),]$PARAMETER <- ifelse(realTest[c(11,41,45),]$PARAMETER=='WXDSL'
                                              ,'WXDSL'
                                              ,'WXDSL'
                                              )
  

  # Test perfect dataframe to look for false positives
  rr <- valStructWood(baseTest, test='all')
  checkEquals(NULL, rr
             ,'Error: Detected structure problems in perfect other invasives dataframe'
             )
             
  # Test real dataframe to look for false negatives
  rr <- valStructWood(realTest, test='all')
  ee <- as.matrix(rbind(
               "Column UID has 2 missing values"                                                            
              ,"Column PARAMETER has 1 missing values"                                                      
              ,"Unexpected value SAMPLE_TYPE=(DODO) at (UID)=(1001)"                                        
              ,"Unexpected value SAMPLE_TYPE=(DODO) at (UID)=(1001)"                                        
              ,"Unexpected value PARAMETER=(NA) at (UID,TRANSECT)=(1001,3)"                                 
              ,"Unexpected value PARAMETER=(WRONG) at (UID,TRANSECT)=(1001,4)"                              
              ,"Unexpected value PARAMETER=(WRONG) at (UID,TRANSECT)=(1001,6)"                              
              ,"Unexpected value PARAMETER=(XDLDLL) at (UID,TRANSECT)=(1001,10)"                            
              ,"Unexpected value PARAMETER=(XDMDML) at (UID,TRANSECT)=(1006,7)"                             
              ,"Unexpected value PARAMETER=(XDLDSL) at (UID,TRANSECT)=(1010,1)"                             
              ,"Unexpected row count at (UID,TRANSECT,SAMPLE_TYPE,PARAMETER)=(1001,5,PHAB_THALW,WXDSL), n=2"
              ,"Unexpected row count at (UID,TRANSECT,SAMPLE_TYPE,PARAMETER)=(1005,5,PHAB_THALW,WXDSL), n=2"
                        
                  ))
  checkEquals(ee, rr
             ,'Error: Did not correctly find structure problems in imperfect Other Invasives data'
             )

}

# end of file

