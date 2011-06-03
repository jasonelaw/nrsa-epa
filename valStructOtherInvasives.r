# valStructOtherInvasives.r
#
# 10/08/2009 mrc Created
# 10/22/2009 add timeThis
#  2/25/2010 cws moved source() calls to NRSAvalidation.r
# first must open a channel to desired db

require(RODBC)

#intermediateMessages <- TRUE


#get and fix the OtherInvasives table


valStructOtherInvasives <- function(df, test='all')

# Performs structure checks on the NRSA table tblOtherInvasives2.  Returns NULL if
# no errors have been found, or an Nx1 character matrix describing the detected
# errors.
#
# ARGUMENTS:
# df        dataframe of NRSA Other Invasives data
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
  intermediateMessage('Structure validation of other invasives data ', loc='start')
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
  probs <- stValColumnPresence(df, c('UID', 'PARAMETER'
                                    ,'RESULT', 'FLAG' 
                                    ,'SAMPLE_TYPE', 'COMMON_NAME'
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
      pp <- stValMissingValues(df, 'UID' , timing=timeThis)
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
      pp <- stValMissingValues(df, 'PARAMETER' , timing=timeThis)
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
      pp <- stValMissingValues(df, 'SAMPLE_TYPE' , timing=timeThis)
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
          # Check for missing COMMON NAME values
      intermediateMessage('.5missingCommonName', loc='end')
      pp <- stValMissingValues(df, 'COMMON_NAME' , timing=timeThis)
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,ifelse(is.null(pp)
                               ,"No Missing COMMON NAME values"
                               ,"Missing COMMON NAME values exist (vital)"
                               )
                        )
      } else {
          probs <- rbind(probs, pp)
      }
      
 #-------------------------UNEXPECTED VALUES-------------------------------#     
 
      
     # Check for unexpected Sample_type values
        intermediateMessage('.6unexpectedSampleType', loc='end')
      pp <- stValUnexpectedValues (df,'SAMPLE_TYPE'
                                 ,c('INVA')
                                 , c('UID')  , timing=timeThis
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
      pp <- stValUnexpectedValues (df,'PARAMETER'
                                 ,c('CONFIDENCE', 'PREVALANCE')
                                 ,c('UID')  , timing=timeThis
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



#----------------------------UNIQUE RECORDS---------------------#

     #  Check for uniqueness of each UID-PARAMETER-COMMON_NAME
      intermediateMessage('.8uniqueUID*PARAMETER*COMMON_NAME', loc='end')
      pp <- stValCountRows(df, c('UID','PARAMETER','COMMON_NAME'), 1 , timing=timeThis)
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Duplicate UID-PARAMETER_COMMON_NAME values: %d (vital)"
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

#--------------------END OF valStructOtherInvasives------------------------------------#

valStructOtherInvasivesTest <- function()
# Tests valStructOtherInvasives()

#cseq = function(from, to, by=1){
#       from=charToInt (from)
#       to=charToInt (to)
#       intToChar(seq(from,to,by))
#       }
       


{
  # Create test data frame
  baseTest <- rbind(expand.grid(UID=as.character(1000 + 1:10)
                               ,SAMPLE_TYPE='INVA'
                               ,PARAMETER=c('CONFIDENCE', 'PREVALANCE')
                               ,COMMON_NAME=LETTERS[1:26]
                               )
                
                   )
  baseTest$UID <- as.character(baseTest$UID)
  baseTest$SAMPLE_TYPE <- as.character(baseTest$SAMPLE_TYPE)
  baseTest$PARAMETER <- as.character(baseTest$PARAMETER)
  baseTest$COMMON_NAME <- as.character(baseTest$COMMON_NAME)
  baseTest$RESULT <- floor(runif(nrow(baseTest), 0, 18))
  baseTest$FLAG <- ''
 
  

  # Create imperfect/real dataframe to check for false negatives
  realTest <- baseTest

  
  #    change parameter by substituting it with illegal values
  realTest[c(02,102,502),]$PARAMETER <- paste('X'
                                          ,realTest[c(5,51,151),]$PARAMETER
                                          ,sep=''
                                          )

  #    make key values missing
  is.na(realTest$UID) <- c(103,220)
  is.na(realTest$PARAMETER) <- c(104,212)
  is.na(realTest$COMMON_NAME) <- c(63,88)
  

  #    unexpected values of keys
  
  realTest[c(110,310,311),]$SAMPLE_TYPE <- 'DODO'
  realTest[c(111,311,511),]$PARAMETER <- 'WRONG'

  #    non-unique values of combinations of UID-COMMON_NAME-PARAMETER
  realTest[c(113,413,450),]$COMMON_NAME <- ifelse(realTest[c(113,413,450),]$COMMON_NAME=='G'
                                              ,'G'
                                              ,'G'
                                              )
  

  # Test perfect dataframe to look for false positives
  rr <- valStructOtherInvasives(baseTest, test='all')
  checkEquals(NULL, rr
             ,'Error: Detected structure problems in perfect other invasives dataframe'
             )
             
  # Test real dataframe to look for false negatives
  rr <- valStructOtherInvasives(realTest, test='all')
  ee <- as.matrix(rbind(
             "Column UID has 2 missing values"                                             
              ,"Column PARAMETER has 2 missing values"                                       
              ,"Column COMMON_NAME has 2 missing values"                                     
              ,"Unexpected value SAMPLE_TYPE=(DODO) at (UID)=(1010)"                         
              ,"Unexpected value SAMPLE_TYPE=(DODO) at (UID)=(1010)"                         
              ,"Unexpected value SAMPLE_TYPE=(DODO) at (UID)=(1001)"                         
              ,"Unexpected value PARAMETER=(XCONFIDENCE) at (UID)=(1002)"                    
              ,"Unexpected value PARAMETER=(XPREVALANCE) at (UID)=(1002)"                    
              ,"Unexpected value PARAMETER=(NA) at (UID)=(1004)"                             
              ,"Unexpected value PARAMETER=(WRONG) at (UID)=(1001)"                          
              ,"Unexpected value PARAMETER=(NA) at (UID)=(1002)"                             
              ,"Unexpected value PARAMETER=(WRONG) at (UID)=(1001)"                          
              ,"Unexpected value PARAMETER=(XPREVALANCE) at (UID)=(1002)"                    
              ,"Unexpected value PARAMETER=(WRONG) at (UID)=(1001)"                          
              ,"Unexpected row count at (UID,PARAMETER,COMMON_NAME)=(1010,CONFIDENCE,G), n=2"
              ,"Unexpected row count at (UID,PARAMETER,COMMON_NAME)=(1003,PREVALANCE,G), n=3"

                  ))
  checkEquals(ee, rr
             ,'Error: Did not correctly find structure problems in imperfect Other Invasives data'
             )

}

# end of file

