# valStructBenthic.r
#
# 10/01/2009 mrc Created
# 10/21/2009 added timeThis
# 12/07/2009 mrc: changed sample_types to reflect new reality
#  2/25/2010 cws moved source() calls to NRSAvalidation.r
# first must open a channel to desired db

require(RODBC)
#intermediateMessages <- TRUE



valStructBenthic <- function(df, test='all')
# Performs structure checks on the NRSA table tblBENTHIC2.  Returns NULL if
# no errors have been found, or an Nx1 character matrix describing the detected
# errors.
#
# ARGUMENTS:
# df        dataframe of NRSA benthic data
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
  intermediateMessage('Structure validation of benthic data ', loc='end')
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
  probs <- stValColumnPresence(df, c('UID', 'TRANSECT',
                                    'PARAMETER', 'RESULT', 
                                    'SAMPLE_TYPE' 
                                    ) , timing=timeThis
                              )
  if(!is.null(probs)) {
      colnames(probs)[1] <-'User.MUST.fix.these.errors.before.proceding'
      intermediateMessage('.  Done (terminated early).', loc='end')
      return(probs)
  }
  
  #  Perform 'vital' tests if requested
  if(test %in% c('all','vital','synopsis')) {
#-----------------------------MISSING VALUES-----------------------------------#
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



      # Check for missing PARAMETER values
      intermediateMessage('.4missingParameterValues', 'loc=end')
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
      intermediateMessage('.5missingSampleType', loc='end')
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

#-------------------------UNEXPECTED VALUES------------------------------------#
      # Check for unexpected TRANSECT values
      intermediateMessage('.6unexpectedTransect', loc='end')
      pp <- stValUnexpectedValues (df,'TRANSECT'
                                 ,c('A','B','C','D','E','F','G','H','I','J','K')
                                 ,'UID', timing=timeThis
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
         # Check for unexpected PARAMETER values
      intermediateMessage('.7unexpectedParameter', loc='end')
      pp <- stValUnexpectedValues(df
                                 ,'PARAMETER'
                                 ,c('SECOND_HABITAT', 'LOCATION', 'CHANNEL', 'SUBSTRATE', 'EDGE', 'HABITAT')
                                 ,c('UID', 'TRANSECT'), timing=timeThis
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
           # Check for unexpected values of SAMPLE_TYPE.
      intermediateMessage('.8unexpectedSampleType', loc='end')
      pp <- stValUnexpectedValues(df
                                 ,'SAMPLE_TYPE'
                                 ,c('BERWW','BELGW', 'BELGB')
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
      # Check for uniqueness of each UID-TRANSECT-PARAMETER
      intermediateMessage('.8uniqueUID*Transect*Parameter', loc='end')
      pp <- stValCountRows(df, c('UID','TRANSECT', 'PARAMETER', 'SAMPLE_TYPE'), timing=timeThis, 1)
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Duplicate UID-TRANSECT-PARAMETER-SAMPLE_TYPE values: %d (vital)"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }
  
}
  # Perform nonvital tests if requested
if(test %in% c('all','nonvital','synopsis')) {
 #----------------------------ABSENT VALUES-------------------------------------#
      # Check for absent TRANSECT values at each UID
      intermediateMessage('.9absentTransectValues', loc='end')
      pp <- stValAbsentValues(df, 'TRANSECT'
                             ,c('A','B','C','D','E','F','G','H','I','J','K')
                             ,'UID'  , timing=timeThis
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
      intermediateMessage('.10absentParameter', loc='end')
      pp <- stValAbsentValues(df
                                 ,'PARAMETER'
                                ,c('SECOND_HABITAT', 'LOCATION', 'CHANNEL', 'SUBSTRATE', 'EDGE', 'HABITAT')
                                 ,c('UID', 'TRANSECT')  , timing=timeThis
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



valStructBenthicTest <- function()
# Tests valStructBenthic()
{
  # Create test data frame
  baseTest <- rbind(expand.grid(UID=as.character(1000 + 1:10)
                               ,TRANSECT=LETTERS[1:11]
                               ,SAMPLE_TYPE='BELGB'
                               ,PARAMETER=c('HABITAT','SECOND_HABITAT','LOCATION','CHANNEL' ,'SUBSTRATE','EDGE')
                               )
                    ,expand.grid(UID=as.character(1000 + 1:10)
                               ,TRANSECT=LETTERS[1:11]
                               ,SAMPLE_TYPE='BELGW'
                               ,PARAMETER=c('HABITAT','SECOND_HABITAT','LOCATION','CHANNEL' ,'SUBSTRATE','EDGE')
                               )
                   ,expand.grid(UID=as.character(2000 + 1:10)
                               ,TRANSECT=LETTERS[1:11]
                               ,SAMPLE_TYPE='BERWW'
                               ,PARAMETER=c('HABITAT','SECOND_HABITAT','LOCATION','CHANNEL' ,'SUBSTRATE','EDGE')
                               )
                   )
  baseTest$UID <- as.character(baseTest$UID)
  baseTest$TRANSECT <- as.character(baseTest$TRANSECT)
  baseTest$SAMPLE_TYPE <- as.character(baseTest$SAMPLE_TYPE)
  baseTest$PARAMETER <- as.character(baseTest$PARAMETER)
  baseTest$RESULT <- floor(runif(nrow(baseTest), 0, 18))
  

  # Create imperfect/real dataframe to check for false negatives
  realTest <- baseTest

  #    remove transect by removing it from dataframe
  realTest <- subset(realTest, !(row.names(realTest) %in% c(201,301,401)))

  #    remove transect by substituting it with legal values
  realTest[c(02,102,502),]$TRANSECT <- paste('X'
                                          ,realTest[c(5,51,151),]$TRANSECT
                                          ,sep=''
                                          )

  #    make key values missing
  is.na(realTest$UID) <- c(103,503,903)
  is.na(realTest$TRANSECT) <- c(104,404,604)
  is.na(realTest$PARAMETER) <- c(106,206,306)

  #    unexpected values of keys
  realTest[c(08,308,508),]$TRANSECT <- 'L'
  realTest[c(110,310,810),]$SAMPLE_TYPE <- 'BEN_WRONG'
  realTest[c(111,311,511),]$PARAMETER <- 'WRONG'

  #    non-unique values of combinations of UID-TRANSECT-PARAMETER
  realTest[c(113,413,713),]$TRANSECT <- ifelse(realTest[c(113,413,713),]$TRANSECT=='G'
                                              ,'H'
                                              ,'G'
                                              )
  

  # Test perfect dataframe to look for false positives
  rr <- valStructBenthic(baseTest, test='all')
  checkEquals(NULL, rr
             ,'Error: Detected structure problems in perfect benthic dataframe'
             )
             
  # Test real dataframe to look for false negatives
  rr <- valStructBenthic(realTest, test='all')
  ee <- as.matrix(rbind(
           "Column UID has 3 missing values"                                                                   
,"Column TRANSECT has 3 missing values"                                                              
,"Column PARAMETER has 3 missing values"                                                             
,"Unexpected value TRANSECT=(XA) at (UID)=(1002)"                                                    
,"Unexpected value TRANSECT=(L) at (UID)=(1008)"                                                     
,"Unexpected value TRANSECT=(XF) at (UID)=(1002)"                                                    
,"Unexpected value TRANSECT=(NA) at (UID)=(1004)"                                                    
,"Unexpected value TRANSECT=(L) at (UID)=(1010)"                                                     
,"Unexpected value TRANSECT=(NA) at (UID)=(1007)"                                                    
,"Unexpected value TRANSECT=(XE) at (UID)=(1005)"                                                    
,"Unexpected value TRANSECT=(L) at (UID)=(1001)"                                                     
,"Unexpected value TRANSECT=(NA) at (UID)=(1007)"                                                    
,"Unexpected value PARAMETER=(NA) at (UID,TRANSECT)=(1006,K)"                                        
,"Unexpected value PARAMETER=(WRONG) at (UID,TRANSECT)=(1001,A)"                                     
,"Unexpected value PARAMETER=(NA) at (UID,TRANSECT)=(1007,J)"                                        
,"Unexpected value PARAMETER=(NA) at (UID,TRANSECT)=(1008,I)"                                        
,"Unexpected value PARAMETER=(WRONG) at (UID,TRANSECT)=(1003,J)"                                     
,"Unexpected value PARAMETER=(WRONG) at (UID,TRANSECT)=(1004,H)"                                     
,"Unexpected value SAMPLE_TYPE=(BEN_WRONG) at (UID,TRANSECT)=(1010,K)"                               
,"Unexpected value SAMPLE_TYPE=(BEN_WRONG) at (UID,TRANSECT)=(1002,J)"                               
,"Unexpected value SAMPLE_TYPE=(BEN_WRONG) at (UID,TRANSECT)=(1003,E)"                               
,"Unexpected row count at (UID,TRANSECT,PARAMETER,SAMPLE_TYPE)=(1006,G,CHANNEL,BELGB), n=2"           
,"Unexpected row count at (UID,TRANSECT,PARAMETER,SAMPLE_TYPE)=(1003,G,SECOND_HABITAT,BELGB), n=2"    
,"Unexpected row count at (UID,TRANSECT,PARAMETER,SAMPLE_TYPE)=(1006,G,HABITAT,BELGW), n=2"           
,"Absent TRANSECT=(A,B,D,E,F,H,I,J) value at UID=(NA) "                                              
,"Absent PARAMETER=(SECOND_HABITAT,LOCATION,CHANNEL,EDGE,HABITAT) value at UID,TRANSECT=(1001,L) "   
,"Absent PARAMETER=(SECOND_HABITAT,LOCATION,CHANNEL,SUBSTRATE,EDGE) value at UID,TRANSECT=(1002,XA) "
,"Absent PARAMETER=(SECOND_HABITAT,LOCATION,CHANNEL,SUBSTRATE,EDGE) value at UID,TRANSECT=(1002,XF) "
,"Absent PARAMETER=(SECOND_HABITAT,LOCATION,CHANNEL,SUBSTRATE,EDGE) value at UID,TRANSECT=(1004,NA) "
,"Absent PARAMETER=(SECOND_HABITAT,LOCATION,CHANNEL,EDGE,HABITAT) value at UID,TRANSECT=(1005,XE) "  
,"Absent PARAMETER=(SECOND_HABITAT,LOCATION,SUBSTRATE,HABITAT) value at UID,TRANSECT=(1007,NA) "     
,"Absent PARAMETER=(SECOND_HABITAT,LOCATION,CHANNEL,SUBSTRATE,EDGE) value at UID,TRANSECT=(1008,L) " 
,"Absent PARAMETER=(SECOND_HABITAT,CHANNEL,SUBSTRATE,EDGE,HABITAT) value at UID,TRANSECT=(1010,L) "  
,"Absent PARAMETER=(SECOND_HABITAT,CHANNEL,SUBSTRATE,EDGE,HABITAT) value at UID,TRANSECT=(NA,C) "    
,"Absent PARAMETER=(SECOND_HABITAT,LOCATION,CHANNEL,EDGE,HABITAT) value at UID,TRANSECT=(NA,G) "     
,"Absent PARAMETER=(SECOND_HABITAT,LOCATION,CHANNEL,SUBSTRATE,EDGE) value at UID,TRANSECT=(NA,K) "   

            
                  ))
  checkEquals(ee, rr
             ,'Error: Did not correctly find structure problems in imperfect Benthic data'
             )

}

# end of file
