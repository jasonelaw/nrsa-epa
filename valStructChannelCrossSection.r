# valStructChannelCrossSection.r
#
# 10/06/2009 mrc Created
# 10/21/2009 mrc added timeThis
# 10/28/2009 mrc changed expected TRANSDIR from "X" to "NONE"
#  2/25/2010 cws moved source() calls to NRSAvalidation.r
# first must open a channel to desired db

require(RODBC)

#intermediateMessages <- TRUE


#get and fix the channelcrossection table





valStructChannelCrossSection <- function(df, test='all')

# Performs structure checks on the NRSA table tblCHANNELCROSSSECTION2.  Returns NULL if
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
  intermediateMessage('.1correctColumnNames')
  probs <- stValColumnPresence(df, c('UID', 'TRANSECT', 'TRANSDIR'
                                    ,'PARAMETER', 'RESULT' 
                                    ,'SAMPLE_TYPE', 'FLAG' 
                                    ), timing=timeThis
                                    
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
 
  # Check for missing TRANSDIR values
      intermediateMessage('.4MIssingTransdirValues', loc='end')
      pp <- stValMissingValues(df, 'TRANSDIR', timing=timeThis)
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,ifelse(is.null(pp)
                               ,"No Missing TRANSDIR values"
                               ,"Missing TRANSDIR values exist (vital)"
                               )
                        )
      } else {
          probs <- rbind(probs, pp)
      }


      # Check for missing PARAMETER values
      intermediateMessage('.5missingParameterValues', loc='end')
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
      intermediateMessage('.6missingSampleType', loc='end')
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
   # Check for unexpected TRANSDIR values in Streams
      intermediateMessage('.7UnexpectedTransdirW', loc='end')
      pp <- stValUnexpectedValues(subset(df, SAMPLE_TYPE=='PHAB_CHANW', timing=timeThis)
                                 ,'TRANSDIR'
                                 ,c('LF','RT','CT','RC','LC')
                                 ,c('UID', 'TRANSECT')
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Unexpected TRANSDIR values with SAMPLE_TYPE='PHAB_CHANW': %d (vital)"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }
      
        # Check for unexpected TRANSDIR values in Thal
      intermediateMessage('.8UexpectedTransdirT', loc='end')
      pp <- stValUnexpectedValues(subset(df, SAMPLE_TYPE=='PHAB_THALW', timing=timeThis)
                                 ,'TRANSDIR'
                                 ,c('LF','RT','CT','RC','LC', 'NONE')
                                 ,c('UID', 'TRANSECT')
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Unexpected TRANSDIR values with SAMPLE_TYPE='PHAB_THALW': %d (vital)"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }
      # Check for unexpected TRANSECT values
      intermediateMessage('.9unexpectedTransect', loc='end')
      pp <- stValUnexpectedValues (df,'TRANSECT'
                                 ,c('A','B','C','D','E','F','G','H','I','J','K', 'NONE',
                                 'XA', 'XB', 'XC', 'XD', 'XE', 'XF', 'XG', 'XH', 'XI', 'XJ', 'XK'
                                 )
                                 ,c('UID') , timing=timeThis
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

          # Check for unexpected PARAMETER values- Fronts
      intermediateMessage('.10unexpectedParameterFront', loc='end')
      tt <- subset(df, SAMPLE_TYPE=='PHAB_CHANW')
      if(nrow(tt) > 0) {
      pp <- stValUnexpectedValues(tt
                                 ,'PARAMETER'
                                 ,c('EMBED', 'SIZE_CLS', 'DEPTH', 'DIST_LB')
                                 ,c('UID', 'TRANSECT')
                                 , timing=timeThis
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
    }
  # Check for unexpected PARAMETER values- Backs
      intermediateMessage('.11unexpectedParameterBack', loc='end')
      tt <- subset(df, SAMPLE_TYPE=='PHAB_THALW', timing=timeThis)
      if(nrow(tt) > 0) {
      pp <- stValUnexpectedValues(tt
                                 ,'PARAMETER'
                                 ,c('XSIZE_CLS', 'SUB_5_7')
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
    }
           # Check for unexpected values of SAMPLE_TYPE.
      intermediateMessage('.12unexpectedSampleType')
      pp <- stValUnexpectedValues(df
                                 ,'SAMPLE_TYPE'
                                 ,c('PHAB_CHANW','PHAB_THALW')
                                 ,c('UID', 'TRANSECT')
                                 , timing=timeThis
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
      # Check for uniqueness of each UID-TRANSECT-TRANSDIR-PARAMETER
      intermediateMessage('.13uniqueUID*Transect*Transdir*Parameter', loc='end')
      pp <- stValCountRows(df, c('UID','TRANSECT', 'TRANSDIR', 'PARAMETER'), 1, timing=timeThis)
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Duplicate UID-TRANSECT-TRANSDIR-PARAMETER values: %d (vital)"
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
  
     # Check for absent TRANSDIR values at each UID-TRANSECT in Fronts
      intermediateMessage('.14Transdir@UID-TRANSECTFront', loc='end')
      tt <- subset(df, SAMPLE_TYPE=='PHAB_CHANW', timing=timeThis)
      if(nrow(tt) > 0) {
          pp <- stValAbsentValues(tt
                                 ,'TRANSDIR'
                                 ,c('LF','RT','CT','RC','LC')
                                 ,c('UID', 'TRANSECT')
                                 )
          if(test=='synopsis') {
              probs <- rbind(probs
                            ,sprintf("Absent TRANSDIR values with SAMPLE_TYPE='PHAB_CHANW': %d"
                                    ,ifelse(is.null(pp), 0, nrow(pp))
                                    )
                            )
          } else {
              probs <- rbind(probs, pp)
          }
      }

      # Check for absent TRANSDIR values at each UID-TRANSECT in Back
      intermediateMessage('.15absentTRANSDIR@UID-TRANSECTBack', loc='end')
      tt <- subset(df, SAMPLE_TYPE=='PHAB_THALW', timing=timeThis)
      if(nrow(tt) > 0) {
          pp <- stValAbsentValues(tt
                                 ,'TRANSDIR'
                                 ,c('LF','RT','CT','RC','LC', 'NONE')
                                 ,c('UID', 'TRANSECT')
                                 )
          if(test=='synopsis') {
              probs <- rbind(probs
                            ,sprintf("Absent TRANSDIR values with SAMPLE_TYPE='PHAB_THALW': %d"
                                    ,ifelse(is.null(pp), 0, nrow(pp))
                                    )
                            )
          } else {
              probs <- rbind(probs, pp)
          }
      }

  
  
      # Check for absent TRANSECT values at each UID
      intermediateMessage('.16absentTransectValues', loc='end')
      pp <- stValAbsentValues(df, 'TRANSECT'
                             ,c('A','B','C','D','E','F','G','H','I','J','K')
                             ,c('UID')
                             , timing=timeThis
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
      intermediateMessage('.17absentParameter-Fronts', loc='end')
    tt <- subset(df, SAMPLE_TYPE=='PHAB_CHANW')
      if(nrow(tt) > 0) {
      pp <- stValAbsentValues(tt
                                 ,'PARAMETER'
                                 ,c('EMBED', 'SIZE_CLS', 'DEPTH', 'DIST_LB')
                                 ,c('UID', 'TRANSECT')
                                 , timing=timeThis
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Unexpected PARAMETER-Fronts values: %d"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }
    }

    # Check for absent PARAMETER values- Backs
      intermediateMessage('.18absentParameterBack', loc='end')
      tt <- subset(df, SAMPLE_TYPE=='PHAB_THALW', timing=timeThis)
      if(nrow(tt) > 0) {
      pp <- stValAbsentValues(tt
                                 ,'PARAMETER'
                                 ,c('XSIZE_CLS', 'SUB_5_7')
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

  }


  intermediateMessage('.  Done', loc='end')
  return(probs)

}



valStructChannelCrossSectionTest <- function()
# Tests valStructChannelCrossSection()
{
  # Create test data frame
  baseTest <- rbind(expand.grid(UID=as.character(1000 + 1:10)
                               ,TRANSECT=LETTERS[1:11]
                               ,TRANSDIR=c('LF','RT','CT','RC','LC')
                               ,SAMPLE_TYPE='PHAB_CHANW'
                               ,PARAMETER=c('EMBED', 'SIZE_CLS', 'DEPTH', 'DIST_LB')
                               )
                   ,expand.grid(UID=as.character(2000 + 1:10)
                               ,TRANSECT=LETTERS[1:11]
                               ,TRANSDIR=c('LF','RT','CT','RC','LC', 'NONE')
                               ,SAMPLE_TYPE='PHAB_THALW'
                               ,PARAMETER=c('XSIZE_CLS', 'SUB_5_7')
                               )
                   )
  baseTest$UID <- as.character(baseTest$UID)
  baseTest$TRANSECT <- as.character(baseTest$TRANSECT)
  baseTest$TRANSDIR <- as.character(baseTest$TRANSDIR)
  baseTest$SAMPLE_TYPE <- as.character(baseTest$SAMPLE_TYPE)
  baseTest$PARAMETER <- as.character(baseTest$PARAMETER)
  baseTest$RESULT <- floor(runif(nrow(baseTest), 0, 18))
  baseTest$FLAG <- ''
  

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
  is.na(realTest$TRANSDIR) <- c(205,405,605)
  is.na(realTest$PARAMETER) <- c(106,206,306)

  #    unexpected values of keys
  realTest[c(08,308,508),]$TRANSECT <- 'L'
  realTest[c(109,509,609),]$TRANSDIR <- 'XX' 
  realTest[c(110,310,810),]$SAMPLE_TYPE <- 'SUB_WRONG'
  realTest[c(111,311,511),]$PARAMETER <- 'WRONG'

  

  # Test perfect dataframe to look for false positives
  rr <- valStructChannelCrossSection(baseTest, test='all')
  checkEquals(NULL, rr
             ,'Error: Detected structure problems in perfect ChannelCrossSection dataframe'
             )
             
  # Test real dataframe to look for false negatives
  rr <- valStructChannelCrossSection(realTest, test='all')
  ee <- as.matrix(rbind(
      "Column UID has 3 missing values"                                           
     ,"Column TRANSECT has 3 missing values"                                      
,"Column TRANSDIR has 3 missing values"                                      
,"Column PARAMETER has 3 missing values"                                     
,"Unexpected value TRANSDIR=(XX) at (UID,TRANSECT)=(1009,K)"                 
,"Unexpected value TRANSDIR=(NA) at (UID,TRANSECT)=(1006,J)"                 
,"Unexpected value TRANSDIR=(NA) at (UID,TRANSECT)=(1008,H)"                 
,"Unexpected value TRANSDIR=(XX) at (UID,TRANSECT)=(1002,H)"                 
,"Unexpected value TRANSDIR=(NA) at (UID,TRANSECT)=(1008,F)"                 
,"Unexpected value TRANSDIR=(XX) at (UID,TRANSECT)=(1002,G)"                 
,"Unexpected value TRANSECT=(L) at (UID)=(1008)"                             
,"Unexpected value TRANSECT=(NA) at (UID)=(1004)"                            
,"Unexpected value TRANSECT=(L) at (UID)=(1010)"                             
,"Unexpected value TRANSECT=(NA) at (UID)=(1007)"                            
,"Unexpected value TRANSECT=(L) at (UID)=(1001)"                             
,"Unexpected value TRANSECT=(NA) at (UID)=(1007)"                            
,"Unexpected value PARAMETER=(NA) at (UID,TRANSECT)=(1006,K)"                
,"Unexpected value PARAMETER=(WRONG) at (UID,TRANSECT)=(1001,A)"             
,"Unexpected value PARAMETER=(NA) at (UID,TRANSECT)=(1007,J)"                
,"Unexpected value PARAMETER=(NA) at (UID,TRANSECT)=(1008,I)"                
,"Unexpected value PARAMETER=(WRONG) at (UID,TRANSECT)=(1003,J)"             
,"Unexpected value PARAMETER=(WRONG) at (UID,TRANSECT)=(1004,H)"             
,"Unexpected value SAMPLE_TYPE=(SUB_WRONG) at (UID,TRANSECT)=(1010,K)"       
,"Unexpected value SAMPLE_TYPE=(SUB_WRONG) at (UID,TRANSECT)=(1002,J)"       
,"Unexpected value SAMPLE_TYPE=(SUB_WRONG) at (UID,TRANSECT)=(1003,E)"       
,"Absent TRANSDIR=(LF,RT,CT,RC) value at UID,TRANSECT=(1001,L) "             
,"Absent TRANSDIR=(RT,CT,RC,LC) value at UID,TRANSECT=(1002,XA) "            
,"Absent TRANSDIR=(RT,CT,RC,LC) value at UID,TRANSECT=(1002,XF) "            
,"Absent TRANSDIR=(RT,CT,RC,LC) value at UID,TRANSECT=(1004,NA) "            
,"Absent TRANSDIR=(LF,RT,CT,RC) value at UID,TRANSECT=(1005,XE) "            
,"Absent TRANSDIR=(RT,CT,LC) value at UID,TRANSECT=(1007,NA) "               
,"Absent TRANSDIR=(RT,CT,RC,LC) value at UID,TRANSECT=(1008,L) "             
,"Absent TRANSDIR=(LF,RT,RC,LC) value at UID,TRANSECT=(1010,L) "             
,"Absent TRANSDIR=(LF,RT,CT,LC) value at UID,TRANSECT=(NA,C) "               
,"Absent TRANSDIR=(LF,RT,CT,RC) value at UID,TRANSECT=(NA,G) "               
,"Absent TRANSDIR=(RT,CT,RC,LC) value at UID,TRANSECT=(NA,K) "               
,"Absent TRANSECT=(A,B,D,E,F,H,I,J) value at UID=(NA) "                      
,"Absent PARAMETER=(SIZE_CLS,DEPTH,DIST_LB) value at UID,TRANSECT=(1001,L) " 
,"Absent PARAMETER=(SIZE_CLS,DEPTH,DIST_LB) value at UID,TRANSECT=(1002,XA) "
,"Absent PARAMETER=(SIZE_CLS,DEPTH,DIST_LB) value at UID,TRANSECT=(1002,XF) "
,"Absent PARAMETER=(SIZE_CLS,DEPTH,DIST_LB) value at UID,TRANSECT=(1004,NA) "
,"Absent PARAMETER=(SIZE_CLS,DEPTH,DIST_LB) value at UID,TRANSECT=(1005,XE) "
,"Absent PARAMETER=(DEPTH,DIST_LB) value at UID,TRANSECT=(1007,NA) "         
,"Absent PARAMETER=(SIZE_CLS,DEPTH,DIST_LB) value at UID,TRANSECT=(1008,L) " 
,"Absent PARAMETER=(SIZE_CLS,DEPTH,DIST_LB) value at UID,TRANSECT=(1010,L) " 
,"Absent PARAMETER=(EMBED,DEPTH,DIST_LB) value at UID,TRANSECT=(NA,C) "      
,"Absent PARAMETER=(SIZE_CLS,DEPTH,DIST_LB) value at UID,TRANSECT=(NA,G) "   
,"Absent PARAMETER=(SIZE_CLS,DEPTH,DIST_LB) value at UID,TRANSECT=(NA,K) "   

            
                  ))
  checkEquals(ee, rr
             ,'Error: Did not correctly find structure problems in imperfect ChannelCrossSection data'
             )

}

# end of file
