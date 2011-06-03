# valStructChanCov.r
#
# 09/24/09 cws Created
#  3/25/10 cws removed source() for nlaSupport.r (now sharedSupport.r) and
#          validation.r.
#

#source('l:/Priv/CORFiles/IM/Rwork/SharedCode/validation.r')
#source('l:/Priv/CORFiles/IM/Rwork/SharedCode/nlaSupport.r')
require(RODBC)
#intermediateMessages <- TRUE

valStructChanCov <- function(df, test='all')
# Performs structure checks on the NRSA table tblCHANCOV2.  Returns NULL if
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
  probs <- stValColumnPresence(df, c('UID', 'TRANSECT', 'TRANSDIR'
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


      # Check for missing TRANSDIR values
      intermediateMessage('.4')
      pp <- stValMissingValues(df, 'TRANSDIR')
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


      # Check for unexpected TRANSDIR values in Streams
      intermediateMessage('.7')
      pp <- stValUnexpectedValues(subset(df, SAMPLE_TYPE=='PHAB_CHANW')
                                 ,'TRANSDIR'
                                 ,c('LF','RT','CD','CL','CR','CU')
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


      # Check for unexpected TRANSDIR values in Rivers
      intermediateMessage('.8')
      pp <- stValUnexpectedValues(subset(df, SAMPLE_TYPE=='PHAB_CHANB')
                                 ,'TRANSDIR'
                                 ,c('DN','LF','RT','UP')
                                 ,c('UID', 'TRANSECT')
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Unexpected TRANSDIR values with SAMPLE_TYPE='PHAB_CHANB': %d (vital)"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }


      # Check for unexpected TRANSECT values in Streams
      intermediateMessage('.9')
      pp <- stValUnexpectedValues(subset(df, SAMPLE_TYPE=='PHAB_CHANW')
                                 ,'TRANSECT'
                                 ,c('A','B','C','D','E','F','G','H','I','J','K'
                                   ,'XA','XB','XC','XD','XE','XF','XG','XH'
                                   ,'XI','XJ','XK'
                                   )
                                 ,'UID'
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Unexpected TRANSECT values with SAMPLE_TYPE='PHAB_CHANW': %d (vital)"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }


      # Check for unexpected TRANSECT values in Rivers
      intermediateMessage('.10')
      pp <- stValUnexpectedValues(subset(df, SAMPLE_TYPE=='PHAB_CHANB')
                                 ,'TRANSECT'
                                 ,c('A','B','C','D','E','F','G','H','I','J','K')
                                 ,'UID'
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Unexpected TRANSECT values with SAMPLE_TYPE='PHAB_CHANB': %d (vital)"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }


      # Check for uniqueness of each UID-TRANSECT-TRANSDIR
      intermediateMessage('.11')
      pp <- stValCountRows(df, c('UID','TRANSECT','TRANSDIR'), 1)
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Duplicate UID-TRANSECT-TRANSDIR values: %d (vital)"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }


  }
  
  # Perform nonvital tests if requested
  if(test %in% c('all','nonvital','synopsis')) {

      # Check for absent TRANSDIR values at each UID-TRANSECT in Streams
      intermediateMessage('.12')
      tt <- subset(df, SAMPLE_TYPE=='PHAB_CHANW')
      if(nrow(tt) > 0) {
          pp <- stValAbsentValues(tt
                                 ,'TRANSDIR'
                                 ,c('LF','RT','CD','CL','CR','CU')
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

      # Check for absent TRANSDIR values at each UID-TRANSECT in Rivers
      intermediateMessage('.13')
      tt <- subset(df, SAMPLE_TYPE=='PHAB_CHANB')
      if(nrow(tt) > 0) {
          pp <- stValAbsentValues(tt
                                 ,'TRANSDIR'
                                 ,c('DN','LF','RT','UP')
                                 ,c('UID', 'TRANSECT')
                                 )
          if(test=='synopsis') {
              probs <- rbind(probs
                            ,sprintf("Absent TRANSDIR values with SAMPLE_TYPE='PHAB_CHANB': %d"
                                    ,ifelse(is.null(pp), 0, nrow(pp))
                                    )
                            )
          } else {
              probs <- rbind(probs, pp)
          }
      }


      # Check for absent TRANSECT values at each UID
      intermediateMessage('.14')
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
      intermediateMessage('.15')
      pp <- stValUnexpectedValues(df
                                 ,'PARAMETER'
                                 ,c('DENSIOM')
                                 ,c('UID', 'TRANSECT', 'TRANSDIR')
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
      intermediateMessage('.16')
      pp <- stValAbsentValues(df
                                 ,'PARAMETER'
                                 ,c('DENSIOM')
                                 ,c('UID', 'TRANSECT', 'TRANSDIR')
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
      intermediateMessage('.17')
      pp <- stValUnexpectedValues(df
                                 ,'SAMPLE_TYPE'
                                 ,c('PHAB_CHANW','PHAB_CHANB')
                                 ,c('UID', 'TRANSECT', 'TRANSDIR')
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

fixChannelCover <- function(df)
# fix up the channel cover data a bit.
{
  # Get rid of trailing spaces in character columns and unfactor them
  df$TRANSECT <- trimws(as.character(df$TRANSECT))
  df$TRANSDIR <- trimws(as.character(df$TRANSDIR))
  df$PARAMETER <- trimws(as.character(df$PARAMETER))
  df$FLAG <- trimws(as.character(df$FLAG))
  df$SAMPLE_TYPE <- trimws(as.character(df$SAMPLE_TYPE))

  df$UID <- as.character(df$BATCHNO)  # Convert to standard site identifier
  
  df <- df[c('UID','TRANSECT','TRANSDIR','PARAMETER','RESULT','FLAG'
            ,'SAMPLE_TYPE'
            )
          ]

  return(df)
}


valStructChanCovTest <- function()
# Tests valStructChanCov()
{
  # Create test data frame
  baseTest <- rbind(expand.grid(UID=as.character(1000 + 1:10)
                               ,TRANSECT=LETTERS[1:11]
                               ,TRANSDIR=c('LF','RT','CU','CD','CL','CR')
                               ,SAMPLE_TYPE='PHAB_CHANW'
                               )
                   ,expand.grid(UID=as.character(2000 + 1:10)
                               ,TRANSECT=LETTERS[1:11]
                               ,TRANSDIR=c('LF','RT','UP','DN')
                               ,SAMPLE_TYPE='PHAB_CHANB'
                               )
                   )
  baseTest$UID <- as.character(baseTest$UID)
  baseTest$TRANSECT <- as.character(baseTest$TRANSECT)
  baseTest$TRANSDIR <- as.character(baseTest$TRANSDIR)
  baseTest$SAMPLE_TYPE <- as.character(baseTest$SAMPLE_TYPE)
  baseTest$PARAMETER <- 'DENSIOM'
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
  realTest[c(109,509,609),]$TRANSDIR <- 'XX'           # always unexpected
  realTest[realTest$UID=='2001' &                      # stream values in a river
           realTest$TRANSECT=='A' &
           realTest$TRANSDIR %in% c('UP','DN')
          ,
          ]$TRANSDIR <- c('CU','CD')
  realTest[c(110,310,810),]$SAMPLE_TYPE <- 'PHAB_WRONG'
  realTest[c(111,311,511),]$PARAMETER <- 'WRONG'

  #    non-unique values of combinations of UID-TRANSECT-TRANSDIR
  realTest[c(113,413,713),]$TRANSECT <- ifelse(realTest[c(113,413,713),]$TRANSECT=='G'
                                              ,'H'
                                              ,'G'
                                              )
  

  # Test perfect dataframe to look for false positives
  rr <- valStructChanCov(baseTest, test='all')
  checkEquals(NULL, rr
             ,'Error: Detected structure problems in perfect Channel cover dataframe'
             )
             
  # Test real datafram to look for false negatives
  rr <- valStructChanCov(realTest, test='all')
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
                 ,"Unexpected value TRANSDIR=(CU) at (UID,TRANSECT)=(2001,A)"
                 ,"Unexpected value TRANSDIR=(CD) at (UID,TRANSECT)=(2001,A)"
                 ,"Unexpected value TRANSECT=(L) at (UID)=(1008)"
                 ,"Unexpected value TRANSECT=(NA) at (UID)=(1004)"
                 ,"Unexpected value TRANSECT=(L) at (UID)=(1010)"
                 ,"Unexpected value TRANSECT=(NA) at (UID)=(1007)"
                 ,"Unexpected value TRANSECT=(L) at (UID)=(1001)"
                 ,"Unexpected value TRANSECT=(NA) at (UID)=(1007)"
                 ,"Unexpected row count at (UID,TRANSECT,TRANSDIR)=(1006,G,CD), n=2"
                 ,"Unexpected row count at (UID,TRANSECT,TRANSDIR)=(2006,G,LF), n=2"
                 ,"Unexpected row count at (UID,TRANSECT,TRANSDIR)=(1003,G,RT), n=2"
                 ,"Absent TRANSDIR=(CD,CL) value at UID,TRANSECT=(1001,H) "
                 ,"Absent TRANSDIR=(CU) value at UID,TRANSECT=(1001,I) "
                 ,"Absent TRANSDIR=(RT) value at UID,TRANSECT=(1001,J) "
                 ,"Absent TRANSDIR=(LF,RT,CD,CR,CU) value at UID,TRANSECT=(1001,L) "
                 ,"Absent TRANSDIR=(LF) value at UID,TRANSECT=(1002,A) "
                 ,"Absent TRANSDIR=(CR) value at UID,TRANSECT=(1002,G) "
                 ,"Absent TRANSDIR=(CL) value at UID,TRANSECT=(1002,H) "
                 ,"Absent TRANSDIR=(CU) value at UID,TRANSECT=(1002,J) "
                 ,"Absent TRANSDIR=(LF) value at UID,TRANSECT=(1002,K) "
                 ,"Absent TRANSDIR=(RT,CD,CL,CR,CU) value at UID,TRANSECT=(1002,XA) "
                 ,"Absent TRANSDIR=(RT,CD,CL,CR,CU) value at UID,TRANSECT=(1002,XF) "
                 ,"Absent TRANSDIR=(RT) value at UID,TRANSECT=(1003,A) "
                 ,"Absent TRANSDIR=(LF) value at UID,TRANSECT=(1003,K) "
                 ,"Absent TRANSDIR=(LF) value at UID,TRANSECT=(1004,K) "
                 ,"Absent TRANSDIR=(RT,CD,CL,CR,CU) value at UID,TRANSECT=(1004,NA) "
                 ,"Absent TRANSDIR=(CL) value at UID,TRANSECT=(1005,G) "
                 ,"Absent TRANSDIR=(LF,RT,CD,CR,CU) value at UID,TRANSECT=(1005,XE) "
                 ,"Absent TRANSDIR=(CL) value at UID,TRANSECT=(1006,G) "
                 ,"Absent TRANSDIR=(CD) value at UID,TRANSECT=(1006,I) "
                 ,"Absent TRANSDIR=(RT) value at UID,TRANSECT=(1006,J) "
                 ,"Absent TRANSDIR=(CR) value at UID,TRANSECT=(1007,F) "
                 ,"Absent TRANSDIR=(CD) value at UID,TRANSECT=(1007,H) "
                 ,"Absent TRANSDIR=(LF,RT,CL,CU) value at UID,TRANSECT=(1007,NA) "
                 ,"Absent TRANSDIR=(LF) value at UID,TRANSECT=(1008,A) "
                 ,"Absent TRANSDIR=(CR) value at UID,TRANSECT=(1008,F) "
                 ,"Absent TRANSDIR=(CD) value at UID,TRANSECT=(1008,H) "
                 ,"Absent TRANSDIR=(RT,CD,CL,CR,CU) value at UID,TRANSECT=(1008,L) "
                 ,"Absent TRANSDIR=(LF) value at UID,TRANSECT=(1009,K) "
                 ,"Absent TRANSDIR=(CU) value at UID,TRANSECT=(1010,I) "
                 ,"Absent TRANSDIR=(LF) value at UID,TRANSECT=(1010,K) "
                 ,"Absent TRANSDIR=(LF,RT,CD,CL,CR) value at UID,TRANSECT=(1010,L) "
                 ,"Absent TRANSDIR=(LF,RT,CD,CR,CU) value at UID,TRANSECT=(NA,G) "
                 ,"Absent TRANSDIR=(RT,CD,CL,CR,CU) value at UID,TRANSECT=(NA,K) "
                 ,"Absent TRANSDIR=(DN,UP) value at UID,TRANSECT=(2001,A) "
                 ,"Absent TRANSDIR=(RT) value at UID,TRANSECT=(2003,E) "
                 ,"Absent TRANSDIR=(UP) value at UID,TRANSECT=(2006,C) "
                 ,"Absent TRANSDIR=(LF) value at UID,TRANSECT=(2006,F) "
                 ,"Absent TRANSDIR=(DN,LF,RT) value at UID,TRANSECT=(NA,C) "
                 ,"Absent TRANSECT=(A,B,D,E,F,H,I,J) value at UID=(NA) "
                 ,"Unexpected value PARAMETER=(NA) at (UID,TRANSECT,TRANSDIR)=(1006,K,LF)"
                 ,"Unexpected value PARAMETER=(WRONG) at (UID,TRANSECT,TRANSDIR)=(1001,A,RT)"
                 ,"Unexpected value PARAMETER=(NA) at (UID,TRANSECT,TRANSDIR)=(1007,J,RT)"
                 ,"Unexpected value PARAMETER=(NA) at (UID,TRANSECT,TRANSDIR)=(1008,I,CU)"
                 ,"Unexpected value PARAMETER=(WRONG) at (UID,TRANSECT,TRANSDIR)=(1003,J,CU)"
                 ,"Unexpected value PARAMETER=(WRONG) at (UID,TRANSECT,TRANSDIR)=(1004,H,CL)"
                 ,"Absent PARAMETER=(DENSIOM) value at UID,TRANSECT,TRANSDIR=(1001,A,RT) "
                 ,"Absent PARAMETER=(DENSIOM) value at UID,TRANSECT,TRANSDIR=(1003,J,CU) "
                 ,"Absent PARAMETER=(DENSIOM) value at UID,TRANSECT,TRANSDIR=(1004,H,CL) "
                 ,"Absent PARAMETER=(DENSIOM) value at UID,TRANSECT,TRANSDIR=(1006,K,LF) "
                 ,"Absent PARAMETER=(DENSIOM) value at UID,TRANSECT,TRANSDIR=(1007,J,RT) "
                 ,"Absent PARAMETER=(DENSIOM) value at UID,TRANSECT,TRANSDIR=(1008,I,CU) "
                 ,"Unexpected value SAMPLE_TYPE=(PHAB_WRONG) at (UID,TRANSECT,TRANSDIR)=(1010,K,LF)"
                 ,"Unexpected value SAMPLE_TYPE=(PHAB_WRONG) at (UID,TRANSECT,TRANSDIR)=(1002,J,CU)"
                 ,"Unexpected value SAMPLE_TYPE=(PHAB_WRONG) at (UID,TRANSECT,TRANSDIR)=(2003,E,RT)"
                 ))
  checkEquals(ee, rr
             ,'Error: Did not correctly find structure problems in imperfect Channel cover data'
             )

}

# end of file