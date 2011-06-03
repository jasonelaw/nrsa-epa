# valStructBankGeometry.r
#
# 09/24/09 cws Created valStructChancov
# 10/02/09  SSR adapted for tblBANKGEOMETRY2
# 10/21/09 cws Added timing argument to structure check function calls.
# 11/24/09 cws Modified check and unit test to use NONE instead of X for
#          'unused' transdir values
# 12/23/09 cws updated unit test, removing TRANSDIR values CT, RC, LC, and
#          changing records based on key values rather than numeric row names.
#  2/25/10 cws moved source() calls to NRSAvalidation.r
#
# 05/28/10 ssr added check for non-numeric values
#

# Contains functions valStructBankGeometry 

require(RODBC)
#intermediateMessages <- TRUE

valStructBankGeometry <- function(df, test='all')
# Performs structure checks on the NRSA table tblBANKGEOMETRY2.  Returns NULL if
# no errors have been found, or an Nx1 character matrix describing the detected
# errors.
#
# ARGUMENTS:
# df        dataframe of NRSA bank geometry data
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
  intermediateMessage('Structure validation of bank geometry data ', loc='start')
  
  timeThis <- FALSE
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
                                    ,'SAMPLE_TYPE','UNITS'
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

      # Check for missing UID values
      intermediateMessage('.2')
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
      intermediateMessage('.3')
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
      intermediateMessage('.4')
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
      intermediateMessage('.5')
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
      intermediateMessage('.6')
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


      # Check for unexpected TRANSDIR values in Streams
      intermediateMessage('.7')
      pp <- stValUnexpectedValues(subset(df, SAMPLE_TYPE=='PHAB_CHANW')
                                 ,'TRANSDIR'
                                 ,c('LF', 'RT', 'NONE')
                                 ,c('UID', 'TRANSECT')
                                 , timing=timeThis
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Unexpected TRANSDIR values (streams): %d (vital)"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }


      # Check for unexpected TRANSDIR values in Rivers
      intermediateMessage('.8')
      pp <- stValUnexpectedValues(subset(df, SAMPLE_TYPE=='PHAB_CHANB' | SAMPLE_TYPE=='PHAB_CHANBFRONT')
                                 ,'TRANSDIR'
                                 ,c('NONE')
                                 ,c('UID', 'TRANSECT')
                                 , timing=timeThis
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Unexpected TRANSDIR values (rivers): %d (vital)"
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
                                 , timing=timeThis
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Unexpected TRANSECT values (streams): %d (vital)"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }


      # Check for unexpected TRANSECT values in Rivers
      intermediateMessage('.10')
      pp <- stValUnexpectedValues(subset(df, SAMPLE_TYPE=='PHAB_CHANB' | SAMPLE_TYPE=='PHAB_CHANBFRONT')
                                 ,'TRANSECT'
                                 ,c('A','B','C','D','E','F','G','H','I','J','K'
                                   ,'XA','XB','XC','XD','XE','XF','XG','XH'
                                   ,'XI','XJ','XK'
                                   )
                                 ,'UID'
                                 , timing=timeThis
                                 )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Unexpected TRANSECT values (rivers): %d (vital)"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }


      # Check for uniqueness of each UID-TRANSECT-TRANSDIR-PARAMETER
      intermediateMessage('.11')
      pp <- stValCountRows(df, c('UID','TRANSECT','TRANSDIR','PARAMETER'), 1, timing=timeThis)
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Duplicate UID-TRANSECT-TRANSDIR-PARAMETER values: %d (vital)"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }

      #  Check that numeric fields contain numeric data
      intermediateMessage('.11a')
      pp <- stValNonNumericValues(subset(df, PARAMETER %in% c('ANGLE','UNDERCUT'
                                      ,'BANKHGT','BANKWID','BARWID','INCISHGT'
                                      ,'WETWID','BANKHT','INCISED'))
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

      # Check for absent TRANSDIR values at each UID-TRANSECT in Streams
      intermediateMessage('.12')
      pp <- stValAbsentValues(subset(df, SAMPLE_TYPE=='PHAB_CHANW')
                             ,'TRANSDIR'
                             ,c('LF', 'RT', 'NONE')
                             ,c('UID', 'TRANSECT')
                             , timing=timeThis
                             )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Absent TRANSDIR values (streams): %d"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }


      # Check for absent TRANSDIR values at each UID-TRANSECT in Rivers
      intermediateMessage('.13')
      pp <- stValAbsentValues(subset(df, SAMPLE_TYPE=='PHAB_CHANB' | SAMPLE_TYPE=='PHAB_CHANBFRONT')
                             ,'TRANSDIR'
                             ,c('NONE')
                             ,c('UID', 'TRANSECT')
                             , timing=timeThis
                             )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Absent TRANSDIR values (rivers): %d"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }



      # Check for absent TRANSECT values at each UID
      intermediateMessage('.14')
      pp <- stValAbsentValues(df, 'TRANSECT'
                             ,c('A','B','C','D','E','F','G','H','I','J','K')
                             ,'UID'
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

      # Check for unexpected PARAMETER values
      intermediateMessage('.15')
      pp <- stValUnexpectedValues(df
                                 ,'PARAMETER'
                                 ,c('ANGLE', 'UNDERCUT', 'BANKHGT', 'BANKWID',
                                    'BARWID', 'INCISHGT', 'WETWID', 'BANKHT',
                                    'INCISED', 'CONSTRT', 'SEEOVRBK', 'SHOR2RIP')
                                 ,c('UID', 'TRANSECT', 'TRANSDIR')
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

      # Check for absent PARAMETER values in streams
      intermediateMessage('.16')
      pp <- stValAbsentValues(subset(df, SAMPLE_TYPE == 'PHAB_CHANW' & TRANSDIR == 'NONE')
                             ,'PARAMETER'
                             ,c('BANKHGT', 'BANKWID',
                                'BARWID', 'INCISHGT', 'WETWID'
                               )
                             ,c('UID', 'TRANSECT')
                             , timing=timeThis
                             )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Absent PARAMETER values (streams): %d"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }

      # Check for absent PARAMETER values in streams
      intermediateMessage('.16a')
      pp <- stValAbsentValues(subset(df, SAMPLE_TYPE == 'PHAB_CHANW' & TRANSDIR != 'NONE')
                             ,'PARAMETER'
                             ,c('ANGLE', 'UNDERCUT')
                             ,c('UID', 'TRANSECT', 'TRANSDIR')
                             , timing=timeThis
                             )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Absent PARAMETER values (streams): %d"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }

      # Check for absent PARAMETER values in rivers
      intermediateMessage('.17')
      pp <- stValAbsentValues(subset(df, SAMPLE_TYPE =='PHAB_CHANBFRONT')
                             ,'PARAMETER'
                             ,c('ANGLE', 'BANKWID',
                                'BARWID', 'BANKHT',
                                'INCISED')
                             ,c('UID', 'TRANSECT')
                             , timing=timeThis
                             )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Absent PARAMETER values (rivers): %d"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }

      # Check for absent PARAMETER values in rivers
      intermediateMessage('.17a')
      pp <- stValAbsentValues(subset(df, SAMPLE_TYPE =='PHAB_CHANB')
                             ,'PARAMETER'
                             ,c('CONSTRT', 'SEEOVRBK', 'SHOR2RIP')
                             ,c('UID', 'TRANSECT')
                             , timing=timeThis
                             )
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,sprintf("Absent PARAMETER values (rivers): %d"
                                ,ifelse(is.null(pp), 0, nrow(pp))
                                )
                        )
      } else {
          probs <- rbind(probs, pp)
      }

      # Check for unexpected values of SAMPLE_TYPE.
      intermediateMessage('.18')
      pp <- stValUnexpectedValues(df
                                 ,'SAMPLE_TYPE'
                                 ,c('PHAB_CHANW','PHAB_CHANB', 'PHAB_CHANBFRONT')
                                 ,c('UID', 'TRANSECT', 'TRANSDIR')
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


  }

  intermediateMessage('.  Done', loc='end')
  return(probs)

}




valStructBankGeometryTest <- function()
# Tests valStructBankGeometry()
{
  # Create test data frame
  baseTest <- rbind(expand.grid(UID=as.character(1000 + 1:10)
                               ,TRANSECT=LETTERS[1:11]
                               ,TRANSDIR=c('LF','RT')
                               ,SAMPLE_TYPE='PHAB_CHANW'
                               ,PARAMETER=c('ANGLE','UNDERCUT')
                               )
                   ,expand.grid(UID=as.character(1000 + 1:10)
                               ,TRANSECT=LETTERS[1:11]
                               ,TRANSDIR='NONE'
                               ,SAMPLE_TYPE='PHAB_CHANW'
                               ,PARAMETER=c('BANKHGT','BANKWID',
                                    'BARWID','INCISHGT','WETWID')
                               )
                   ,expand.grid(UID=as.character(2000 + 1:10)
                               ,TRANSECT=LETTERS[1:11]
                               ,TRANSDIR=c('NONE')
                               ,SAMPLE_TYPE='PHAB_CHANB'
                               ,PARAMETER=c('CONSTRT','SEEOVRBK','SHOR2RIP')
                               )
                   ,expand.grid(UID=as.character(2000 + 1:10)
                               ,TRANSECT=LETTERS[1:11]
                               ,TRANSDIR='NONE'
                               ,SAMPLE_TYPE='PHAB_CHANBFRONT'
                               ,PARAMETER=c('ANGLE','BANKHT','BANKWID',
                                    'BARWID','INCISED','WETWID')
                                )
                     )
  baseTest$UID <- as.character(baseTest$UID)
  baseTest$TRANSECT <- as.character(baseTest$TRANSECT)
  baseTest$TRANSDIR <- as.character(baseTest$TRANSDIR)
  baseTest$PARAMETER <- as.character(baseTest$PARAMETER)
  baseTest$SAMPLE_TYPE <- as.character(baseTest$SAMPLE_TYPE)
  baseTest$RESULT <- floor(runif(nrow(baseTest), 0, 18))
  baseTest$UNITS <- 'NONE'
  baseTest$FLAG <- ''


  # Create imperfect/real dataframe to check for false negatives
  realTest <- baseTest

  #    make key values missing
  realTest[realTest$UID=='1001' & realTest$TRANSECT=='B' &
           realTest$TRANSDIR=='LF' & realTest$PARAMETER=='ANGLE'
          ,]$UID <- NA
  realTest[realTest$UID=='1001' & realTest$TRANSECT=='B' &
           realTest$TRANSDIR=='RT' & realTest$PARAMETER=='ANGLE'
          ,]$TRANSECT <- NA
  realTest[realTest$UID=='1002' & realTest$TRANSECT=='C' &
           realTest$TRANSDIR=='RT' & realTest$PARAMETER=='ANGLE'
          ,]$TRANSDIR <- NA
  realTest[realTest$UID=='1002' & realTest$TRANSECT=='D' &
           realTest$TRANSDIR=='RT' & realTest$PARAMETER=='ANGLE'
          ,]$PARAMETER <- NA
  realTest[realTest$UID=='1002' & realTest$TRANSECT=='E' &
           realTest$TRANSDIR=='RT' & realTest$PARAMETER=='ANGLE'
          ,]$SAMPLE_TYPE <- NA

  #    Create unexpected values of keys
  realTest[realTest$UID=='1003' & realTest$TRANSECT=='A' &
           realTest$TRANSDIR=='NONE' & realTest$PARAMETER=='BANKHGT'
          ,]$TRANSECT <- 'L'
  realTest[realTest$UID=='1003' & realTest$TRANSECT=='B' &
           realTest$TRANSDIR=='NONE' & realTest$PARAMETER=='BANKHGT'
          ,]$TRANSDIR <- 'AB'

          
  realTest[realTest$UID=='2009' &                     # stream values in a river
           realTest$TRANSECT=='A' &
           realTest$TRANSDIR %in% c('NONE')
          ,
          ]$TRANSDIR <- 'RT'
          
  realTest[realTest$UID=='1009' &  !is.na(realTest$UID) & # river values in a stream
           realTest$TRANSECT=='B' &
           realTest$TRANSDIR %in% c('LF')
          ,
          ]$TRANSDIR <- 'NONE'
          
  realTest[realTest$UID=='1004' & realTest$TRANSECT=='C' &
           realTest$TRANSDIR=='RT' & realTest$PARAMETER=='UNDERCUT'
          ,]$SAMPLE_TYPE <- 'PHAB_WRONG'
  realTest[realTest$UID=='1004' & realTest$TRANSECT=='D' &
           realTest$TRANSDIR=='NONE' & realTest$PARAMETER=='BANKWID'
          ,]$PARAMETER <- 'WRONG'

  #    Create non-unique values of combinations of UID-TRANSECT-TRANSDIR-PARAMETER
  realTest$TRANSECT <- ifelse(realTest$UID==2008 & realTest$TRANSECT=='G' &
                              realTest$TRANSDIR=='NONE' & realTest$PARAMETER=='CONSTRT'
                             ,'J'
                             ,realTest$TRANSECT
                             )
  
  #    Create absent transects by removing them from dataframe
  realTest <- subset(realTest
                    ,!(UID=='1004' & TRANSECT=='A' & TRANSDIR=='RT' &
                       PARAMETER=='ANGLE'
                      )
                    )

  #    Create absent transects by substituting them with legal values
  realTest[realTest$UID=='1004' & realTest$TRANSECT=='B' &
           realTest$TRANSDIR=='NONE' & realTest$PARAMETER=='BANKWID'
          ,]$TRANSECT <- 'XB'

  #  Create absent TRANSDIR by removing them from the dataframe
  realTest <- subset(realTest, !(UID==1005 & TRANSECT=='F' & TRANSDIR=='RT'))
  realTest <- subset(realTest, !(UID==1005 & TRANSECT=='H' & TRANSDIR=='NONE'))
  realTest <- subset(realTest, !(UID==2005 & TRANSECT=='K' & TRANSDIR=='NONE'))
  realTest <- subset(realTest, !(UID==2005 & TRANSECT=='I' & TRANSDIR=='NONE'))

  #  Create non-numeric data
  realTest$RESULT <- ifelse(realTest$UID==1001 & realTest$TRANSECT=='A' 
                      & realTest$TRANSDIR=='LF' & realTest$PARAMETER=='ANGLE'
                      , '.', realTest$RESULT)
 
  # Test perfect dataframe to look for false positives
  rr <- valStructBankGeometry(baseTest, test='all')
  checkEquals(NULL, rr
             ,'Error: Detected structure problems in perfect bank geometry dataframe'
             )
             
  # Test real dataframe to look for false negatives
  rr <- valStructBankGeometry(realTest, test='all')
  ee <- as.matrix(rbind(
      "Column UID has 1 missing values"
     ,"Column TRANSECT has 1 missing values"
     ,"Column TRANSDIR has 1 missing values"
     ,"Column PARAMETER has 1 missing values"
     ,"Column SAMPLE_TYPE has 1 missing values"
     ,"Unexpected value TRANSDIR=(NA) at (UID,TRANSECT)=(1002,C)"
     ,"Unexpected value TRANSDIR=(AB) at (UID,TRANSECT)=(1003,B)"
     ,"Unexpected value TRANSDIR=(RT) at (UID,TRANSECT)=(2009,A)"
     ,"Unexpected value TRANSDIR=(RT) at (UID,TRANSECT)=(2009,A)"
     ,"Unexpected value TRANSDIR=(RT) at (UID,TRANSECT)=(2009,A)"
     ,"Unexpected value TRANSDIR=(RT) at (UID,TRANSECT)=(2009,A)"
     ,"Unexpected value TRANSDIR=(RT) at (UID,TRANSECT)=(2009,A)"
     ,"Unexpected value TRANSDIR=(RT) at (UID,TRANSECT)=(2009,A)"
     ,"Unexpected value TRANSDIR=(RT) at (UID,TRANSECT)=(2009,A)"
     ,"Unexpected value TRANSDIR=(RT) at (UID,TRANSECT)=(2009,A)"
     ,"Unexpected value TRANSDIR=(RT) at (UID,TRANSECT)=(2009,A)"
     ,"Unexpected value TRANSECT=(NA) at (UID)=(1001)"
     ,"Unexpected value TRANSECT=(L) at (UID)=(1003)"
     ,"Unexpected row count at (UID,TRANSECT,TRANSDIR,PARAMETER)=(2008,J,NONE,CONSTRT), n=2"
     ,"Column RESULT has 1 non-numeric values"
     ,"Absent TRANSDIR=(LF,NONE) value at UID,TRANSECT=(1001,NA) "
     ,"Absent TRANSDIR=(LF,RT) value at UID,TRANSECT=(1003,L) "
     ,"Absent TRANSDIR=(LF,RT) value at UID,TRANSECT=(1004,XB) "
     ,"Absent TRANSDIR=(RT) value at UID,TRANSECT=(1005,F) "
     ,"Absent TRANSDIR=(NONE) value at UID,TRANSECT=(1005,H) "
     ,"Absent TRANSDIR=(LF) value at UID,TRANSECT=(1009,B) "
     ,"Absent TRANSDIR=(RT,NONE) value at UID,TRANSECT=(NA,B) "
     ,"Absent TRANSDIR=(NONE) value at UID,TRANSECT=(2009,A) "
     ,"Absent TRANSECT=(I,K) value at UID=(2005) "
     ,"Absent TRANSECT=(A,C,D,E,F,G,H,I,J,K) value at UID=(NA) "
     ,"Unexpected value PARAMETER=(NA) at (UID,TRANSECT,TRANSDIR)=(1002,D,RT)"
     ,"Unexpected value PARAMETER=(WRONG) at (UID,TRANSECT,TRANSDIR)=(1004,D,NONE)"
     ,"Absent PARAMETER=(BANKHGT) value at UID,TRANSECT=(1003,A) "
     ,"Absent PARAMETER=(BANKHGT) value at UID,TRANSECT=(1003,B) "
     ,"Absent PARAMETER=(BANKWID,BARWID,INCISHGT,WETWID) value at UID,TRANSECT=(1003,L) "
     ,"Absent PARAMETER=(BANKWID) value at UID,TRANSECT=(1004,B) "
     ,"Absent PARAMETER=(BANKWID) value at UID,TRANSECT=(1004,D) "
     ,"Absent PARAMETER=(BANKHGT,BARWID,INCISHGT,WETWID) value at UID,TRANSECT=(1004,XB) "
     ,"Absent PARAMETER=(ANGLE) value at UID,TRANSECT,TRANSDIR=(1001,B,LF) "
     ,"Absent PARAMETER=(ANGLE) value at UID,TRANSECT,TRANSDIR=(1001,B,RT) "
     ,"Absent PARAMETER=(ANGLE) value at UID,TRANSECT,TRANSDIR=(1002,C,RT) "
     ,"Absent PARAMETER=(ANGLE) value at UID,TRANSECT,TRANSDIR=(1002,D,RT) "
     ,"Absent PARAMETER=(ANGLE) value at UID,TRANSECT,TRANSDIR=(1002,E,RT) "
     ,"Absent PARAMETER=(ANGLE,UNDERCUT) value at UID,TRANSECT,TRANSDIR=(1003,B,AB) "
     ,"Absent PARAMETER=(ANGLE) value at UID,TRANSECT,TRANSDIR=(1004,A,RT) "
     ,"Absent PARAMETER=(UNDERCUT) value at UID,TRANSECT,TRANSDIR=(1004,C,RT) "
     ,"Absent PARAMETER=(UNDERCUT) value at UID,TRANSECT,TRANSDIR=(NA,B,LF) "
     ,"Absent PARAMETER=(CONSTRT) value at UID,TRANSECT=(2008,G) "
     ,"Unexpected value SAMPLE_TYPE=(NA) at (UID,TRANSECT,TRANSDIR)=(1002,E,RT)"
     ,"Unexpected value SAMPLE_TYPE=(PHAB_WRONG) at (UID,TRANSECT,TRANSDIR)=(1004,C,RT)"
     ))
  checkEquals(ee, rr
             ,'Error: Did not correctly find structure problems in imperfect Channel cover data'
             )

}

# end of file