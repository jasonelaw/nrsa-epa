# constructNRSAValidationResults.r
#
# 10/30/09 cws Created
# 11/10/09 cws Testing for presence of UNITS column instead of hardwiring its
#          occurence in the output.  Removing imageBasePath argument pointing to
#          field form images as it only worked for single-year studies.  Removed
#          visual spacing in output as it's excessively slow.  Updated unit
#          tests accordingly.
# 11/24/09 cws Writing FLAG column if present in data.
# 12/07/09 cws No longer using UNITS when merging metadata with form
#          information -- the PARAMETER-SAMPLE_TYPE pair is sufficient, and
#          the merge obviously would not work when checking for illegal units.
# 12/08/09 cws ChRip form files for 2009 now include _P1, _P2 to account for
#          how the images were exported.  Unit test updated to test these last
#          two changes.
# 12/23/09 cws When creating ffName, DATE_COL values are explicitly cast to
#          class POSIXct before format() is used to extract the year.  This
#          seems to be required under v2.10.
#  3/31/10 cws Comment description rewritten. Added optional argument ssFmt
#          to specify the spreadsheet that will be used for reviewing these
#          results.  Currently this affects how the HYPERLINK() arguments are
#          written.  Modified unit test accordingly.
#  4/27/10 cws Adding SAMPLE_TYPE PHAB_CHANBFRONT as requiring a page number
#          in the ChRip form image filenames for year >2008.
#  5/18/10 ssr Added UID to list of columns exported.
#  6/11/10 cws Modified unit test to expect UID column in return value
#
require(RUnit)

constructNRSAValidationResults <- function (df, formMetadata, siteInfo, ssFmt='EXCEL')
# Organizes the output dataframe of a validation test in the format needed for
# the review process.  Typically, the output of this function is written to a
# spreadsheet file which is edited and then read back in to make adjustments to
# a specific table in the database.
#                                                                                    1
# Columns with hypertext link to relevant field form image for each parameter,
# and space for user comments are added, and UID is converted to SITE_ID,
# VISIT_NO and DATE_COL.  These values are used by the EPA to identify a
# specific visit to a site, and may not be meaningful to those outside the
# agency.  In addition to being used in creation of the hyperlink to the field
# form image, the SITE_ID, VISIT_NO and DATE_COL values are used instead of UID
# to insure the correct updating of the database even after it has been
# reconstructed and the former UID values may not relate to the same site visit.
#
# Returns constructed dataframe with the following columns: SITE_ID, VISIT_NO,
# DATE_COL, TRANSECT, PARAMETER, TESTDESCRIPTION, RESULT, UNITS (if present
# in df), FORMIMAGE, COMMENTS.
#
# POSSIBLE FUTURE EXTENSIONS:
# + Handling fish related form images, which have _P1, _P2, etc. at end of
#   the file name.  There is also the Lower Missisippi form which may have these
#   extensions.
#
# ARGUMENTS:
# df            data frame of validation results.  Is expected to have the
#                 columns expected for NRSA validation results, and will
#                 specifically use UID, TRANSECT, PARAMETER, TESTDESCRIPTION,
#                 RESULT, UNITS (if present) and TESTDESCRIPTION.
# formMetadata  dataframe with information relating each parameter to a field
#                 form, with columns PARAMETER and FORMABBR holding the
#                 parameter name and the abbreviated name of the form on which
#                 it is recorded and which will appear in the validation results
#                 file being created e.g. a link to the Thalweg Profile and
#                 Woody Debris form will show up as simply Thal.
# siteInfo      dataframe with information relating the UID of a table to the
#                 relevant site information.  Contains columns UID, SITE_ID,
#                 and VISIT_NO.
# ssFmt         Specifies the spreadsheet that will be used for reviewing these
#                 results.  Currently this affects how the HYPERLINK() arguments
#                 are written.  Value is either 'EXCEL' or 'OO' (for Open Office).
#
# ASSUMPTIONS:
# Image file names are of the form: NRSA_<FormAbbr>_<SiteID>_V<Visit_no>.tif or
#   NRSA_<FormAbbr>_<SiteID>_V<Visit_no>_<Transect>.tif for those forms (PHab)
#   which are filled out for every station.
# Image files are stored in L:/Apps/Scantron/Images/<year>/Flowing Waters/,
#   where <year> is the year of the sample, as determined by DATE_COL in the
#   data in siteInfo dataframe.
# Validation results in df has columns SAMPLE_TYPE, PARAMETER
#
{
  intermediateMessage('Construction of validation results', loc='start')

  ############################
  # Create hyperlinks to relevant field form image files.
  #
  # Construct the field form image name (ffName) on which each PARAMETER was
  # recorded.  This requires metadata relating PARAMETER to the field form
  # name, and UID to the site name and visit number information.
  # NOTE: SAMPLE_TYPE column in siteInfo table has entirely different
  # meaning than in other tables, and is not used here.
  byVars <- c('SAMPLE_TYPE', 'PARAMETER')
  metadataCols <- c('PARAMETER','FORMABBR','SAMPLE_TYPE')
  formMetadata<-formMetadata[!duplicated(formMetadata[c('PARAMETER','SAMPLE_TYPE')]),]
  df <- merge(df, formMetadata[metadataCols]
             ,by=byVars, all.x=TRUE, all.y=FALSE
             )
  intermediateMessage('.1')

  df <- merge(df, siteInfo[c('UID','SITE_ID','VISIT_NO','DATE_COL')]
             ,by='UID', all.x=TRUE, all.y=FALSE)
  intermediateMessage('.2')

  parametersChRip_p2 <- c("CANBTRE", "CANSTRE", "CANVEG"
                         ,"UNDERVEG", "UNDNWDY", "UNDWDY"
                         ,"GCNWDY", "GCWDY", "BARE"
                         ,"WALL", "BUILD", "PAVE", "ROAD", "PIPES", "LANDFL"
                         ,"PARK", "ROW", "PAST", "LOG", "MINE"
                         ,"ALGAE", "MACPHY", "WOODY", "BRISH", "LVTREE"
                         ,"OVRHNG", "UNDCUT", "BOULDR", "STRUCT"
                         ,"SHOR2RIP", "CONSTRT", "SEEOVRBK"
                         ,"DENSIOM"
                         )
  df$ffName <- ifelse(df$FORMABBR %in% c('ChRip', 'Thal')
                     # The value of TRANSECT is a part of these form file names
                     ,ifelse(df$FORMABBR=='ChRip' &
                             format(as.POSIXct(df$DATE_COL), '%Y')> 2008 &
                             df$SAMPLE_TYPE %in% c('PHAB_CHANB','PHAB_CHANBFRONT')
                            # Both sides of the boatable form of ChRip were
                            # exported as a single TIFF in 2008, but as separate
                            # files in 2009 because of scanner troubles.  Use
                            # PARAMETER to determine which file to hyperlink to.
                           ,paste('NRSA'
                                 ,df$FORMABBR
                                 ,df$SITE_ID
                                 ,paste('V', df$VISIT_NO, sep='')
                                 ,df$TRANSECT
                                 ,ifelse(df$PARAMETER %in% parametersChRip_p2
                                        ,'P2'
                                        ,'P1'
                                        )
                                 ,sep='_'
                                 )
                           # Otherwise just include transect without any page num.
                           ,paste('NRSA'
                                 ,df$FORMABBR
                                 ,df$SITE_ID
                                 ,paste('V', df$VISIT_NO, sep='')
                                 ,df$TRANSECT
                                 ,sep='_'
                                 )
                           )
                     # Other forms do not contain transect in their file names
                     ,paste('NRSA'
                           ,df$FORMABBR
                           ,df$SITE_ID
                           ,paste('V', df$VISIT_NO, sep='')
                           ,sep='_'
                           )
                     )

  df$ffName <- paste('L:/Apps/Scantron/Images/'
                    ,format(as.POSIXct(df$DATE_COL), '%Y')
                    ,'/Flowing Waters/'
                    ,df$ffName
                    ,'.tif'
                    , sep=''
                    )


  # Write field form file name as URL.  This is spreadsheet dependent.
  #   Microsoft Excel: =HYPERLINK("file:/path/fname.tif", "optional label")
  #                    Note there can be 1-3 slashes after the 'file:', but
  #                    double-quotes are required.
  #   OpenOffice Calc: =HYPERLINK('file:///path/fname.tif'; 'optional label')
  #                    Note that 1 or 3 slashes are required after the file:
  #                    and the semicolon is required to delimit the arguments,
  #                    as is the use of double-quotes.
  #   Gnu Gnumeric:    =HYPERLINK() does not work, may be fixed soon. feh.
  if(ssFmt=='OO') {
      df$FORMIMAGE <- paste('=HYPERLINK("file:///'
                           ,df$ffName, '" ; "', df$FORMABBR
                           ,'")'
                           ,sep=''
                           )
  } else if(ssFmt=='EXCEL') {
      df$FORMIMAGE <- paste('=HYPERLINK("file:///'
                           ,df$ffName, '" , "', df$FORMABBR
                           ,'")'
                           ,sep=''
                           )
  }
  intermediateMessage('.3')


  ############################
  # Prepare validation test results for review.
  #
  # Determine table keys used in this particular table.
  tableKeys <- names(df)[names(df) %in% NRSAKeyColumns & names(df) != 'UID']


  # Order resulting dataframe by form name at each site (and any intrasite keys)
  ordering <- eval(parse(text=sprintf('order(%s)'
                                     ,paste('df$'
                                           ,c('SITE_ID','VISIT_NO','DATE_COL'
                                             ,tableKeys
                                             ,'TESTDESCRIPTION','FORMABBR'
                                             ,'PARAMETER'
                                             )
                                           ,sep=''
                                           ,collapse=', '
                                           )
                                     )
                        )
                  )
  df <- df[ordering,]
  intermediateMessage('.4')

  # List columns to write, in order of appearance.  Include all key columns
  # used in the table other than UID, which has been replaced by SITE_ID,
  # VISIT_NO and DATE_COL.
  unitsIfPresent <- names(df)[names(df) %in% 'UNITS']
  flagIfPresent <- names(df)[names(df) %in% 'FLAG']
  exportColumns <- c('UID','SITE_ID', 'VISIT_NO','DATE_COL', tableKeys, 'PARAMETER'
                    ,'TESTDESCRIPTION', 'RESULT'
                    ,unitsIfPresent, flagIfPresent
                    ,'FORMIMAGE', 'COMMENTS'
                    )
  df$COMMENTS <- "                                              "
  df <- df[exportColumns]

  rownames(df) <- NULL
  intermediateMessage('.Done', loc='end')

  return(df)
}

constructNRSAValidationResultsTest <- function()
# Tests constructNRSAValidationResults()
{
  constructNRSAValidationResultsTest.1() # Test with one intra site key and FLAG
  constructNRSAValidationResultsTest.2() # Test with three intra site keys, no FLAG
  constructNRSAValidationResultsTest.3() # Test with zero intra site keys and FLAG
  constructNRSAValidationResultsTest.4() # Test with wadeable & boatable ChRip data
  constructNRSAValidationResultsTest.5() # Test with one intra site key and FLAG
                                         # and prepared for Open Office Calc.
}

constructNRSAValidationResultsTest.1 <- function()
# Tests constructNRSAValidationResults() with one intrasite key, one data year
# and no UNITS column.
{
  baseData <- data.frame(UID=c('1','1','1','2','2','3')
                        ,TRANLINE=c('A','C','E','B','B','XJ')
                        ,SAMPLE_TYPE='test'
                        ,UNITS='furlongs'
                        ,FLAG=c(as.character(NA), '', 'K', '', '', '')
                        ,PARAMETER=c('p1','p2','p3','p2','p4','p1')
                        ,TESTDESCRIPTION=c('bad', 'worse', 'whoa'
                                          ,rep('odd relationship', 2)
                                          ,'just odd'
                                          )
                        ,RESULT=c('one','two','three','four','five','six')
                        ,stringsAsFactors=FALSE
                        )
  formInfo <- data.frame(PARAMETER=paste('p', as.character(1:20), sep='')
                        ,FORMABBR=paste('form'
                                       ,as.character(rep(1:10, each=2))
                                       ,sep=''
                                       )
                        ,SAMPLE_TYPE='test'
                        ,UNITS='furlongs'
                        ,MOREMETADATA='another metadata column'
                        ,stringsAsFactors=FALSE
                        )
  sites <- data.frame(UID=as.character(1:50)
                     ,SITE_ID=paste('site'
                                   ,as.character(rep(1:25, each=2))
                                   ,sep=''
                                   )
                     ,VISIT_NO=rep(1:2, times=25)
                     ,DATE_COL=paste('2008', 4:5, rep(1:25, each=2), sep='-')
                     ,OTHERJUNK='other junk'
                     ,stringsAsFactors=FALSE
                     )

  tt <- merge(baseData, formInfo, by=c('PARAMETER','UNITS','SAMPLE_TYPE')
             ,all.x=TRUE, all.y=FALSE
             )
  expected <- merge(tt, sites, by='UID', all.x=TRUE, all.y=FALSE)
  expected$FORMIMAGE <- paste('=HYPERLINK("file:///'
                             ,'L:/Apps/Scantron/Images/'
                             ,'2008'
                             ,'/Flowing Waters/'
                             ,paste('NRSA'
                                   ,expected$FORMABBR
                                   ,expected$SITE_ID
                                   ,paste('V', expected$VISIT_NO, sep='')
                                   ,sep='_'
                                   )
                             ,'.tif'
                             , '" , "', expected$FORMABBR
                             ,'")'
                             ,sep=''
                             )
  expected$COMMENTS <- "                                              "
  
  expected <- expected[c('UID','SITE_ID','VISIT_NO','DATE_COL','SAMPLE_TYPE'
                        ,'TRANLINE','PARAMETER'
                        ,'TESTDESCRIPTION','RESULT','UNITS','FLAG'
                        ,'FORMIMAGE','COMMENTS'
                        )
                      ]
  rownames(expected)<-NULL
  rr <- constructNRSAValidationResults(baseData, formInfo, sites)
  checkIdentical(expected, rr
             ,"Error: Incorrect construction of validation results with one intrasite key"
             )
}

constructNRSAValidationResultsTest.2 <- function()
# Tests constructNRSAValidationResults() with three intrasite keys, UNITS column
# and multiple years.
{
  baseData <- data.frame(UID=c('1','1','1','2','2','3')
                        ,TRANSECT=c('A','C','E','B','B','XJ')
                        ,TRANSDIR=c('LF','LC','CT','RC','RC','RT')
                        ,SAMPLE_TYPE='test'
                        ,UNITS='furlongs'
                        ,ANALYTE=c('CaO','CaW','CrOW','AgCH3','AgCH3','Hg')
                        ,PARAMETER=c('p1','p2','p3','p2','p4','p1')
                        ,TESTDESCRIPTION=c('bad', 'worse', 'whoa'
                                          ,rep('odd relationship', 2)
                                          ,'just odd'
                                          )
                        ,RESULT=c('one','two','three','four','five','six')
                        ,UNITS=c('NONE','m','ft','celcius','barns','volts')
                        ,stringsAsFactors=FALSE
                        )
  formInfo <- data.frame(PARAMETER=paste('p', as.character(1:20), sep='')
                        ,FORMABBR=paste('form'
                                       ,as.character(rep(1:10, each=2))
                                       ,sep=''
                                       )
                        ,SAMPLE_TYPE='test'
                        ,UNITS='furlongs'
                        ,MOREMETADATA='another metadata column'
                        ,stringsAsFactors=FALSE
                        )
  sites <- data.frame(UID=as.character(1:50)
                     ,SITE_ID=paste('site'
                                   ,as.character(rep(1:25, each=2))
                                   ,sep=''
                                   )
                     ,VISIT_NO=rep(1:2, times=25)
                     ,DATE_COL=paste(c('2008','2009'), 4:5, rep(1:25, each=2), sep='-')
                     ,OTHERJUNK='other junk'
                     ,stringsAsFactors=FALSE
                     )

  tt <- merge(baseData, formInfo, by=c('PARAMETER','UNITS','SAMPLE_TYPE')
             ,all.x=TRUE, all.y=FALSE
             )
  expected <- merge(tt, sites, by='UID', all.x=TRUE, all.y=FALSE)
  expected$FORMIMAGE <- paste('=HYPERLINK("file:///'
                             ,'L:/Apps/Scantron/Images/'
                             ,format(as.POSIXct(expected$DATE_COL), '%Y')
                             ,'/Flowing Waters/'
                             ,paste('NRSA'
                                   ,expected$FORMABBR
                                   ,expected$SITE_ID
                                   ,paste('V', expected$VISIT_NO, sep='')
                                   ,sep='_'
                                   )
                             ,'.tif'
                             , '" , "', expected$FORMABBR
                             ,'")'
                             ,sep=''
                             )
  expected$COMMENTS <- "                                              "

  expected <- expected[c('UID','SITE_ID','VISIT_NO','DATE_COL','SAMPLE_TYPE'
                        ,'TRANSECT','TRANSDIR'
                        ,'ANALYTE','PARAMETER'
                        ,'TESTDESCRIPTION','RESULT','UNITS'
                        ,'FORMIMAGE','COMMENTS'
                        )
                      ]
  rownames(expected)<-NULL
  rr <- constructNRSAValidationResults(baseData, formInfo, sites)

  checkIdentical(expected, rr
    ,"Error: Incorrect construction of validation results with three intrasite keys"
    )
}

constructNRSAValidationResultsTest.3 <- function()
# Tests constructNRSAValidationResults() with zero intrasite keys, UNITS column
# and multiple years.
{
  baseData <- data.frame(UID=c('1','1','1','2','2','3')
                        ,PARAMETER=c('p1','p2','p3','p2','p4','p1')
                        ,SAMPLE_TYPE='test'
                        ,UNITS='furlongs'
                        ,FLAG=c(as.character(NA), '', 'K', '', '', '')
                        ,TESTDESCRIPTION=c('bad', 'worse', 'whoa'
                                          ,rep('odd relationship', 2)
                                          ,'just odd'
                                          )
                        ,RESULT=c('one','two','three','four','five','six')
                        ,UNITS=c('NONE','m','ft','celcius','barns','volts')
                        ,stringsAsFactors=FALSE
                        )
  formInfo <- data.frame(PARAMETER=paste('p', as.character(1:20), sep='')
                        ,FORMABBR=paste('form'
                                       ,as.character(rep(1:10, each=2))
                                       ,sep=''
                                       )
                        ,UNITS='furlongs'
                        ,SAMPLE_TYPE='test'
                        ,MOREMETADATA='another metadata column'
                        ,stringsAsFactors=FALSE
                        )
  sites <- data.frame(UID=as.character(1:50)
                     ,SITE_ID=paste('site'
                                   ,as.character(rep(1:25, each=2))
                                   ,sep=''
                                   )
                     ,VISIT_NO=rep(1:2, times=25)
                     ,DATE_COL=paste(c('2008','2009','2010'), 4:5, rep(1:25, each=2), sep='-')
                     ,OTHERJUNK='other junk'
                     ,stringsAsFactors=FALSE
                     )

  tt <- merge(baseData, formInfo, by=c('PARAMETER','UNITS','SAMPLE_TYPE')
             ,all.x=TRUE, all.y=FALSE
             )
  expected <- merge(tt, sites, by='UID', all.x=TRUE, all.y=FALSE)
  expected$FORMIMAGE <- paste('=HYPERLINK("file:///'
                             ,'L:/Apps/Scantron/Images/'
                             ,format(as.POSIXct(expected$DATE_COL), '%Y')
                             ,'/Flowing Waters/'
                             ,paste('NRSA'
                                   ,expected$FORMABBR
                                   ,expected$SITE_ID
                                   ,paste('V', expected$VISIT_NO, sep='')
                                   ,sep='_'
                                   )
                             ,'.tif'
                             , '" , "', expected$FORMABBR
                             ,'")'
                             ,sep=''
                             )
  expected$COMMENTS <- "                                              "

  # Insert spacer lines to visually distinguish separate issues.  Note minor
  # reordering of rows, as without intrasite keys, TESTDESCRIPTION now affects
  # ordering.
  expected <- rbind(expected[1,]
                   ,expected[3,]
                   ,expected[2,]
                   ,expected[4,]
                   ,expected[5,]
                   ,expected[6,]
                   )

  expected <- expected[c('UID','SITE_ID','VISIT_NO','DATE_COL','SAMPLE_TYPE'
                        ,'PARAMETER'
                        ,'TESTDESCRIPTION','RESULT','UNITS','FLAG'
                        ,'FORMIMAGE','COMMENTS'
                        )
                      ]
  rownames(expected)<-NULL
  rr <- constructNRSAValidationResults(baseData, formInfo, sites)
  checkIdentical(expected, rr
    ,"Error: Incorrect construction of validation results with zero intrasite keys"
    )
}

constructNRSAValidationResultsTest.4 <- function()
# Tests constructNRSAValidationResults() with wadeable and boatable ChRip data
# in both 2008 and 2009
{
  baseData <- data.frame(UID=c('1','2','3','4','4','4')
                        ,TRANSECT=c('A','C','E','B','B','XJ')
                        ,SAMPLE_TYPE=c(rep('PHAB_CHANW',2)
                                      ,rep('PHAB_CHANB',4)
                                      )
                        ,UNITS='furlongs'
                        ,FLAG=c(as.character(NA), '', 'K', '', '', '')
                        ,PARAMETER=c("CANBTRE", "CANSTRE"
                                    ,"CANVEG", "CANBTRE", "CANSTRE", "SHOREDOM"
                                    )
                        ,TESTDESCRIPTION=c('bad', 'worse', 'whoa'
                                          ,rep('odd relationship', 2)
                                          ,'just odd'
                                          )
                        ,RESULT=c('one','two','three','four','five','six')
                        ,stringsAsFactors=FALSE
                        )
  formInfo <- data.frame(PARAMETER=c("CANBTRE", "CANSTRE", "CANVEG"
                                    ,"CANBTRE", "CANSTRE", "SHOREDOM"
                                    )
                        ,FORMABBR='ChRip'
                        ,SAMPLE_TYPE=c(rep('PHAB_CHANW',2)
                                      ,rep('PHAB_CHANB',4)
                                      )
                        ,UNITS='furlongs'
                        ,MOREMETADATA='another metadata column'
                        ,stringsAsFactors=FALSE
                        )
  sites <- data.frame(UID=as.character(1:40)
                     ,SITE_ID=paste('site'
                                   ,as.character(rep(1:20, each=2))
                                   ,sep=''
                                   )
                     ,VISIT_NO=rep(1:2, each=2, times=10)
                     ,DATE_COL=as.POSIXct(paste(c('2008','2009')
                                               ,4:5
                                               ,rep(1:20, each=2)
                                               ,sep='-'
                                               )
                                         )
                     ,OTHERJUNK='other junk'
                     ,stringsAsFactors=FALSE
                     )

  # Create 'expected' dataframe
  tt <- merge(baseData, formInfo, by=c('PARAMETER','UNITS','SAMPLE_TYPE')
             ,all.x=TRUE, all.y=FALSE
             )
  tt <- merge(tt, sites, by='UID', all.x=TRUE, all.y=FALSE)

  tt1 <-subset(tt, SAMPLE_TYPE=='PHAB_CHANW')             # wadeable data
  tt2 <-subset(tt, SAMPLE_TYPE=='PHAB_CHANB' & UID=='3')  # boatable data 2008
  tt3 <-subset(tt, SAMPLE_TYPE=='PHAB_CHANB' & UID=='4')  # boatable data 2009
  tt1$FORMIMAGE <- paste('=HYPERLINK("file:///'
                        ,'L:/Apps/Scantron/Images/'
                        ,format(as.POSIXct(tt1$DATE_COL), '%Y')
                        ,'/Flowing Waters/'
                        ,paste('NRSA'
                              ,tt1$FORMABBR
                              ,tt1$SITE_ID
                              ,paste('V', tt1$VISIT_NO, sep='')
                              ,tt1$TRANSECT
                              ,sep='_'
                              )
                        ,'.tif'
                        , '" , "', tt1$FORMABBR
                        ,'")'
                        ,sep=''
                        )
  tt2$FORMIMAGE <- paste('=HYPERLINK("file:///'
                        ,'L:/Apps/Scantron/Images/'
                        ,format(as.POSIXct(tt2$DATE_COL), '%Y')
                        ,'/Flowing Waters/'
                        ,paste('NRSA'
                              ,tt2$FORMABBR
                              ,tt2$SITE_ID
                              ,paste('V', tt2$VISIT_NO, sep='')
                              ,tt2$TRANSECT
                              ,sep='_'
                              )
                        ,'.tif'
                        , '" , "', tt2$FORMABBR
                        ,'")'
                        ,sep=''
                        )
  tt3$FORMIMAGE <- paste('=HYPERLINK("file:///'
                        ,'L:/Apps/Scantron/Images/'
                        ,format(as.POSIXct(tt3$DATE_COL), '%Y')
                        ,'/Flowing Waters/'
                        ,paste('NRSA'
                              ,tt3$FORMABBR
                              ,tt3$SITE_ID
                              ,paste('V', tt3$VISIT_NO, sep='')
                              ,tt3$TRANSECT
                              ,c('P2','P2','P1')
                              ,sep='_'
                              )
                        ,'.tif'
                        ,'" , "', tt3$FORMABBR
                        ,'")'
                        ,sep=''
                        )
  expected <- rbind(tt1, tt2, tt3)
  expected$COMMENTS <- "                                              "

  expected <- expected[c('UID','SITE_ID','VISIT_NO','DATE_COL','SAMPLE_TYPE'
                        ,'TRANSECT','PARAMETER'
                        ,'TESTDESCRIPTION','RESULT','UNITS','FLAG'
                        ,'FORMIMAGE','COMMENTS'
                        )
                      ]
  rownames(expected)<-NULL
  rr <- constructNRSAValidationResults(baseData, formInfo, sites)
  checkIdentical(expected, rr
             ,"Error: Incorrect construction of validation results with ChRip-like data"
             )
}

constructNRSAValidationResultsTest.5 <- function()
# Tests constructNRSAValidationResults() with one intrasite key, one data year
# and no UNITS column, and prepared for Open Office Calc instead of Excel.
{
  baseData <- data.frame(UID=c('1','1','1','2','2','3')
                        ,TRANLINE=c('A','C','E','B','B','XJ')
                        ,SAMPLE_TYPE='test'
                        ,UNITS='furlongs'
                        ,FLAG=c(as.character(NA), '', 'K', '', '', '')
                        ,PARAMETER=c('p1','p2','p3','p2','p4','p1')
                        ,TESTDESCRIPTION=c('bad', 'worse', 'whoa'
                                          ,rep('odd relationship', 2)
                                          ,'just odd'
                                          )
                        ,RESULT=c('one','two','three','four','five','six')
                        ,stringsAsFactors=FALSE
                        )
  formInfo <- data.frame(PARAMETER=paste('p', as.character(1:20), sep='')
                        ,FORMABBR=paste('form'
                                       ,as.character(rep(1:10, each=2))
                                       ,sep=''
                                       )
                        ,SAMPLE_TYPE='test'
                        ,UNITS='furlongs'
                        ,MOREMETADATA='another metadata column'
                        ,stringsAsFactors=FALSE
                        )
  sites <- data.frame(UID=as.character(1:50)
                     ,SITE_ID=paste('site'
                                   ,as.character(rep(1:25, each=2))
                                   ,sep=''
                                   )
                     ,VISIT_NO=rep(1:2, times=25)
                     ,DATE_COL=paste('2008', 4:5, rep(1:25, each=2), sep='-')
                     ,OTHERJUNK='other junk'
                     ,stringsAsFactors=FALSE
                     )

  tt <- merge(baseData, formInfo, by=c('PARAMETER','UNITS','SAMPLE_TYPE')
             ,all.x=TRUE, all.y=FALSE
             )
  expected <- merge(tt, sites, by='UID', all.x=TRUE, all.y=FALSE)
  expected$FORMIMAGE <- paste('=HYPERLINK("file:///'
                             ,'L:/Apps/Scantron/Images/'
                             ,'2008'
                             ,'/Flowing Waters/'
                             ,paste('NRSA'
                                   ,expected$FORMABBR
                                   ,expected$SITE_ID
                                   ,paste('V', expected$VISIT_NO, sep='')
                                   ,sep='_'
                                   )
                             ,'.tif'
                             , '" ; "', expected$FORMABBR
                             ,'")'
                             ,sep=''
                             )
  expected$COMMENTS <- "                                              "

  expected <- expected[c('UID','SITE_ID','VISIT_NO','DATE_COL','SAMPLE_TYPE'
                        ,'TRANLINE','PARAMETER'
                        ,'TESTDESCRIPTION','RESULT','UNITS','FLAG'
                        ,'FORMIMAGE','COMMENTS'
                        )
                      ]
  rownames(expected)<-NULL
  rr <- constructNRSAValidationResults(baseData, formInfo, sites, ssFmt='OO')
  checkIdentical(expected, rr
             ,"Error: Incorrect construction of validation results with one intrasite key formatted for OO Calc"
             )
}

# end of file
