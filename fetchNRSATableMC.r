# fetchNRSATable.r
#
# 10/05/09 cws Created
# 11/09/09 cws Does not create UID column unless BATCHNO column exists.
#          Explicitely de-factors character columns.
# 11/13/09 cws Explicitely sets NA UNITS to ''
# 12/03/09 cws Standardizing empty UNITS as NONE and upcasing UNITS.
# 12/04/09 ssr/mrc Brilliant work, now confident...added at and where arguments... took about 30 seconds.
#  2/25/2010 cws moved source() calls to NRSAvalidation.r
# 4/14/2010 mrc write a unit test for fetchNRSATable
#               exclude datalines for sidechannels when PROTOCOL is BOATABLE

fetchNRSATable <- function (chan, tblName, at=NULL, where=NULL, filterSideChannels=TRUE)
# Retrieves the specified NRSA table via an ODBC connection and standardizes
# the contents as follows:
#   a) Changes numeric column 'BATCHNO' to character column 'UID'
#   b) Trims surrounding whitespace from the contents of character columns
#
# Returns the standardized table contents as a dataframe if successful, or
# a character string if an error occurs.
#
# ARGUMENTS:
# chan      an open RODBC channel object through which the data table is read.
# tblName   a string specifying the table to read.
# where    optional string containing an SQL WHERE clause for retrieving
#                  a subset of the table.
# ASSUMPTIONS:
#
{
#first to collect some protocol data so we can exclude river sidechannels

 if (filterSideChannels = TRUE)
  {

   visitInfo <- dbFetch (chan, 'tblVISITS2', at=NULL, where=NULL)
                         if (is.character (visitInfo)) return ('table not here')
   visitInfo <- rename (visitInfo, 'BATCHNO', 'UID')

        riverSites <-  siteProtocol(visitInfo$UID)
        riverSites <- subset(riverSites, PROTOCOL=='BOATABLE')
        riverSites <- riverSites$UID
        
 #       subset (siteProtocol(visitInfo$UID)
 #                                    ,PROTOCOL=='BOATABLE'
 #                                    )$UID
                                     
   filterClause <- paste ("NOT(TRANSECT IN  ('XA', 'XB', 'XC', 'XD', 'XE', 'XF', 'XG', 'XH','XI', 'XJ', 'XK')
                                                   AND BATCHNO IN ("
                                                   ,paste (sprintf ("'%s'",riverSites), collapse=', ')
                                                   ,"))"
                                                   
                                                   )
                                                 
  whereClause <- paste (c(where, filterClause), collapse=' AND ')
      }
      else {
      whereClause=where
            }
     
  
# where <- paste (c(where, filterClause), collapse=' AND ')                                                 
#cat('.1')
  # Attempt to retrieve data, returning error message if unsuccessful
  tt <- dbFetch(chan, tblName, at, where=whereClause)
  if(class(tt) == 'character') {
#cat('.x')
      return(tt)
  }
#cat('.2')

  # Convert numeric column 'BATCHNO' to character column 'UID'
  if('BATCHNO' %in% names(tt)) {
      tt<-rename(tt, 'BATCHNO', 'UID')
      tt$UID <- as.character(tt$UID)
  }
#cat('.3')

  # Trim surrounding whitespace from the contents of character columns
  for(colName in names(tt)) {
      if(is.factor(unlist(tt[colName]))) {
          tt[colName] <- as.character(unlist(tt[colName]))
      }
      if(is.character(unlist(tt[colName]))) {
          tt[colName] <- trimws(unlist(tt[colName]))
      }
  }

#cat('.4')
  if('UNITS' %in% names(tt)) {
      # Standardize missing units as NONE
      if(any(is.na(tt$UNITS) | trimws(tt$UNITS)=='')) {
          tt[is.na(tt$UNITS) | trimws(tt$UNITS)=='',]$UNITS <- 'NONE'
      }

      # Standardize units as all caps
      tt$UNITS <- toupper(tt$UNITS)
  }

  return(tt)

}

fetchNRSATableTest <- function ()
# Tests fetchNRSATable
{
# Create test data ... channel, and table; use NORTHWIND test db

#  chan <- odbcConnect ('Northwind')
#  baseTest <- rbind(expand.grid(UID=as.character(1000 + 1:10)
#                               ,TRANSECT=LETTERS[1:11]
#                              ,TRANSDIR=c('LF','RT','CU','CD','CL','CR')
#                               ,SAMPLE_TYPE='PHAB_CHANW'
#                               )
#                   ,expand.grid(UID=as.character(2000 + 1:10)
#                               ,TRANSECT=LETTERS[1:11]
#                               ,TRANSDIR=c('LF','RT','UP','DN')
#                               ,SAMPLE_TYPE='PHAB_CHANB'
#                               )
#                   )
}
# end of file