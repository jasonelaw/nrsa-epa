# neightborBasedValidation.r
#
# 12/18/09 cws Created
#

neightborBasedValidation <- function(df, keys, name, value, parameter, tbf, min, max)
# Performs a context-based range check of a value based on neighboring values,
# to find values that exceed their neighboring values by more than a specified
# factor, or which exceed specified constants if the neighboring values are
# missing.
#
# Returns a subset of the input dataframe containing the flagged values with
# an additional column TESTDESCRIPTION, and optionally including rows with their
# neighboring values as well.
#
# ARGUMENTS:
# df          dataframe to undergo this validation test
# keys        keys used to uniquely identify each row in the dataframe
# name        string with name of column holding names of parameters in dataframe
# value       string with name of column holding value of parameters in dataframe
# parameter   name of parameter to check
# tbf         factor by which adjacent values may be expected to vary; values
#               which change greater than this factor are flagged for validation.
# min, max    numeric values specifying the expected range of the parameter; this
#               static range test is only performed when a value has no nonmissing
#               neighbors.
#
# ASSUMPTIONS:
# Creation of temporary columns ..value, ..first, ..last, ..next, ..prev
#   and ..flag is OK.
#
{
  ds <- df[df[name]==parameter,]  # subset to specific parameter

  # Converting values to a numeric mode is difficult to do in a single step
  # without generating warning messages, so three steps are used.
  ds$..value <- unlist(ds[value])
  ds$..value <- ifelse(is.na(ds$..value), NA, ds$..value)
  ds$..value <- ifelse(trimws(ds$..value) == '' | ds$..value == '.', NA, ds$..value)
  ds$..value <- as.numeric(ds$..value)

  # Adjacent values are obtained with lag() and lead().  The beginning and end
  # of a series has no previous or next neighbors (respectively), so those are
  # made missing; lag()ing and lead()ing thus requires first() and last(), and
  # thus appropriate ordering.
  # if keys==c('UID','TRANSECT','STATION') then this statement will parse to
  # order(ds$UID, ds$TRANSECT, ds$STATION)
  ordering <- eval(parse(text=sprintf('order(%s)'
                                     ,paste('ds$', keys, sep='', collapse=', ')
                                     )
                        )
                  )
  ds <- ds[ordering,]

  ds <- first(ds, keys[1], '..first')
  ds <- last(ds, keys[1], '..last')
  ds <- lag(ds, '..value', '..prev')
  ds <- lead(ds, '..value', '..next')
  ds$..prev <- ifelse(ds$..first, NA, ds$..prev)
  ds$..next <- ifelse(ds$..last, NA, ds$..next)


  # Compare values with their available neighbors.
  ds$TESTDESCRIPTION <- as.character(NA)
  ds$..flag <- ifelse(!(is.na(ds$..prev) | is.na(ds$..next))
                     ,(ds$..value != 0 & ds$..prev != 0 & ds$..next != 0) &
                      (ds$..value > ds$..prev*tbf | ds$..value < ds$..prev/tbf |
                       ds$..value > ds$..next*tbf | ds$..value < ds$..next/tbf
                      )
                     ,ifelse(!is.na(ds$..prev)
                            ,(ds$..value != 0 & ds$..prev != 0) &
                             (ds$..value > ds$..prev*tbf |
                              ds$..value < ds$..prev/tbf
                             )
                            ,ifelse(!is.na(ds$..next)
                                   ,(ds$..value != 0 & ds$..next != 0) &
                                    (ds$..value > ds$..next*tbf |
                                     ds$..value < ds$..next/tbf
                                    )
                                   ,NA
                                   )
                            )
                     )
  ds$TESTDESCRIPTION <- ifelse(ds$..flag,'Value varies considerably from its neighbors', NA)

  # perform static range checks, used to fill in gaps due to missing values
  if(!is.null(max)) {
      if(!is.null(min)) {
          ff <- ds$..value > max | ds$..value < min
      } else {
          ff <- ds$..value > max
      }
  } else {
      if(!is.null(min)) {
          ff <- ds$..value < min
      } else {
          ff <- FALSE
      }
  }

  # fill in missing checks with static range values, if provided
  ds$TESTDESCRIPTION <- ifelse(is.na(ds$..flag) & ff
                              ,sprintf('Value exceeds specified range (%s,%s)'
                                      ,min, max
                                      )
                              ,ds$TESTDESCRIPTION
                              )

  ds <- subset(ds, !is.na(TESTDESCRIPTION)
              ,select=-c(..value,..flag,..first,..last,..prev,..next)
              )

  return(ds)
}


neightborBasedValidationTest <- function()
# unit test for neightborBasedValidation()
{
  testData <- data.frame('k1'=rep(1:4, each=10)
                        ,'k2'=rep(1:10, times=4)
                        ,'par'=rep('dist', times=40)
                        ,'val'=c(1, 10,  1,  1,  1, 10, 10,  1,  1, 10
                                ,10, 1,  1, 10,100, 10,  1, 10, 10, 10
                                ,NA, 1,  1,  0, 10,  1, 10,  1, NA, 10
                                ,1, NA,  1, NA,  1,  1,  0, 10,  0,  1
                                )
                        ,stringsAsFactors=FALSE
                        )

  # with full arguments
  rr <- neightborBasedValidation(testData, c('k1','k2'), 'par', 'val', 'dist', 5, 0, 7)
  rownames(rr) <- NULL
  ee <- subset(testData
              ,k1==1 & k2 %in% c(1,2,3,5,6,7,8,9,10) |
               k1==2 & k2 %in% c(1:8) |
               k1==3 & k2 %in% c(6,7,8,10)
              )
  ee$TESTDESCRIPTION <- 'Value varies considerably from its neighbors'
  ee[ee$k1==3 & ee$k2==10,]$TESTDESCRIPTION <- "Value exceeds specified range (0,7)"
  rownames(ee) <- NULL

  checkEquals(ee,rr
             ,'Error: Did not correctly detect odd neighboring values with range check'
             )

  # no static range checks
  rr <- neightborBasedValidation(testData, c('k1','k2'), 'par', 'val', 'dist', 5, NULL, NULL)
  rownames(rr) <- NULL
  ee <- subset(testData
              ,k1==1 & k2 %in% c(1,2,3,5,6,7,8,9,10) |
               k1==2 & k2 %in% c(1:8) |
               k1==3 & k2 %in% c(6,7,8)
              )
  ee$TESTDESCRIPTION <- 'Value varies considerably from its neighbors'
  rownames(ee) <- NULL

  checkEquals(ee,rr,
             'Error: Did not correctly detect odd neighboring values with no range check'
             )

}


# end of file