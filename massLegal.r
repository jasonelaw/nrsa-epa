# massLegal.r
#
# Runs missing values tests on all tbl* tables in NRSA.  A simple tweak would
# run missing and range tests.
#
require(RODBC)

chan <- odbcConnect('NRSA2')

tables <- subset(sqlTables(chan)
                ,substr(TABLE_NAME,1,3)=='tbl' &
                 !(TABLE_NAME %in% c('tblCOMMENTS2','tblFISHPHOTO2'
                                    ,'tblLABRESULTS2','tblPARAMETERDESCRIPTIONS2'
                                    ,'tblVISITS2','tblSAMPLECHARACTERISTICS2'
                                    )
                  )
                )$TABLE_NAME


for(t in tables) {
  # Let us know what's happening
  print(sprintf("Working on %s", t))

  # Run a test
  valLegalCheck(t)
}
