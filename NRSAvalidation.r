# NRSAvalidation.r
#
# Sources functions used to validate NRSA data tables
#
# 11/09/09 cws Removed definition of NRSAFormImageLocation string.
# 12/08/09 cws Added SAMPLE_TYPE, SAMPLE_CAT, PAGE to NRSAKeyColumns.
# 12/15/09 cws Added BATCHNO to NRSAKeyColumns for use in valUpdateTable().
# 01/04/10 cws Added definition of NRSACalcLocation
# 02/09/10 cws Added definition of summaryby
# 02/25/10 cws Added definitions of metrics and other functions, moved all
#          source() calls to here.
# 04/12/10 cws Updated NRSAvalidationLocation to point to Island2.

# Define location where validation files will be written and read from:
NRSAvalidationLocation <- 'l:/Priv/CORFiles/IM/Rwork/nrsa/projects/validation/Island2/'

# Define location where metrics files will be written and read from:
NRSACalcLocation <- 'l:/Priv/CORFiles/IM/Rwork/nrsa/results/'

# Define location where form images are stored
# NRSAFormImageLocation <- 'L:/Apps/Scantron/Images/2008/Flowing Waters/'

# Define list of column names that occur as keys in NRSA tables.
NRSAKeyColumns <- c('UID', 'BATCHNO','TRANSECT','TRANSDIR','STATION','REP'
                   ,'LINE', 'PAGE', 'TRANLINE','BANK','ANALYTE', 'SAMPLE_TYPE'
                   ,'SAMPLE_CAT'
                   )
                   
# Test suites
# testSuiteDb(); testSuiteValidation(); testSuiteSharedCode(); testSuiteNRSAValidation(); testSuiteNRSAMetrics();
source('l:/Priv/CORFiles/IM/Rwork/SharedCode/testSuiteDb.r')
source('l:/Priv/CORFiles/IM/Rwork/SharedCode/testSuiteValidation.r')
source('l:/Priv/CORFiles/IM/Rwork/SharedCode/testSuiteSharedCode.r')
source('l:/Priv/CORFiles/IM/Rwork/nrsa/code/testSuiteNRSAValidation.r')
source('l:/Priv/CORFiles/IM/Rwork/nrsa/code/testSuiteNRSAMetrics.r')

# Miscellaneous support functions:
source('l:/Priv/CORFiles/IM/Rwork/SharedCode/sharedSupport.r')
source('l:/Priv/CORFiles/IM/Rwork/SharedCode/db.r')
source('l:/Priv/CORFiles/IM/Rwork/SharedCode/validation.r')
source('l:/Priv/CORFiles/IM/Rwork/SharedCode/modalCount.r')
source('l:/Priv/CORFiles/IM/Rwork/SharedCode/mem.dir.r')
source('l:/Priv/CORFiles/IM/Rwork/SharedCode/prepareDataForSAS.r')
source('l:/Priv/CORFiles/IM/Rwork/nrsa/code/constructNRSAValidationResults.r')
source('l:/Priv/CORFiles/IM/Rwork/nrsa/code/constructNRSAValidationUpdate.r')
source('l:/Priv/CORFiles/IM/Rwork/nrsa/code/fetchNRSATable.r')
source('l:/Priv/CORFiles/IM/Rwork/nrsa/code/interpolatePercentile.r')
source('l:/Priv/CORFiles/IM/Rwork/nrsa/code/NA_filler.r')
source('l:/Priv/CORFiles/IM/Rwork/nrsa/code/nWadeableStationsPerTransect.r')
source('l:/Priv/CORFiles/IM/Rwork/nrsa/code/quickUnitsCheck.r')
source('l:/Priv/CORFiles/IM/Rwork/nrsa/code/readNRSACalculationResults.r')
source('l:/Priv/CORFiles/IM/Rwork/nrsa/code/readNRSAValidationResults.r')
source('l:/Priv/CORFiles/IM/Rwork/nrsa/code/siteProtocol.r')
source('l:/Priv/CORFiles/IM/Rwork/nrsa/code/summaryby.r')
source('l:/Priv/CORFiles/IM/Rwork/nrsa/code/valUpdateTable.r')
source('l:/Priv/CORFiles/IM/Rwork/nrsa/code/writeNRSAValidationResults.r')
source('l:/Priv/CORFiles/IM/Rwork/nrsa/code/writeNRSACalcResults.r')
source('l:/Priv/CORFiles/IM/Rwork/nrsa/code/writeNRSACalcResults.r')


# Structure checks:
source('l:/Priv/CORFiles/IM/Rwork/nrsa/code/valStructBankGeometry.r')
source('l:/Priv/CORFiles/IM/Rwork/nrsa/code/valStructBenthic.r')
source('l:/Priv/CORFiles/IM/Rwork/nrsa/code/valStructChanChar.r')
source('l:/Priv/CORFiles/IM/Rwork/nrsa/code/valStructChanCov.r')
source('l:/Priv/CORFiles/IM/Rwork/nrsa/code/valStructChanDepth.r')
source('l:/Priv/CORFiles/IM/Rwork/nrsa/code/valStructChannelCrossSection.r')
source('l:/Priv/CORFiles/IM/Rwork/nrsa/code/valStructChannelGeometry.r')
source('l:/Priv/CORFiles/IM/Rwork/nrsa/code/valStructDischarge.r')
source('l:/Priv/CORFiles/IM/Rwork/nrsa/code/valStructFieldCalibration.r')
source('l:/Priv/CORFiles/IM/Rwork/nrsa/code/valStructFieldMeasure.r')
source('l:/Priv/CORFiles/IM/Rwork/nrsa/code/valStructFishCollection.r')
source('l:/Priv/CORFiles/IM/Rwork/nrsa/code/valStructFishCov.r')
source('l:/Priv/CORFiles/IM/Rwork/nrsa/code/valStructInvasiveLegacy.r')
source('l:/Priv/CORFiles/IM/Rwork/nrsa/code/valStructLittoral.r')
source('l:/Priv/CORFiles/IM/Rwork/nrsa/code/valStructOtherInvasives.r')
source('l:/Priv/CORFiles/IM/Rwork/nrsa/code/valStructThalweg.r')
source('l:/Priv/CORFiles/IM/Rwork/nrsa/code/valStructTorrent.r')
source('l:/Priv/CORFiles/IM/Rwork/nrsa/code/valStructVisAssess.r')
source('l:/Priv/CORFiles/IM/Rwork/nrsa/code/valStructWood.r')

# Other Validation checks checks:
source('l:/Priv/CORFiles/IM/Rwork/nrsa/code/valMissingCheck.r')
source('l:/Priv/CORFiles/IM/Rwork/nrsa/code/valLegalCheck.r')
source('l:/Priv/CORFiles/IM/Rwork/nrsa/code/valLegalUnitsCheck.r')
source('l:/Priv/CORFiles/IM/Rwork/nrsa/code/valRangeCheck.r')
source('l:/Priv/CORFiles/IM/Rwork/nrsa/code/valRangeThalwegSupplemental.r')
source('l:/Priv/CORFiles/IM/Rwork/nrsa/code/valRangeSlope.r')
source('l:/Priv/CORFiles/IM/Rwork/nrsa/code/neighborBasedValidation.r')
source('l:/Priv/CORFiles/IM/Rwork/nrsa/code/valLogicThalweg.r')
source('l:/Priv/CORFiles/IM/Rwork/nrsa/code/valLogicChannelWidths.r')
source('l:/Priv/CORFiles/IM/Rwork/nrsa/code/valLogicChannelWidths.r')

# Metrics calculation functions
source('l:/Priv/CORFiles/IM/Rwork/nrsa/code/metsBankMorphology.r')
source('l:/Priv/CORFiles/IM/Rwork/nrsa/code/metsBedStability.r')
source('l:/Priv/CORFiles/IM/Rwork/nrsa/code/metsCanopyDensiometer.r')
source('l:/Priv/CORFiles/IM/Rwork/nrsa/code/metsChannelChar.r')
source('l:/Priv/CORFiles/IM/Rwork/nrsa/code/metsChannelHabitat.r')
source('l:/Priv/CORFiles/IM/Rwork/nrsa/code/metsChannelMorphology.r')
source('l:/Priv/CORFiles/IM/Rwork/nrsa/code/metsFishCover.r')
source('l:/Priv/CORFiles/IM/Rwork/nrsa/code/metsGeneral.r')
source('l:/Priv/CORFiles/IM/Rwork/nrsa/code/metsHumanInfluence.r')
source('l:/Priv/CORFiles/IM/Rwork/nrsa/code/metsInvasiveSpecies.r')
source('l:/Priv/CORFiles/IM/Rwork/nrsa/code/metsLargeWoody.r')
source('l:/Priv/CORFiles/IM/Rwork/nrsa/code/metsLegacyTree.r')
source('l:/Priv/CORFiles/IM/Rwork/nrsa/code/metsLittoralDepth.r')
source('l:/Priv/CORFiles/IM/Rwork/nrsa/code/metsResidualPools.r')
source('l:/Priv/CORFiles/IM/Rwork/nrsa/code/metsRiparianVegetation.r')
source('l:/Priv/CORFiles/IM/Rwork/nrsa/code/metsSlopeBearing.r')
source('l:/Priv/CORFiles/IM/Rwork/nrsa/code/metsSubstrateEmbed.r')
source('l:/Priv/CORFiles/IM/Rwork/nrsa/code/metsSubstrateCharacterization.r')
source('l:/Priv/CORFiles/IM/Rwork/nrsa/code/nrsaPhab.r')


# end of file
