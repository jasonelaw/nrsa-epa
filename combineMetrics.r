# combineMetrics.r
#
# Assembles the individual temporary metrics .csv files into two .csv files
# containing all the metrics: phabmet.csv has sites identified with UIDs, and
# phabmet_siteids.csv identifies sites with SITE_ID, VISIT_NO and DATE_COL.
#
#  3/29/10 cws Created
#

  nrsaMets <- NULL
  
  tt <- readNRSACalculationResults('metsBankMorphology.csv')
  if(is.data.frame(tt)) {
      nrsaMets <- rbind(nrsaMets, tt)
  } else {
      print(sprintf("Error reading bank morphology mets: %s\n", tt))
  }
  
  tt <- readNRSACalculationResults('metsCanopyDensiometer.csv')
  if(is.data.frame(tt)) {
      nrsaMets <- rbind(nrsaMets, tt)
  } else {
      print(sprintf("Error reading canopy densiometer mets: %s\n", tt))
  }

  tt <- readNRSACalculationResults('metsChannelChar.csv')
  if(is.data.frame(tt)) {
      nrsaMets <- rbind(nrsaMets, tt)
  } else {
      print(sprintf("Error reading channel characteristics mets: %s\n", tt))
  }

  tt <- readNRSACalculationResults('metsChannelHabitat.csv')
  if(is.data.frame(tt)) {
      nrsaMets <- rbind(nrsaMets, tt)
  } else {
      print(sprintf("Error reading channel habitat mets: %s\n", tt))
  }

  tt <- readNRSACalculationResults('metsChannelMorphology.csv')
  if(is.data.frame(tt)) {
      nrsaMets <- rbind(nrsaMets, tt)
  } else {
      print(sprintf("Error reading channel morphology mets: %s\n", tt))
  }

  tt <- readNRSACalculationResults('metsFishCover.csv')
  if(is.data.frame(tt)) {
      nrsaMets <- rbind(nrsaMets, tt)
  } else {
      print(sprintf("Error reading fish cover mets: %s\n", tt))
  }

  tt <- readNRSACalculationResults('metsGeneral.csv')
  if(is.data.frame(tt)) {
      nrsaMets <- rbind(nrsaMets, tt)
  } else {
      print(sprintf("Error reading general mets: %s\n", tt))
  }

  tt <- readNRSACalculationResults('metsHumanInfluence.csv')
  if(is.data.frame(tt)) {
      nrsaMets <- rbind(nrsaMets, tt)
  } else {
      print(sprintf("Error reading human influence mets: %s\n", tt))
  }

  tt <- readNRSACalculationResults('metsInvasiveSpecies.csv')
  if(is.data.frame(tt)) {
      nrsaMets <- rbind(nrsaMets, tt)
  } else {
      print(sprintf("Error reading invasive species mets: %s\n", tt))
  }

  tt <- readNRSACalculationResults('metsLargeWoody.csv')
  if(is.data.frame(tt)) {
      nrsaMets <- rbind(nrsaMets, tt)
  } else {
      print(sprintf("Error reading LWD mets: %s\n", tt))
  }

  tt <- readNRSACalculationResults('metsLegacyTree.csv')
  if(is.data.frame(tt)) {
      nrsaMets <- rbind(nrsaMets, tt)
  } else {
      print(sprintf("Error reading legacy tree mets: %s\n", tt))
  }

  tt <- readNRSACalculationResults('metsLittoralDepth.csv')
  if(is.data.frame(tt)) {
      nrsaMets <- rbind(nrsaMets, tt)
  } else {
      print(sprintf("Error reading littoral depth mets: %s\n", tt))
  }

  tt <- readNRSACalculationResults('metsResidualPools.csv')
  if(is.data.frame(tt)) {
      nrsaMets <- rbind(nrsaMets, tt)
  } else {
      print(sprintf("Error reading residual pool mets: %s\n", tt))
  }

  tt <- readNRSACalculationResults('metsRiparianVegetation.csv')
  if(is.data.frame(tt)) {
      nrsaMets <- rbind(nrsaMets, tt)
  } else {
      print(sprintf("Error reading riparian vegetation mets: %s\n", tt))
  }

  tt <- readNRSACalculationResults('metsSlopeBearing.csv')
  if(is.data.frame(tt)) {
      nrsaMets <- rbind(nrsaMets, tt)
  } else {
      print(sprintf("Error reading slope & bearing mets: %s\n", tt))
  }

  tt <- readNRSACalculationResults('metsSubstrateCharacterization.csv')
  if(is.data.frame(tt)) {
      nrsaMets <- rbind(nrsaMets, tt)
  } else {
      print(sprintf("Error reading substrate characterization mets: %s\n", tt))
  }

  tt <- readNRSACalculationResults('metsSubstrateEmbed.csv')
  if(is.data.frame(tt)) {
      nrsaMets <- rbind(nrsaMets, tt)
  } else {
      print(sprintf("Error reading bsubstrate embededness mets: %s\n", tt))
  }

  tt <- readNRSACalculationResults('metsBedStability.csv')
  if(is.data.frame(tt)) {
      nrsaMets <- rbind(nrsaMets, tt)
  } else {
      print(sprintf("Error reading bed stability mets: %s\n", tt))
  }

  writeNRSACalcResults(nrsaMets, 'phabmet.csv')
  
  chan <- odbcConnect('NRSA2')
  visits <- fetchNRSATable(chan, 'tblVISITS2')
  if(is.character(visits)) {
      print(sprintf("Error reading tblVISITS2: %s", visits))
  } else {
      metsPlus <- merge(nrsaMets
                       ,subset(visits
                              ,select=c(UID,SITE_ID,VISIT_NO,DATE_COL)
                              )
                       ,by='UID', all.x=TRUE
                       )
      metsPlus <- metsPlus[c('SITE_ID','VISIT_NO','DATE_COL', 'METRIC', 'RESULT')]
      
      writeNRSACalcResults(metsPlus, 'phabmet_siteids.csv')
  }