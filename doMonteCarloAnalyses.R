# monte carlo analysis for all pathways.
# vary input through getGlobalFactorsFromFile()
# get feedstocks from Feedstock.csv
# just do the calculations here and return processed data to caller
# plotting can be done in R or another application
source("treatmentClasses.R") 
source("treatmentAnaerobicDigestion.R") 
source("treatmentLandApplication.R") 
source("treatmentLandfill.R")
source("treatmentcompost.R")
source("treatmentAnimalFeed.R")
source("parseGlobalFactors.R")
source("baselineFuncs.R")

#################################################################################

#####################################################################
# Do the MC for all pathways, then pick what you want to show.
# ADstats <- calculatePathwayMC(FUN=AnaerobicDigestionTreatmentPathway)
# ADfstats <- calculatePathwayMC(FUN=AnaerobicDigestionTreatmentPathway, Application='Fertilizer')
# LFstats <- calculatePathwayMC(FUN=LandfillTreatmentPathway)
# CMstats <- calculatePathwayMC(FUN=compostTreatmentPathway)
# CMfstats <- calculatePathwayMC(FUN=compostTreatmentPathway, Application = 'Fertilizer')
# CMpstats <- calculatePathwayMC(FUN=compostTreatmentPathway, Application = 'Peat')
# CMbstats <- calculatePathwayMC(FUN=compostTreatmentPathway, Application = 'Blended')
# LAstats <- calculatePathwayMC(FUN=LandApplicationTreatmentPathway)
# LAfstats <- calculatePathwayMC(FUN=LandApplicationTreatmentPathway, Application='Fertilizer')
# AFstats <- calculatePathwayMC(FUN=AnimalFeedTreatmentPathway)
#####################################################################
# Get baselines
# b <- getBaselineResults()
#####################################################################
AllStats <- calcAllStats()
print(createPathwaysPlot(FALSE,s=AllStats))
print(createPathwaysPlot(TRUE,s=AllStats))

r <- read.csv(file="GlobalFactorsCompostsensitivity.csv",stringsAsFactors = FALSE)
All2 <- calcAllStats(GFmemfile = r)
print(createPathwaysCMPlot(TRUE,s=All2))

r <- read.csv(file="GlobalFactorsADsensitivity.csv",stringsAsFactors = FALSE)
All3 <- calcAllStats(GFmemfile = r)
print(createPathwaysPlot(TRUE,s=All3))

stop()
#####################################################################
# sensitivity analysis...  start slow

par(mfrow=c(2,3)) # how many do you want to show?  rows x columns number of graphs
plotFS(AllStats$ADstats,"Food waste","AD_Digester_CH4Leaks","CH4 Leaks","AD")
plotFS(AllStats$ADstats,"Food waste","AD_MCFactor","utilization Factor","AD")
plotFS(AllStats$ADstats,"Food waste","AD_Digester_parasiticLoad","parastic load","AD")
plotFS(AllStats$ADstats,"Food waste","AD_reductionInVS","reduct in VS","AD")
plotFS(AllStats$ADstats,"Food waste","AD_Storage_EFresidualMethaneM3CH4PerKgVS","storage resid methane","AD")
plotFS(AllStats$ADstats,"Food waste","AD_xportTofield","xportToField","AD")


par(mfrow=c(2,3)) # how many do you want to show?  rows x columns number of graphs
plotFS(AllStats$ADfstats,"Baked goods","AD_Digester_CH4Leaks","CH4 Leaks","ADf")
plotFS(AllStats$ADfstats,"Baked goods","AD_MCFactor","utilization Factor","ADf")
plotFS(AllStats$ADfstats,"Baked goods","AD_Digester_parasiticLoad","parastic load","ADf")
plotFS(AllStats$ADfstats,"Baked goods","AD_reductionInVS","reduct in VS","ADf")
plotFS(AllStats$ADfstats,"Baked goods","AD_Storage_EFresidualMethaneM3CH4PerKgVS","storage resid methane","ADf")
plotFS(AllStats$ADfstats,"Baked goods","AD_xportTofield","xportToField","ADf")

par(mfrow=c(2,3)) # how many do you want to show?  rows x columns number of graphs
plotFS(AllStats$CMstats,"Unsweetened dry goods","Compost_dieseLlpert","diesel","CM")
plotFS(AllStats$CMstats,"Unsweetened dry goods","CompostPercentCdegraded","percent C degraded","CM")
plotFS(AllStats$CMstats,"Unsweetened dry goods","Compost_degradedC_CH4","degraded C CH4","CM")
plotFS(AllStats$CMstats,"Unsweetened dry goods","Compost_N2OperN","N20 per N","CM")
plotFS(AllStats$CMstats,"Unsweetened dry goods","Compost_CS_factor","storage factor","CM")
plotFS(AllStats$CMstats,"Unsweetened dry goods","Compost_N_loss","compost N remaining","CM")

par(mfrow=c(1,3)) # how many do you want to show?  rows x columns number of graphs
plotFS(AllStats$LFstats,"Sweet dry goods","LFDieseluseLpert","diesel","LF")
plotFS(AllStats$LFstats,"Sweet dry goods","Landfill_Oxidation_Factor","ox factor","LF")
plotFS(AllStats$LFstats,"Sweet dry goods","LCEMax","LCEMax","LF")
