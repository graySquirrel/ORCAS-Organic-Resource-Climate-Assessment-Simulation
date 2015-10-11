# monte carlo analysis for all pathways.
# vary input through getGlobalFactorsFromFile()
# get feedstocks from Feedstock.csv
# just do the calculations here and return processed data to caller
# plotting can be done in R or another application
source("baselineFuncs.R")

#################################################################################

#####################################################################
# Do the MC for all pathways, then pick what you want to show.
ADstats <- calculatePathwayMC(FUN=AnaerobicDigestionTreatmentPathway)
ADfstats <- calculatePathwayMC(FUN=AnaerobicDigestionTreatmentPathway, Application='Fertilizer')
LFstats <- calculatePathwayMC(FUN=LandfillTreatmentPathway)
CMstats <- calculatePathwayMC(FUN=compostTreatmentPathway)
CMfstats <- calculatePathwayMC(FUN=compostTreatmentPathway, Application = 'Fertilizer')
CMpstats <- calculatePathwayMC(FUN=compostTreatmentPathway, Application = 'Peat')
CMbstats <- calculatePathwayMC(FUN=compostTreatmentPathway, Application = 'Blended')
LAstats <- calculatePathwayMC(FUN=LandApplicationTreatmentPathway)
LAfstats <- calculatePathwayMC(FUN=LandApplicationTreatmentPathway, Application='Fertilizer')
AFstats <- calculatePathwayMC(FUN=AnimalFeedTreatmentPathway)
#####################################################################
# Get baselines
b <- getBaselineResults()
#####################################################################

print(createPathwaysPlot(FALSE))
print(createPathwaysPlot(TRUE))
stop()
#####################################################################
# sensitivity analysis...  start slow
plotFactorSensitivity <- function(theObj,feedstock,rangeFactor,xlabl,treatment) {
    df <- data.frame(theObj$outRanges)
    var <- gsub(" ",".",feedstock)
    y <- df[[var]]
    x <- theObj$g1[[rangeFactor]]
    plot(x, y, xlab=xlabl,ylab=paste(feedstock,treatment))
    abline(lm(y ~ x),col='red')
}
par(mfrow=c(2,3)) # how many do you want to show?  rows x columns number of graphs
plotFactorSensitivity(ADstats,"Food waste","AD_Digester_CH4Leaks","CH4 Leaks","AD")
plotFactorSensitivity(ADstats,"Food waste","AD_MCFactor","utilization Factor","AD")
plotFactorSensitivity(ADstats,"Food waste","AD_Digester_parasiticLoad","parastic load","AD")
plotFactorSensitivity(ADstats,"Food waste","AD_reductionInVS","reduct in VS","AD")
plotFactorSensitivity(ADstats,"Food waste","AD_Storage_EFresidualMethaneM3CH4PerKgVS","storage resid methane","AD")
plotFactorSensitivity(ADstats,"Food waste","AD_xportTofield","xportToField","AD")

par(mfrow=c(2,3)) # how many do you want to show?  rows x columns number of graphs
plotFactorSensitivity(ADfstats,"Baked goods","AD_Digester_CH4Leaks","CH4 Leaks","ADf")
plotFactorSensitivity(ADfstats,"Baked goods","AD_MCFactor","utilization Factor","ADf")
plotFactorSensitivity(ADfstats,"Baked goods","AD_Digester_parasiticLoad","parastic load","ADf")
plotFactorSensitivity(ADfstats,"Baked goods","AD_reductionInVS","reduct in VS","ADf")
plotFactorSensitivity(ADfstats,"Baked goods","AD_Storage_EFresidualMethaneM3CH4PerKgVS","storage resid methane","ADf")
plotFactorSensitivity(ADfstats,"Baked goods","AD_xportTofield","xportToField","ADf")

par(mfrow=c(2,3)) # how many do you want to show?  rows x columns number of graphs
plotFactorSensitivity(CMstats,"Unsweetened dry goods","Compost_dieseLlpert","diesel","CM")
plotFactorSensitivity(CMstats,"Unsweetened dry goods","CompostPercentCdegraded","percent C degraded","CM")
plotFactorSensitivity(CMstats,"Unsweetened dry goods","Compost_degradedC_CH4","degraded C CH4","CM")
plotFactorSensitivity(CMstats,"Unsweetened dry goods","Compost_N2OperN","N20 per N","CM")
plotFactorSensitivity(CMstats,"Unsweetened dry goods","Compost_CS_factor","storage factor","CM")
plotFactorSensitivity(CMstats,"Unsweetened dry goods","Compost_N_loss","compost N remaining","CM")

par(mfrow=c(1,3)) # how many do you want to show?  rows x columns number of graphs
plotFactorSensitivity(LFstats,"Sweet dry goods","LFDieseluseLpert","diesel","LF")
plotFactorSensitivity(LFstats,"Sweet dry goods","Landfill_Oxidation_Factor","ox factor","LF")
plotFactorSensitivity(LFstats,"Sweet dry goods","LCEMax","LCEMax","LF")
