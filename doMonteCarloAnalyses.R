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
makeYforPlot1 <- function(s=NULL) {
    y1 <- massageDataforPlot(s$ADstats$confDat, s$b$AD$ADnetEmissions,"AD")
    y2 <- massageDataforPlot(s$LFstats$confDat, s$b$LF$LandfillNetEmissions,"LF")
    y3Special <- massageDataforPlot(s$CMpstats$confDat, s$b$CMb$final,"CM")
    y5 <- massageDataforPlot(s$AFstats$confDat,s$b$AF$EMAnimalFeed,"AF")
    y <- rbind(y1,y2,y5,y3Special)
    # order by LF
    y$feedstock <- factor(y$feedstock, levels=y$feedstock[order(y2$Emissions)]) 
    y
}
#################################################################################
makeYforPlot2 <- function(s=NULL) {
    y3 <- massageDataforPlot(s$CMstats$confDat, s$b$CM$final,"CM")
    y3f <- massageDataforPlot(s$CMfstats$confDat, s$b$CMf$final,"CMf")
    y3p <- massageDataforPlot(s$CMpstats$confDat, s$b$CMp$final,"CMp")
    y3b <- massageDataforPlot(s$CMbstats$confDat, s$b$CMb$final,"CMb")
    y3la <- massageDataforPlot(s$CMLAstats$confDat, s$b$CMLA$final,"CMLA")
    y <- rbind(y3,y3f, y3p, y3b, y3la) 
    # order by emissions
    y$feedstock <- factor(y$feedstock, levels=y$feedstock[order(y3$Emissions)]) 
    y
}
#################################################################################
makeYforPlot3 <- function(s=NULL) {
    y1 <- massageDataforPlot(s$ADstats$confDat, s$b$AD$ADnetEmissions,"AD")
    y2 <- massageDataforPlot(s$LFstats$confDat, s$b$LF$LandfillNetEmissions,"LF")
    y3 <- massageDataforPlot(s$CMbstats$confDat, s$b$CMb$final,"CM")
    y5 <- massageDataforPlot(s$AFstats$confDat,s$b$AF$EMAnimalFeed,"AF")
    y <- rbind(y1,y2,y5,y3)
    # order by LF
    y$feedstock <- factor(y$feedstock, levels=y$feedstock[order(y2$Emissions)]) 
    y
}
#################################################################################
AllStats <- calcAllStats()
y <- makeYforPlot1(AllStats)
print(makePathwaysPlot(FALSE,y=y,title="nominal emissions"))
print(makePathwaysPlot(TRUE,y=y,title="nominal emissions with 95% bars"))

r <- read.csv(file="GlobalFactorsCompostsensitivity.csv",stringsAsFactors = FALSE)
All2 <- calcAllStats(GFmemfile = r)
y2 <- makeYforPlot2(All2)
print(makePathwaysPlot(TRUE,y=y2,title="compost emissions comparison"))
y4 <- makeYforPlot3(All2)
print(makePathwaysPlot(TRUE,y=y4,title="nominal emissions with Compost Blended case"))

r <- read.csv(file="GlobalFactorsADsensitivity.csv",stringsAsFactors = FALSE)
All3 <- calcAllStats(GFmemfile = r)
y3 <- makeYforPlot1(All3)
print(makePathwaysPlot(TRUE,y=y3,title="nominal emissions with AD sensitivity factors"))

# stop()
# #####################################################################
# # sensitivity analysis...  start slow
# 
# par(mfrow=c(2,3)) # how many do you want to show?  rows x columns number of graphs
# plotFS(AllStats$ADstats,"Food waste","AD_Digester_CH4Leaks","CH4 Leaks","AD")
# plotFS(AllStats$ADstats,"Food waste","AD_MCFactor","utilization Factor","AD")
# plotFS(AllStats$ADstats,"Food waste","AD_Digester_parasiticLoad","parastic load","AD")
# plotFS(AllStats$ADstats,"Food waste","AD_reductionInVS","reduct in VS","AD")
# plotFS(AllStats$ADstats,"Food waste","AD_Storage_EFresidualMethaneM3CH4PerKgVS","storage resid methane","AD")
# plotFS(AllStats$ADstats,"Food waste","AD_xportTofield","xportToField","AD")
# 
# 
# par(mfrow=c(2,3)) # how many do you want to show?  rows x columns number of graphs
# plotFS(AllStats$ADfstats,"Baked goods","AD_Digester_CH4Leaks","CH4 Leaks","ADf")
# plotFS(AllStats$ADfstats,"Baked goods","AD_MCFactor","utilization Factor","ADf")
# plotFS(AllStats$ADfstats,"Baked goods","AD_Digester_parasiticLoad","parastic load","ADf")
# plotFS(AllStats$ADfstats,"Baked goods","AD_reductionInVS","reduct in VS","ADf")
# plotFS(AllStats$ADfstats,"Baked goods","AD_Storage_EFresidualMethaneM3CH4PerKgVS","storage resid methane","ADf")
# plotFS(AllStats$ADfstats,"Baked goods","AD_xportTofield","xportToField","ADf")
# 
# par(mfrow=c(2,3)) # how many do you want to show?  rows x columns number of graphs
# plotFS(AllStats$CMstats,"Unsweetened dry goods","Compost_dieseLlpert","diesel","CM")
# plotFS(AllStats$CMstats,"Unsweetened dry goods","CompostPercentCdegraded","percent C degraded","CM")
# plotFS(AllStats$CMstats,"Unsweetened dry goods","Compost_degradedC_CH4","degraded C CH4","CM")
# plotFS(AllStats$CMstats,"Unsweetened dry goods","Compost_N2OperN","N20 per N","CM")
# plotFS(AllStats$CMstats,"Unsweetened dry goods","Compost_CS_factor","storage factor","CM")
# plotFS(AllStats$CMstats,"Unsweetened dry goods","Compost_N_loss","compost N remaining","CM")
# 
# par(mfrow=c(1,3)) # how many do you want to show?  rows x columns number of graphs
# plotFS(AllStats$LFstats,"Sweet dry goods","LFDieseluseLpert","diesel","LF")
# plotFS(AllStats$LFstats,"Sweet dry goods","Landfill_Oxidation_Factor","ox factor","LF")
# plotFS(AllStats$LFstats,"Sweet dry goods","LCEMax","LCEMax","LF")
