# monte carlo analysis for all pathways.
# vary input through getGlobalFactorsFromFile()
# get feedstocks from Feedstock.csv
# just do the calculations here and return processed data to caller
# plotting can be done in R or another application
source("treatmentClasses.R")
source("treatmentAnaerobicDigestion.R")
source("treatmentLandApplication.R")
source("treatmentcompost.R")
source("treatmentLandfill.R")
source("parseGlobalFactors.R")
source("baselines.R")
#################################################################################
calculatePathwayMC <- function(feedstockfile="Feedstock.csv",
                             factorFile="Globalfactors.csv",
                             FUN=NULL,
                             Application=NULL) {
    stocks <- read.csv(file=feedstockfile,sep = ",",stringsAsFactors=FALSE)
    g1 <- getGlobalFactorsFromFile(file = factorFile)
    outRanges <- NULL
    for(i in 1:length(stocks$Feedstock)) {
        #print(i)
        f1 <- Feedstock(type=stocks[i,]$Feedstock,TS=stocks[i,]$TS,VS=stocks[i,]$VS,
                        Bo=stocks[i,]$Bo,TKN=stocks[i,]$TKN,
                        percentCarboTS = stocks[i,]$PercentCarboTS, 
                        percentLipidTS = stocks[i,]$PercentlipidTS,
                        percentProteinTS = stocks[i,]$PercentproteinTS, 
                        fdeg = stocks[i,]$fdeg)
        if(length(Application)!=0) {
          out <- FUN(f1, g1,Application=Application)
        } else {
          out <- FUN(f1, g1)
        }
        q <- out[,1]
        outRanges <- cbind(outRanges, q)
    }
    colnames(outRanges) <- stocks$Feedstock
    bpstats <- boxplot(outRanges,plot=FALSE)
    confDat <- apply(outRanges,2,function(x) quantile(x,probs=c(.025,.5,.975)))
    #confDat <- apply(outRanges,2,function(x) quantile(x,probs=c(0,.5,1)))
    o<-NULL
    o$confDat <- confDat
    o$bpstats <- bpstats
    o$outRanges <- outRanges
    o$f1 <- stocks
    o$g1 <- g1
    o
}
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
#####################################################################
# Get baselines
b <- getBaselineResults()
#####################################################################
massageDataforPlot <- function(in1,in2,treat) {
    df <-data.frame(t(in1))
    df <- cbind(rownames(df),df,in2)
    colnames(df)<- c("feedstock","lo","Median","hi","Nominal")
    df$treatment <- treat
    df$feedstock <- factor(df$feedstock)
    df
}
# Now do the dirty work of plotting
library(ggplot2)
y1 <- massageDataforPlot(ADstats$confDat, b$AD$ADnetEmissions,"AD")
y1f <- massageDataforPlot(ADfstats$confDat, b$ADf$ADnetEmissions,"ADf")
y2 <- massageDataforPlot(LFstats$confDat, b$LF$LandfillNetEmissions,"LF")
y3 <- massageDataforPlot(CMstats$confDat, b$CM$final,"CM")
y3f <- massageDataforPlot(CMfstats$confDat, b$CMf$final,"CMf")
y3p <- massageDataforPlot(CMpstats$confDat, b$CMp$final,"CMp")
y3b <- massageDataforPlot(CMbstats$confDat, b$CMb$final,"CMb")
y4 <- massageDataforPlot(LAstats$confDat, b$LA$EMNetLandapp,"LA")
y4f <- massageDataforPlot(LAfstats$confDat, b$LAf$EMNetLandapp,"LAf")

#y <- rbind(y1,y1f,y2,y3,y3f,y3p,y4,y4f)
y <- rbind(y1,y2,y3,y4)
#y <- rbind(y1f,y2,y3f,y4f)
#y <- rbind(y1,y3,y4)

y$feedstock <- factor(y$feedstock, levels=y$feedstock[order(y2$Nominal)]) # order by LF

# Plot Nominal values
p1 <- ggplot(y, aes(x=feedstock, y=Nominal,fill=treatment)) + 
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin=lo, ymax=hi), width=.3, position=position_dodge(0.9)) +
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5,size=16))

# Plot Nominals without ranges.
p2 <- ggplot(y, aes(x=feedstock, y=Nominal,fill=treatment)) + 
    geom_bar(position=position_dodge(), stat="identity") +
    #geom_errorbar(aes(ymin=lo, ymax=hi), width=.3, position=position_dodge(0.9)) +
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5,size=16))
print(p1)
print(p2)

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
