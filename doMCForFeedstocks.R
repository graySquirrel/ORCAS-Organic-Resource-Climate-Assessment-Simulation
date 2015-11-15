# monte carlo analysis for all pathways, for varying feedstock parameters
# around nominal values.  For now we keep all the global Factors constant
# at nominal.
# get feedstocks from Feedstock.csv - but it needs the Lo and Hi columns
# 
source("treatmentClasses.R")
source("treatmentAnaerobicDigestion.R")
source("treatmentLandApplication.R")
source("treatmentcompost.R")
source("treatmentLandfill.R")
source("baselines.R")
#################################################################################
calculateFeedstockMC <- function(fsName="Food waste",
                                 feedstockfile="Feedstock.csv",
                                 FUN=NULL,
                                 Application=NULL) {
    stocks <- read.csv(file=feedstockfile,sep = ",",stringsAsFactors=FALSE)
    stockToVary <- stocks[stocks$Feedstock==fsName,] # One at a time
    g1 <- GlobalFactors() # Only using nominal values here
    #print(i)
    f1 <- Feedstock(type=stockToVary$Feedstock,
        TS=runif(n = numSamps, min = stockToVary$TSLo, max = stockToVary$TSHi),
        VS=runif(n = numSamps, min = stockToVary$VSLo, max = stockToVary$VSHi),
        Bo=runif(n = numSamps, min = stockToVary$BoLo, max = stockToVary$BoHi),
        TKN=runif(n = numSamps, min = stockToVary$TKNLo, max = stockToVary$TKNHi),
        percentCarboTS = runif(n = numSamps, min = stockToVary$PcTSLo, max = stockToVary$PcTSHi), 
        percentLipidTS = runif(n = numSamps, min = stockToVary$PlTSLo, max = stockToVary$PlTSHi), 
        percentProteinTS = runif(n = numSamps, min = stockToVary$PpTSLo, max = stockToVary$PpTSHi), 
        fdeg = runif(n = numSamps, min = stockToVary$fdegLo, max = stockToVary$fdegHi)
        )
    if(length(Application)!=0) {
        out <- FUN(f1, g1,Application=Application)
    } else {
        out <- FUN(f1, g1)
    }
    bpstats <- boxplot(out[1],plot=FALSE)
    confDat <- apply(out[1],2,function(x) quantile(x,probs=c(.025,.5,.975)))
    o<-NULL
    o$bpstats <- bpstats
    o$out <- out
    o$f1 <- f1
    o$g1 <- g1
    o
}
#####################################################################
# Do the MC for all pathways, then pick what you want to show.
feedstockName <- "Food waste"
ADstats <- calculateFeedstockMC(fsName=feedstockName,FUN=AnaerobicDigestionTreatmentPathway)
ADfstats <- calculateFeedstockMC(fsName=feedstockName,FUN=AnaerobicDigestionTreatmentPathway, Application='Fertilizer')
LFstats <- calculateFeedstockMC(fsName=feedstockName,FUN=LandfillTreatmentPathway)
CMstats <- calculateFeedstockMC(fsName=feedstockName,FUN=compostTreatmentPathway)
CMfstats <- calculateFeedstockMC(fsName=feedstockName,FUN=compostTreatmentPathway, Application = 'Fertilizer')
CMpstats <- calculateFeedstockMC(fsName=feedstockName,FUN=compostTreatmentPathway, Application = 'Peat')
# LAstats <- calculateFeedstockMC(fsName=feedstockName,FUN=LandApplicationTreatmentPathway)
# LAfstats <- calculateFeedstockMC(fsName=feedstockName,FUN=LandApplicationTreatmentPathway, Application='Fertilizer')
#####################################################################
# Get baselines
b <- getBaselineResults()
#####################################################################
# sensitivity analysis...  start slow
plotFactorSensitivity <- function(theObj,feedstock,stockFactor,treatment,
                                  type="p",add=FALSE,ylim=NULL) {
    y <- unlist(theObj$out[1])
    x <- theObj$f1[[stockFactor]]
    if (!add) {
        plot(x, y, xlab=stockFactor,ylab=paste(feedstock,treatment),type=type,ylim=ylim)
    }
    abline(lm(y ~ x),col='red')
}
par(mfrow=c(3,3)) # how many do you want to show?  rows x columns number of graphs
plotFactorSensitivity(ADstats,feedstockName,"Bo","AD")
plotFactorSensitivity(CMstats,feedstockName,"Bo","CM")
plotFactorSensitivity(ADfstats,feedstockName,"Bo","ADf")
plotFactorSensitivity(LFstats,feedstockName,"Bo","LF")
plotFactorSensitivity(CMfstats,feedstockName,"Bo","CMf")
plotFactorSensitivity(CMpstats,feedstockName,"Bo","CMp")
# plotFactorSensitivity(LAstats,feedstockName,"Bo","LA")
# plotFactorSensitivity(LAfstats,feedstockName,"Bo","LAf")

plotFactorSensitivity(ADstats,feedstockName,"Bo","AD",type="n",ylim=c(10,150))
plotFactorSensitivity(CMstats,feedstockName,"Bo","CM",type="n",add=TRUE)
# plotFactorSensitivity(ADfstats,feedstockName,"Bo","ADf",type="n",add=TRUE)
# plotFactorSensitivity(LFstats,feedstockName,"Bo","LF",type="n",add=TRUE)
# plotFactorSensitivity(CMfstats,feedstockName,"Bo","CMf",type="n",add=TRUE)
# plotFactorSensitivity(CMpstats,feedstockName,"Bo","CMp",type="n",add=TRUE)
# plotFactorSensitivity(LAstats,feedstockName,"Bo","LA",type="n",add=TRUE)
# plotFactorSensitivity(LAfstats,feedstockName,"Bo","LAf",type="n",add=TRUE)

# par(mfrow=c(2,3)) # how many do you want to show?  rows x columns number of graphs
# plotFactorSensitivity(ADstats,feedstockName,"TS","AD")
# plotFactorSensitivity(ADstats,feedstockName,"VS","AD")
# plotFactorSensitivity(ADstats,feedstockName,"Lo","AD")
# plotFactorSensitivity(ADstats,feedstockName,"Bo","AD")
# plotFactorSensitivity(ADstats,feedstockName,"TKN","AD")
# plotFactorSensitivity(ADstats,feedstockName,"TVS","AD")
# 
# par(mfrow=c(2,3)) # how many do you want to show?  rows x columns number of graphs
# plotFactorSensitivity(CMstats,feedstockName,"TS","CM")
# plotFactorSensitivity(CMstats,feedstockName,"VS","CM")
# plotFactorSensitivity(CMstats,feedstockName,"Lo","CM")
# plotFactorSensitivity(CMstats,feedstockName,"Bo","CM")
# plotFactorSensitivity(CMstats,feedstockName,"TKN","CM")
# plotFactorSensitivity(CMstats,feedstockName,"TVS","CM")
# 
# par(mfrow=c(2,3)) # how many do you want to show?  rows x columns number of graphs
# plotFactorSensitivity(LFstats,feedstockName,"TS","LF")
# plotFactorSensitivity(LFstats,feedstockName,"VS","LF")
# plotFactorSensitivity(LFstats,feedstockName,"Lo","LF")
# plotFactorSensitivity(LFstats,feedstockName,"Bo","LF")
# plotFactorSensitivity(LFstats,feedstockName,"TKN","LF")
# plotFactorSensitivity(LFstats,feedstockName,"TVS","LF")


