# monte carlo analysis for Anaerobic Digestion.
# vary input through getGlobalFactorsFromFile()
# get feedstocks from Feedstock.csv
# plot boxplots of emissions vs. feedstock ordered by median output
# and plot regression lines of emissions vs. input parameters to see sensitivity
#     to inputs
library(Hmisc)
source("treatmentClasses.R")
source("treatmentAnaerobicDigestion.R")
source("treatmentLandApplication.R")
source("parseGlobalFactors.R")
#################################################################################
set.seed(1234)
stocks <- read.csv(file="Feedstock.csv",sep = ",",stringsAsFactors=FALSE)
g1 <- getGlobalFactorsFromFile()
outRanges <- NULL
for(i in 1:length(stocks$Feedstock)) {
    #print(i)
    f1 <- Feedstock(type=stocks[i,]$Feedstock,TS=stocks[i,]$TS,VS=stocks[i,]$VS,
                    Bo=stocks[i,]$Bo,TKN=stocks[i,]$TKN,
                    percentCarboTS = stocks[i,]$PercentCarboTS, 
                    percentLipidTS = stocks[i,]$PercentlipidTS,
                    percentProteinTS = stocks[i,]$PercentproteinTS, 
                    fdeg = stocks[i,]$fdeg)
    out <- AnaerobicDigestionTreatmentPathway(f1, g1, debug = F)
    outOfInterest <- out[,c(1,2,3,4)]
    q <- outOfInterest$ADnetEmissions
    outRanges <- cbind(outRanges, q)
}
#outRanges <- outRanges[,order(outRanges[2,])]
colnames(outRanges) <- stocks$Feedstock
thestats <- boxplot(outRanges,plot=FALSE)
outRanges <- outRanges[,order(thestats$stats[3,])]
confDat <- apply(outRanges,2,function(x) quantile(x,probs=c(.025,.5,.975)))
par(mar = c(12,5,2,1))
par(mfrow=c(1,1))
#plot(confDat[2,],xaxt='n')
#axis(1, at=1:11, labels=colnames(confDat),las=2)
errbar(x=thestats$names,y=confDat[2,],
       yplus=confDat[3,],yminus = confDat[1,])

boxplot(outRanges,main="Anaerobic digestion for different feedstocks",las=2)
# print(thestats)
par(mar = c(5.1, 4.1, 4.1, 2.1))
par(mfrow=c(2,3))
allEM <- as.vector(outRanges) # Net emissions from all 11 feedstocks
allin <- rep(g1$AD_Digester_utilizationFactor, 11)
plot(allin, allEM,xlab="AD_Digester_utilizationFactor",ylab="Emissions")
abline(lm(allEM ~ allin),col='red')
allin <- rep(g1$AD_Digester_CH4Leaks, 11)
plot(allin, allEM,xlab="AD_Digester_CH4Leaks",ylab="Emissions")
abline(lm(allEM ~ allin),col='red')
allin <- rep(g1$AD_Digester_parasiticLoad, 11)
plot(allin, allEM,xlab="AD_Digester_parasiticLoad",ylab="Emissions")
abline(lm(allEM ~ allin),col='red')
allin <- rep(g1$AD_reductionInVS, 11)
plot(allin, allEM,xlab="AD_reductionInVS",ylab="Emissions")
abline(lm(allEM ~ allin),col='red')
allin <- rep(g1$AD_Storage_EFresidualMethaneM3CH4PerKgVS, 11)
plot(allin, allEM,xlab="AD_Storage_EFresidualMethaneM3CH4PerKgVS",ylab="Emissions")
abline(lm(allEM ~ allin),col='red')
allin <- rep(g1$xportToField, 11)
plot(allin, allEM,xlab="xportToField",ylab="Emissions")
abline(lm(allEM ~ allin),col='red')
# hist.data.frame(outOfInterest)
##############################################################################
# input <- data.frame(g1$AD_Digester_utilizationFactor,
#                     g1$AD_Digester_CH4Leaks,
#                     g1$AD_Digester_parasiticLoad,
#                     g1$AD_reductionInVS,
#                     g1$AD_Storage_EFresidualMethaneM3CH4PerKgVS,
#                     g1$xportToField)
# hist.data.frame(input)    
