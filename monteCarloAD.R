# singleSensitivityAD.R will vary a factor independently until a change in output of
#    a chosen percentage is reached.  Factors come from globalFactors in this analysis.
#setwd("C:/Users/febner/Documents/CourseraDataScience/fracGASM")
source("treatmentClasses.R")
source("treatmentAnaerobicDigestion.R")
source("treatmentLandApplication.R")
source("parseGlobalFactors.R")
library(Hmisc)
#################################################################################
set.seed(1234)
#numsamp = 1001
stocks <- read.csv(file="Feedstock.csv",sep = ",",stringsAsFactors=FALSE)
g1 <- getGlobalFactorsFromFile()
# g1 <- GlobalFactors()
# # Create samples over range of input factors that you want.
# # If you don't like them, change them.
# # the only constraint is that they must all be the same length.
# g1$AD_Digester_utilizationFactor = runif(numsamp,0.7, 0.9) #0.84
# g1$AD_Digester_CH4Leaks = runif(numsamp,0.0,0.1) #0.03
# #g1$AD_Digester_CH4incompleteCombustion = runif(numsamp,0.0025,0.0075) #0.005
# g1$AD_Digester_conversion_KwHPerM3 = runif(numsamp,3.7,4.5) #4.19318820416827
# g1$AD_Digester_parasiticLoad = runif(numsamp,0.1,0.2)  #0.12
# g1$AD_reductionInVS = runif(numsamp,0.45,0.65) #0.55
# g1$AD_Storage_EFresidualMethaneM3CH4PerKgVS = runif(numsamp,0.004,0.06)  #0.043
# g1$AD_Storage_IPCC_EF3 = runif(numsamp,0.0,0.01)  #0.005
# g1$AD_Storage_IPCC_FracGasMS = runif(numsamp,0.15,0.45)  #0.26
# g1$LandApplication_FracGasM = runif(numsamp,0.0,0.8)  #0.2
# g1$LandApplication_EF1 = runif(numsamp,0.003,0.02)  #0.0125
# #g1$AD_LandApplication_OtherNFactor = runif(numsamp,0.01,0.03)  #0.02
# g1$LandApp_NAvailabiltiy_Factor = runif(numsamp,0.2,0.6)  #0.4
# g1$LA_DisplacedFertilizer_Production_Factor = runif(numsamp,-9,-3) #-6.8
# g1$LA_DisplacedFertilizer_Direct_Indirect = runif(numsamp,-7.5,-2.5)  #-5.4
# g1$xportToField = runif(numsamp,15,25)  #20
outRanges <- NULL
input<-NULL
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
    ##############################################################################
    input <- data.frame(g1$AD_Digester_utilizationFactor,
                        g1$AD_Digester_CH4Leaks,
                        g1$AD_Digester_parasiticLoad,
                        g1$AD_reductionInVS,
                        g1$AD_Storage_EFresidualMethaneM3CH4PerKgVS,
                        g1$xportToField)
    q <- outOfInterest$ADnetEmissions
        #quantile(outOfInterest$ADnetEmissions,probs = c(0.025,0.5,0.975))
    outRanges <- cbind(outRanges, q)
}
#outRanges <- outRanges[,order(outRanges[2,])]
colnames(outRanges) <- stocks$Feedstock
thestats <- boxplot(outRanges,plot=FALSE)
outRanges <- outRanges[,order(thestats$stats[3,])]
par(mar = c(12,5,2,1))
par(mfrow=c(1,1))
boxplot(outRanges,main="Anaerobic digestion for different feedstocks",las=2)
# print(thestats)
par(mar = c(5.1, 4.1, 4.1, 2.1))
par(mfrow=c(2,3))
#hist.data.frame(input)    
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