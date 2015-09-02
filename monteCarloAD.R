# singleSensitivityAD.R will vary a factor independently until a change in output of
#    a chosen percentage is reached.  Factors come from globalFactors in this analysis.
#setwd("C:/Users/febner/Documents/CourseraDataScience/fracGASM")
source("treatmentClasses.R")
source("treatmentAnaerobicDigestion.R")
source("treatmentLandApplication.R")
library(Hmisc)
#################################################################################
numsamp = 10000
f1 <- Feedstock(type = "GTW",TS = 18,VS = 17 / 18,Bo = 887,TKN = 5600)
g1 <- GlobalFactors()
# Create samples over range of input factors that you want.
# If you don't like them, change them.
# the only constraint is that they must all be the same length.
g1$AD_Digester_utilizationFactor = runif(numsamp,0.48,1.2) #0.84
g1$AD_Digester_CH4Leaks = runif(numsamp,0.01,0.05) #0.03
g1$AD_Digester_CH4incompleteCombustion = runif(numsamp,0.0025,0.0075) #0.005
g1$AD_Digester_conversion_KwHPerM3 = runif(numsamp,2,4) #4.19318820416827
g1$AD_Digester_parasiticLoad = runif(numsamp,0.06,0.18)  #0.12
g1$AD_reductionInVS = runif(numsamp,0.27,0.8) #0.55
g1$AD_Storage_EFresidualMethaneM3CH4PerKgVS = runif(numsamp,0.02,0.06)  #0.043
g1$AD_Storage_IPCC_EF3 = runif(numsamp,0.0025,0.0075)  #0.005
g1$AD_Storage_IPCC_FracGasMS = runif(numsamp,0.12,0.38)  #0.26
g1$AD_LandApplication_FracGasM = runif(numsamp,0.1,0.3)  #0.2
g1$AD_LandApplication_EF1 = runif(numsamp,0.006,0.0175)  #0.0125
g1$AD_LandApplication_OtherNFactor = runif(numsamp,0.01,0.03)  #0.02
g1$AD_LandApp_NAvailabiltiy_Factor = runif(numsamp,0.2,0.6)  #0.4
g1$AD_DisplacedFertilizer_Production_Factor = runif(numsamp,-9,-3) #-6.8
g1$AD_DisplacedFertilizer_Direct_Indirect = runif(numsamp,-7.5,-2.5)  #-5.4
g1$xportToField = runif(numsamp,15,25)  #20

out <- AnaerobicDigestionTreatmentPathway(f1, g1, debug = F)
##############################################################################
input <- data.frame(g1$AD_Digester_utilizationFactor,
                 g1$AD_Digester_CH4Leaks,
                 g1$AD_Digester_CH4incompleteCombustion,
                 g1$AD_Digester_conversion_KwHPerM3,
                 g1$AD_Digester_parasiticLoad,
                 g1$AD_reductionInVS,
                 g1$AD_Storage_EFresidualMethaneM3CH4PerKgVS,
                 g1$AD_Storage_IPCC_EF3,
                 g1$AD_Storage_IPCC_FracGasMS,
                 g1$AD_LandApplication_FracGasM,
                 g1$AD_LandApplication_EF1,
                 g1$AD_LandApplication_OtherNFactor,
                 g1$AD_LandApp_NAvailabiltiy_Factor,
                 g1$AD_DisplacedFertilizer_Production_Factor,
                 g1$AD_DisplacedFertilizer_Direct_Indirect,
                 g1$xportToField)
dev.off() 
hist.data.frame(input)    
hist.data.frame(out)