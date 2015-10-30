library(MASS)

source("treatmentClasses.R")
source("treatmentAnaerobicDigestion.R")
source("treatmentLandApplication.R")
source("treatmentAnimalFeed.R")
source("treatmentcompost.R")
source("treatmentLandfill.R")
source("parseGlobalFactors.R")
source("baselineFuncs.R")

o <- calcAllStats()

TKN<-o$b$f1$TKN[2]
Lo<-o$b$f1$Lo[2]
TVS<-o$b$f1$TVS[2]
InitialC<-o$b$f1$InitialC[2]
fdeg <- o$b$f1$fdeg[2]
rdeg <- o$b$f1$rdeg[2]
npert <- o$b$f1$Nperton[2]
Bo <- o$b$f1$Bo[2]
TS <- o$b$f1$TS[2]

# just fit baked goods
y <- as.vector(unlist(o$ADstats$outRanges[,2]))
Lo <- rep(as.vector(Lo),1001)
TVS <- rep(as.vector(TVS),1001)
npert <- rep(as.vector(npert),1001)
InitialC <- rep(as.vector(InitialC),1001)
EFGrid <- as.vector(unlist(o$ADstats$g1$EFGrid))
AD_Digester_CH4Leaks <- as.vector(unlist(o$ADstats$g1$AD_Digester_CH4Leaks))
AD_Digester_conversion_KwHPerM3 <- as.vector(unlist(o$ADstats$g1$AD_Digester_conversion_KwHPerM3))
redVS <- as.vector(unlist(o$ADstats$g1$AD_reductionInVS))
AD_OneMinusreductionInVS <- 1-redVS
AD_MCFactor <- as.vector(unlist(o$ADstats$g1$AD_MCFactor))
AD_flared <- as.vector(unlist(o$ADstats$g1$AD_flared))
parasLd <- as.vector(unlist(o$ADstats$g1$AD_Digester_parasiticLoad))
AD_Digester_OneMinusparasiticLoad <- 1-parasLd
resMeth <- as.vector(unlist(o$ADstats$g1$AD_Storage_EFresidualMethaneM3CH4PerKgVS))
AD_Storage_IPCC_EF3 <- as.vector(unlist(o$ADstats$g1$AD_Storage_IPCC_EF3))
AD_Storage_IPCC_FracGasMS <- as.vector(unlist(o$ADstats$g1$AD_Storage_IPCC_FracGasMS))
DieselspreadLpertkm <- as.vector(unlist(o$ADstats$g1$DieselspreadLpertkm))
AD_xportTofield <- as.vector(unlist(o$ADstats$g1$AD_xportTofield))
LandApplication_EF1 <- as.vector(unlist(o$ADstats$g1$LandApplication_EF1))
LandApplication_FracGasM <- as.vector(unlist(o$ADstats$g1$LandApplication_FracGasM))
AD_CSfactor <- as.vector(unlist(o$ADstats$g1$AD_CSfactor))
navail <- as.vector(unlist(o$ADstats$g1$N_availabilityfactor))
ladis <- as.vector(unlist(o$ADstats$g1$LA_DisplacedFertilizer_Production_Factor))
AD_Storage_EFresidualMethaneM3CH4PerKgVS <- as.vector(unlist(o$ADstats$g1$AD_Storage_EFresidualMethaneM3CH4PerKgVS))

#ADLM <- lm(y ~ Lo * mcfac * CH4Leaks * digconv * flare * parasLd + redVS + gasms + ef3 + resMeth * redVS + Lo * mcfac * csfac)


# all constants
# AD_MCFactor 
# AD_MCFactor : AD_Digester_CH4Leaks 
# AD_MCFactor : AD_flared
# AD_MCFactor : AD_Digester_conversion_KwHPerM3 : AD_Digester_OneMinusparasiticLoad : EFGrid
# AD_MCFactor : AD_Digester_conversion_KwHPerM3 : AD_Digester_OneMinusparasiticLoad : EFGrid : AD_Digester_CH4Leaks
# AD_MCFactor : AD_Digester_conversion_KwHPerM3 : AD_Digester_OneMinusparasiticLoad : EFGrid : AD_flared
# AD_MCFactor : AD_CSfactor
# DieselspreadLpertkm : AD_xportTofield
# AD_Storage_IPCC_EF3
# AD_Storage_IPCC_FracGasMS
# LandApplication_EF1
# AD_Storage_IPCC_EF3 : LandApplication_EF1
# AD_Storage_IPCC_FracGasMS : LandApplication_EF1
# AD_Storage_IPCC_EF3 : LandApplication_FracGasM
# AD_Storage_IPCC_FracGasMS : LandApplication_FracGasM
# LandApplication_FracGasM
# AD_CSfactor
# AD_OneMinusreductionInVS : AD_Storage_EFresidualMethaneM3CH4PerKgVS : AD_CSfactor
# AD_OneMinusreductionInVS : AD_Storage_EFresidualMethaneM3CH4PerKgVS

ADLM <- lm(y ~ AD_MCFactor : AD_Digester_CH4Leaks + 
               AD_MCFactor : AD_flared + 
               AD_MCFactor : AD_Digester_conversion_KwHPerM3 : AD_Digester_OneMinusparasiticLoad : EFGrid +
               AD_MCFactor : AD_Digester_conversion_KwHPerM3 : AD_Digester_OneMinusparasiticLoad : EFGrid : AD_Digester_CH4Leaks +
               AD_MCFactor : AD_Digester_conversion_KwHPerM3 : AD_Digester_OneMinusparasiticLoad : EFGrid : AD_flared +
               AD_MCFactor : AD_CSfactor + 
               DieselspreadLpertkm : AD_xportTofield + 
               AD_Storage_IPCC_EF3 + 
               AD_Storage_IPCC_FracGasMS + 
               LandApplication_EF1 +
               AD_Storage_IPCC_EF3 : LandApplication_EF1 + 
               AD_Storage_IPCC_FracGasMS : LandApplication_EF1 + 
               AD_Storage_IPCC_EF3 : LandApplication_FracGasM +
               AD_Storage_IPCC_FracGasMS : LandApplication_FracGasM + 
               LandApplication_FracGasM + 
               AD_CSfactor + 
               AD_OneMinusreductionInVS : AD_Storage_EFresidualMethaneM3CH4PerKgVS : AD_CSfactor + 
               AD_OneMinusreductionInVS : AD_Storage_EFresidualMethaneM3CH4PerKgVS)

summary(ADLM)
par(mfrow=c(2,2))
plot(ADLM)
coef(ADLM)

# removed low impact Lo multipliers
ADLMSimp <- lm(y ~ AD_MCFactor : AD_Digester_CH4Leaks + 
                   AD_MCFactor : AD_Digester_conversion_KwHPerM3 : AD_Digester_OneMinusparasiticLoad : EFGrid +
                   AD_MCFactor : AD_CSfactor + 
                   DieselspreadLpertkm : AD_xportTofield + AD_Storage_IPCC_EF3 + 
                   AD_Storage_IPCC_FracGasMS + 
                   LandApplication_EF1 +
                   AD_Storage_IPCC_EF3 : LandApplication_EF1 + 
                   AD_Storage_IPCC_FracGasMS : LandApplication_EF1 + 
                   AD_Storage_IPCC_EF3 : LandApplication_FracGasM +
                   AD_Storage_IPCC_FracGasMS : LandApplication_FracGasM + 
                   LandApplication_FracGasM + 
                   AD_CSfactor + 
                   AD_OneMinusreductionInVS : AD_Storage_EFresidualMethaneM3CH4PerKgVS : AD_CSfactor + 
                   AD_OneMinusreductionInVS : AD_Storage_EFresidualMethaneM3CH4PerKgVS)
summary(ADLMSimp)
par(mfrow=c(2,2))
plot(ADLMSimp)
coef(ADLMSimp)

# stepAIC frees up some more low value factors
adlm2 <- stepAIC(ADLMSimp, trace = FALSE)
adlm2$anova

ADLMSimp2 <- lm(y ~ AD_Storage_IPCC_EF3 + 
                    AD_Storage_IPCC_FracGasMS + 
                    LandApplication_EF1 + 
                    LandApplication_FracGasM + AD_CSfactor + 
                    AD_MCFactor:AD_Digester_CH4Leaks + 
                    AD_CSfactor:AD_MCFactor + 
                    DieselspreadLpertkm:AD_xportTofield + 
                    AD_Storage_IPCC_FracGasMS:LandApplication_EF1 + 
                    AD_Storage_IPCC_EF3:LandApplication_FracGasM + 
                    AD_OneMinusreductionInVS:AD_Storage_EFresidualMethaneM3CH4PerKgVS + 
                    AD_MCFactor:AD_Digester_conversion_KwHPerM3:AD_Digester_OneMinusparasiticLoad:EFGrid)

summary(ADLMSimp2)
par(mfrow=c(2,2))
plot(ADLMSimp2)
coef(ADLMSimp2)