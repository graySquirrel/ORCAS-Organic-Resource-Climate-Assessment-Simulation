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

TKN<-o$b$f1$TKN
Lo<-o$b$f1$Lo
TVS<-o$b$f1$TVS
InitialC<-o$b$f1$InitialC
fdeg <- o$b$f1$fdeg
rdeg <- o$b$f1$rdeg
npert <- o$b$f1$Nperton
Bo <- o$b$f1$Bo
TS <- o$b$f1$TS

y <- as.vector(unlist(o$ADstats$outRanges))
Lo <- as.vector(matrix(Lo,nrow=1001,ncol=11))
TVS <- as.vector(matrix(TVS,nrow=1001,ncol=11))
npert <- as.vector(matrix(npert,nrow=1001,ncol=11))
InitialC <- as.vector(matrix(InitialC,nrow=1001,ncol=11))
EFGrid <- rep(as.vector(unlist(o$ADstats$g1$EFGrid)),11)
CH4Leaks <- rep(as.vector(unlist(o$ADstats$g1$AD_Digester_CH4Leaks)),11)
digconv <- rep(as.vector(unlist(o$ADstats$g1$AD_Digester_conversion_KwHPerM3)),11)
redVS <- rep(as.vector(unlist(o$ADstats$g1$AD_reductionInVS)),11)
mcfac <- rep(as.vector(unlist(o$ADstats$g1$AD_MCFactor)),11)
flare <- rep(as.vector(unlist(o$ADstats$g1$AD_flared)),11)
parasLd <- rep(as.vector(unlist(o$ADstats$g1$AD_Digester_parasiticLoad)),11)
resMeth <- rep(as.vector(unlist(o$ADstats$g1$AD_Storage_EFresidualMethaneM3CH4PerKgVS)),11)
ef3 <- rep(as.vector(unlist(o$ADstats$g1$AD_Storage_IPCC_EF3)),11)
gasms <- rep(as.vector(unlist(o$ADstats$g1$AD_Storage_IPCC_FracGasMS)),11)
spread <- rep(as.vector(unlist(o$ADstats$g1$DieselspreadLpertkm)),11)
xprt <- rep(as.vector(unlist(o$ADstats$g1$AD_xportTofield)),11)
laef1 <- rep(as.vector(unlist(o$ADstats$g1$LandApplication_EF1)),11)
lagasm <- rep(as.vector(unlist(o$ADstats$g1$LandApplication_FracGasM)),11)
csfac <- rep(as.vector(unlist(o$ADstats$g1$AD_CSfactor)),11)
navail <- rep(as.vector(unlist(o$ADstats$g1$N_availabilityfactor)),11)
ladis <- rep(as.vector(unlist(o$ADstats$g1$LA_DisplacedFertilizer_Production_Factor)),11)
#ADLM <- lm(y ~ Lo * EFGrid + Lo * CH4Leaks + TVS + npert + InitialC + digconv + redVS)
#ADLM <- lm(y ~ TVS + npert + InitialC + Lo + EFGrid + CH4Leaks + digconv + redVS + mcfac + flare + parasLd + resMeth + ef3 + gasms + spread + xprt + laef1 + lagasm + csfac + navail + ladis)
#ADLM <- lm(y ~ TVS + npert + InitialC + Lo * EFGrid + CH4Leaks * digconv * redVS * mcfac + flare * parasLd * resMeth * ef3 + gasms * spread * xprt * laef1 + lagasm * csfac * navail * ladis)
#ADLM <- lm(y ~ EFGrid + CH4Leaks + digconv + redVS + mcfac + flare + parasLd + resMeth + ef3 + gasms + spread + xprt + laef1 + lagasm + csfac)
ADLM <- lm(y ~ EFGrid + CH4Leaks + digconv + redVS + mcfac + flare + parasLd + resMeth + ef3 + gasms + spread + xprt + laef1 + lagasm + csfac + CH4Leaks:mcfac + digconv:mcfac + gasms:laef1)
summary(ADLM)
par(mfrow=c(2,2))
plot(ADLM)
coef(ADLM)

adlm2 <- stepAIC(ADLM, trace = FALSE)
adlm2$anova