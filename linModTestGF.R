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
EFresid <- rep(as.vector(unlist(o$ADstats$g1$AD_Storage_EFresidualMethaneM3CH4PerKgVS)),11)

ADLM <- lm(y ~ Lo + TVS + npert + InitialC + EFGrid + CH4Leaks + EFresid)
summary(ADLM)
par(mfrow=c(2,2))
plot(ADLM)
coef(ADLM)
