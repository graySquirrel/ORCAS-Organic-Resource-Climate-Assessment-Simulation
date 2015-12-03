
source("treatmentClasses.R")
source("treatmentAnaerobicDigestion.R")
source("treatmentLandApplication.R")
source("treatmentcompost.R")
source("treatmentLandfill.R")
source("parseGlobalFactors.R")
source("baselines.R")

# o is in the env when you source baselines.R# ADLM <- lm(I(unlist(o$AD[1])) ~ I(o$f1$Bo/500) + I(o$f1$TS) + 
#                  I(o$f1$VS) + I(o$f1$TKN/20000)))

TKN<-o$f1$TKN
TDN<-o$f1$TDN
Lo<-o$f1$Lo
TVS<-o$f1$TVS
InitialC<-o$f1$InitialC
fdeg <- o$f1$fdeg
rdeg <- o$f1$rdeg
npert <- o$f1$Nperton
Bo <- o$f1$Bo
TS <- o$f1$TS
percentProteinTS <- o$f1$percentProteinTS
percentLipidTS <- o$f1$percentLipidTS
percentCarboTS <- o$f1$percentCarboTS

# scaling is done by dividing the columns of x by the root mean square of the vector
# this is the default scaling algorithm in the r scale function
scaledTKN <- (TKN - min(TKN))/(max(TKN)-min(TKN))
scaledLo <- (Lo - min(Lo))/(max(Lo)-min(Lo))
scaledTS <- (TS - min(TS))/(max(TS)-min(TS))
scalednpert <- (npert - min(npert))/(max(npert)-min(npert))
scaledTDN <- (TDN - min(TDN))/(max(TDN)-min(TDN))
scaledTVS <- (TVS - min(TVS))/(max(TVS)-min(TVS))
scaledInitialC <- (InitialC - min(InitialC))/(max(InitialC)-min(InitialC))
scaledrdeg <- (rdeg - min(rdeg))/(max(rdeg)-min(rdeg))

# scaledTKN<-scale(o$f1$TKN, center = FALSE)
# scaledTDN<-scale(o$f1$TDN, center = FALSE)
# scaledLo<-scale(o$f1$Lo, center = FALSE)
# scaledTVS<-scale(o$f1$TVS, center = FALSE)
# scaledInitialC<-scale(o$f1$InitialC, center = FALSE)
# scaledfdeg <- scale(o$f1$fdeg, center = FALSE)
# scaledrdeg <- scale(o$f1$rdeg, center = FALSE)
# scalednpert <- scale(o$f1$Nperton, center = FALSE)
# scaledTS <- scale(o$f1$TS, center = FALSE)

par(mfrow=c(2,2),oma = c(0, 0, 2, 0))

AFLM <- lm(unlist(o$AF[1]) ~ TS : TDN) # perfect fit!
summary(AFLM)
#plot(AFLM)
coef(AFLM)

TSTDN <- TS * TDN
scaledTS_TDN <- (TSTDN - min(TSTDN))/(max(TSTDN) - min(TSTDN))
AFLMS <- lm(unlist(o$AF[1]) ~ scaledTS_TDN) # perfect fit!
#summary(AFLMS)
#plot(AFLMS)
coef(AFLMS)

# AFLM <- lm(unlist(o$AF[1]) ~ TS ) 
# summary(AFLM)
# #plot(AFLM)
# coef(AFLM)
# # plot(fitted(AFLM),unlist(o$AF[1]),xlab="fit",ylab="Animal Feed Emissions")
# # lines(c(-600,2500),c(-600,2500))
# 
# AFLM <- lm(unlist(o$AF[1]) ~ TS:percentProteinTS + 
#              TS:percentLipidTS + TS:percentCarboTS ) 
# summary(AFLM)
# #plot(AFLM)
# coef(AFLM)
# # plot(fitted(AFLM),unlist(o$AF[1]),xlab="fit",ylab="Animal Feed Emissions")
# # lines(c(-600,2500),c(-600,2500))

# Good Fit 
# ADfLM <- lm(unlist(o$ADf[1]) ~ Lo + TVS + InitialC:rdeg)
# Perfect Fit
ADfLM <- lm(unlist(o$ADf[1]) ~ Lo + TVS + InitialC:fdeg + TKN + InitialC) 
summary(ADfLM)
#plot(ADfLM)
coef(ADfLM)
#plot(fitted(ADfLM),unlist(o$ADf[1]),xlab="fit",ylab="AD Emissions")
#lines(c(-600,2500),c(-600,2500))

# ADfLMS <- lm(unlist(o$ADf[1]) ~ scaledLo + scaledTVS) 
# summary(ADfLMS)
# #plot(ADfLMS)
# coef(ADfLMS)
# plot(fitted(ADfLMS),unlist(o$ADf[1]),xlab="fit",ylab="AD emission")
# lines(c(-600,2500),c(-600,2500))

ADfLMS <- lm(unlist(o$ADf[1]) ~ scaledLo + scaledTVS + scaledInitialC : scaledrdeg)
#ADfLMS <- lm(unlist(o$ADf[1]) ~ scaledLo + scaledTVS)
summary(ADfLMS)
#plot(ADfLMS)
coef(ADfLMS)

# Perfect Fit
CMbLM <- lm(unlist(o$CMb[1]) ~ npert + InitialC )
summary(CMbLM)
#plot(CMbLM)
coef(CMbLM)
#plot(fitted(CMbLM),xlab="Feedstock",ylab="Compost Emissions")
#points(o$CMb[1], cex=2)

CMbLMS <- lm(unlist(o$CMb[1]) ~ scalednpert + scaledInitialC)
#summary(CMbLMS)
#plot(CMbLMS)
coef(CMbLMS)


y<-unlist(o$LF[1])
names(y) <- o$f1$type
# Good Fit
#LFLM <- lm(y ~ Lo + InitialC)
# perfect fit
LFLM <- lm(y ~ Lo + InitialC:rdeg) 
summary(LFLM)
#plot(LFLM, las = 1)
#plot(fitted(LFLM),unlist(o$LF[1]),xlab="fit",ylab="LF emission")
#lines(c(-600,3500),c(-600,3500))
coef(LFLM)
# 
# LFLM <- lm(y ~ TS + InitialC)#:rdeg) # perfect fit!
# summary(LFLM)
# #plot(LFLM, las = 1)
# coef(LFLM)

LFLMS <- lm(unlist(o$LF[1]) ~ scaledLo + scaledInitialC)# : scaledrdeg)
summary(LFLMS)
#plot(LFLMS)
coef(LFLMS)


# plot(fitted(AFLM),xlab="Feedstock",ylab="Animal Feed Emissions")
# points(o$AF[1], cex=2)
# plot(fitted(ADfLM),xlab="Feedstock",ylab="Anaerobic Digestion Emissions")
# points(o$ADf[1], cex=2)
# plot(fitted(CMbLM),xlab="Feedstock",ylab="Compost Emissions")
# points(o$CMb[1], cex=2)
# plot(fitted(LFLM),xlab="Feedstock",ylab="Landfill Emissions")
# points(y, cex=2)
# mtext("Linear Models fit vs. actual", outer = TRUE, cex = 1.5)

plot(fitted(AFLM),unlist(o$AF[1]),xlab="fit",ylab="Animal Feed Emissions")
lines(c(-600,2500),c(-600,2500))
plot(fitted(ADfLM),unlist(o$ADf[1]),xlab="fit",ylab="Anaerobic Digestion Emissions")
lines(c(-600,2500),c(-600,2500))
plot(fitted(CMbLM),unlist(o$CMb[1]),xlab="fit",ylab="Compost Emissions")
lines(c(-600,2500),c(-600,2500))
plot(fitted(LFLM),unlist(y),xlab="fit",ylab="Landfill Emissions")
lines(c(-600,3500),c(-600,3500))
mtext("Linear Models fit vs. actual", outer = TRUE, cex = 1.5)

