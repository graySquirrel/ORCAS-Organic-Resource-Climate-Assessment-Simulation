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

# scaling is done by dividing the columns of x by the root mean square of the vector
# this is the default scaling algorithm in the r scale function
scaledTKN<-scale(o$f1$TKN, center = FALSE)
scaledTDN<-scale(o$f1$TDN, center = FALSE)
scaledLo<-scale(o$f1$Lo, center = FALSE)
scaledTVS<-scale(o$f1$TVS, center = FALSE)
scaledInitialC<-scale(o$f1$InitialC, center = FALSE)
scaledfdeg <- scale(o$f1$fdeg, center = FALSE)
scaledrdeg <- scale(o$f1$rdeg, center = FALSE)
scalednpert <- scale(o$f1$Nperton, center = FALSE)
scaledTS <- scale(o$f1$TS, center = FALSE)

par(mfrow=c(2,2),oma = c(0, 0, 2, 0))

AFLM <- lm(unlist(o$AF[1]) ~ TS : TDN) # perfect fit!
#summary(AFLM)
#plot(AFLM)
coef(AFLM)

AFLMS <- lm(unlist(o$AF[1]) ~ scaledTS : scaledTDN) # perfect fit!
#summary(AFLMS)
#plot(AFLMS)
coef(AFLMS)
plot(fitted(AFLMS),xlab="Feedstock",ylab="Animal Feed Emissions")
points(o$AF[1], cex=2)

#ADfLM <- lm(unlist(o$ADf[1]) ~ Lo + TVS + InitialC : rdeg) # perfect fit!
ADfLM <- lm(unlist(o$ADf[1]) ~ Lo + TVS) 
summary(ADfLM)
#plot(ADfLM)
coef(ADfLM)

#ADfLMS <- lm(unlist(o$ADf[1]) ~ scaledLo + scaledTVS + scaledInitialC : scaledrdeg)
ADfLMS <- lm(unlist(o$ADf[1]) ~ scaledLo + scaledTVS)
summary(ADfLMS)
#plot(ADfLMS)
coef(ADfLMS)
plot(fitted(ADfLM),xlab="Feedstock",ylab="Anaerobic Digestion Emissions")
points(o$ADf[1], cex=2)

CMbLM <- lm(unlist(o$CMb[1]) ~ npert + InitialC )
#summary(CMbLM)
#plot(CMbLM)
coef(CMbLM)

CMbLMS <- lm(unlist(o$CMb[1]) ~ scalednpert + scaledInitialC)
#summary(CMbLMS)
#plot(CMbLMS)
coef(CMbLMS)
plot(fitted(CMbLM),xlab="Feedstock",ylab="Compost Emissions")
points(o$CMb[1], cex=2)

y<-unlist(o$LF[1])
names(y) <- o$f1$type
LFLM <- lm(y ~ Lo + InitialC)#:rdeg) # perfect fit!
#summary(LFLM)
#plot(LFLM, las = 1)
coef(LFLM)

LFLMS <- lm(unlist(o$LF[1]) ~ scaledLo + scaledInitialC)# : scaledrdeg)
#summary(LFLMS)
#plot(LFLMS)
coef(LFLMS)
plot(fitted(LFLM),xlab="Feedstock",ylab="Landfill Emissions")
points(y, cex=2)


mtext("Linear Model fit vs. actual", outer = TRUE, cex = 1.5)

