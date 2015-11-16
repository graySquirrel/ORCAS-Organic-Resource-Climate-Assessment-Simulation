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
scaledLo<-scale(o$f1$Lo, center = FALSE)
scaledTVS<-scale(o$f1$TVS, center = FALSE)
scaledInitialC<-scale(o$f1$InitialC, center = FALSE)
scaledfdeg <- scale(o$f1$fdeg, center = FALSE)
scaledrdeg <- scale(o$f1$rdeg, center = FALSE)
scalednpert <- scale(o$f1$Nperton, center = FALSE)

par(mfrow=c(2,2),oma = c(0, 0, 2, 0))

AFLM <- lm(unlist(o$AF[1]) ~ TS : TDN) # perfect fit!
summary(AFLM)
#plot(AFLM)
coef(AFLM)
plot(fitted(AFLM),xlab="Feedstock",ylab="Animal Feed Emissions")
points(o$AF[1], cex=2)


# print(o$g1$EFGrid)
# ADLM <- lm(unlist(o$AD[1]) ~ Lo + TVS + npert + InitialC)# + InitialC : rdeg) # perfect fit!
# summary(ADLM)
# par(mfrow=c(2,2))
# plot(ADLM)
# coef(ADLM)
# 
# o$g1$EFGrid <- -918
# o$AD  <- AnaerobicDigestionTreatmentPathway(o$f1, o$g1, Application = 'noDisplace')
# ADLM <- lm(unlist(o$AD[1]) ~ Lo + TVS + npert + InitialC)# + InitialC : rdeg) # perfect fit!
# summary(ADLM)
# par(mfrow=c(2,2))
# plot(ADLM)
# coef(ADLM)
# 
# o$g1$EFGrid <- -537
# o$AD  <- AnaerobicDigestionTreatmentPathway(o$f1, o$g1, Application = 'noDisplace')
# ADLM <- lm(unlist(o$AD[1]) ~ Lo + TVS + npert + InitialC)# + InitialC : rdeg) # perfect fit!
# summary(ADLM)
# par(mfrow=c(2,2))
# plot(ADLM)
# coef(ADLM)
#stop()

# ADLM <- lm(unlist(o$AD[1]) ~ Lo + TVS + npert + InitialC)
# summary(ADLM)
# plot(ADLM)
# coef(ADLM)

# ADLMS <- lm(unlist(o$AD[1]) ~ scaledLo + scaledTVS + scalednpert + scaledInitialC)
# summary(ADLMS)
# plot(ADLMS)
# coef(ADLMS)

ADfLM <- lm(unlist(o$ADf[1]) ~ Lo + TVS + npert + InitialC : rdeg) # perfect fit!
summary(ADfLM)
#plot(ADfLM)
coef(ADfLM)
plot(fitted(ADfLM),xlab="Feedstock",ylab="Anaerobic Digestion Emissions")
points(o$ADf[1], cex=2)

# ADfLMS <- lm(unlist(o$ADf[1]) ~ scaledLo + scaledTVS + scaledTKN + scaledInitialC : scaledrdeg)
# summary(ADfLMS)
# plot(ADfLMS)
# coef(ADfLMS)
# plot(fitted(ADfLMS))
# points(o$ADf[1], cex=2)

# CMLM <- lm(unlist(o$CM[1]) ~ npert + InitialC + InitialC : rdeg)# perfect fit! doesn't simplify to InitialC:rdeg
# summary(CMLM)
# par(mfrow=c(2,2))
# plot(CMLM)
# coef(CMLM)
# plot(fitted(CMLM))
# points(o$CM[1], cex=2)

# CMLMS <- lm(unlist(o$CM[1]) ~ scalednpert + scaledInitialC : scaledrdeg)
# summary(CMLMS)
# plot(CMLMS)
# coef(CMLMS)

CMbLM <- lm(unlist(o$CMb[1]) ~ npert + InitialC )
summary(CMbLM)
#plot(CMbLM)
coef(CMbLM)
plot(fitted(CMbLM),xlab="Feedstock",ylab="Compost Emissions")
points(o$CMb[1], cex=2)

# CMbLMS <- lm(unlist(o$CMb[1]) ~ scalednpert + scaledInitialC : scaledrdeg)
# summary(CMbLMS)
# plot(CMbLMS)
# coef(CMbLMS)

# y<-unlist(o$LA[1])
# names(y) <- o$f1$type
# LALM <- lm(y ~ npert + InitialC:rdeg)# perfect fit!
# summary(LALM)
# par(mfrow=c(2,2))
# plot(LALM)
# coef(LALM)
# # > coef(LALM)
# # (Intercept)           TKN      InitialC InitialC:fdeg 
# # 19.020000000   0.006038214  -2.566666667   2.566666667 
# LALMS <- lm(unlist(o$LA[1]) ~ scaledTKN + scaledInitialC + scaledInitialC : scaledfdeg)
# summary(LALMS)
# plot(LALMS)
# coef(LALMS)
# # > coef(LALMS)
# # (Intercept)                 scaledTKN            scaledInitialC 
# # 19.02000                  64.14935                -654.80775 
# # scaledInitialC:scaledfdeg 
# # 595.29912 

y<-unlist(o$LF[1])
names(y) <- o$f1$type
LFLM <- lm(y ~ Lo + InitialC:rdeg) # perfect fit!
summary(LFLM)
#plot(LFLM, las = 1)
coef(LFLM)
plot(fitted(LFLM),xlab="Feedstock",ylab="Landfill Emissions")
points(y, cex=2)

mtext("Linear Model fit vs. actual", outer = TRUE, cex = 1.5)


# > coef(LFLM)
# (Intercept)            Lo      InitialC InitialC:fdeg 
# 18.481100      5.756141     -3.666667      3.666667 
# LFLMS <- lm(unlist(o$LF[1]) ~ scaledLo + scaledInitialC + scaledInitialC : scaledfdeg)
# summary(LFLMS)
# plot(LFLMS)
# coef(LFLMS)
# plot(fitted(LFLMS))
# points(y, cex=2)
# > coef(LFLMS)
# (Intercept)                  scaledLo            scaledInitialC 
# 18.4811                 1218.6494                 -935.4396 
# scaledInitialC:scaledfdeg 
# 850.4273 


