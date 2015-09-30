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
Lo<-o$f1$Lo
TVS<-o$f1$TVS
InitialC<-o$f1$InitialC
fdeg <- o$f1$fdeg
npert <- o$f1$Nperton

# scaling is done by dividing the columns of x by the root mean square of the vector
# this is the default scaling algorithm in the r scale function
scaledTKN<-scale(o$f1$TKN, center = FALSE)
scaledLo<-scale(o$f1$Lo, center = FALSE)
scaledTVS<-scale(o$f1$TVS, center = FALSE)
scaledInitialC<-scale(o$f1$InitialC, center = FALSE)
scaledfdeg <- scale(o$f1$fdeg, center = FALSE)
scalednpert <- scale(o$f1$Nperton, center = FALSE)

ADLM <- lm(unlist(o$AD[1]) ~ Lo + TVS + TKN + InitialC + InitialC : fdeg) # perfect fit!
summary(ADLM)
#anova(ADLM)
par(mfrow=c(2,2))
plot(ADLM)
coef(ADLM)
# > coef(ADLM)
# (Intercept)            Lo           TVS           TKN      InitialC InitialC:fdeg 
# 19.02000000   -1.11405959  455.86800000    0.00748218   -1.46666667    1.46666667 
ADLMS <- lm(unlist(o$AD[1]) ~ scaledLo + scaledTVS + scaledTKN + 
               scaledInitialC + scaledInitialC : scaledfdeg)
summary(ADLMS)
plot(ADLMS)
coef(ADLMS)
# > coef(ADLMS)
# (Intercept)                  scaledLo                 scaledTVS 
# 19.02000                -235.86082                 244.90025 
# scaledTKN            scaledInitialC scaledInitialC:scaledfdeg 
# 79.48989                -374.17586                 340.17093

CMLM <- lm(unlist(o$CM[1]) ~ npert + InitialC + InitialC : fdeg)# perfect fit!
summary(CMLM)
par(mfrow=c(2,2))
plot(CMLM)
coef(CMLM)
# > coef(CMLM)
# (Intercept)         npert      InitialC InitialC:fdeg 
# 38.040000      5.954970     -2.133600      2.566667 
CMLMS <- lm(unlist(o$CM[1]) ~ scalednpert + scaledInitialC + scaledInitialC : scaledfdeg)
summary(CMLMS)
plot(CMLMS)
coef(CMLMS)
# > coef(CMLMS)
# (Intercept)               scalednpert            scaledInitialC 
# 38.04000                  63.26497                -544.32383 
# scaledInitialC:scaledfdeg 
# 595.29912 

y<-unlist(o$LA[1])
names(y) <- o$f1$type
LALM <- lm(y ~ TKN + InitialC + InitialC: fdeg)# perfect fit!
summary(LALM)
par(mfrow=c(2,2))
plot(LALM)
coef(LALM)
# > coef(LALM)
# (Intercept)           TKN      InitialC InitialC:fdeg 
# 19.020000000   0.006038214  -2.566666667   2.566666667 
LALMS <- lm(unlist(o$LA[1]) ~ scaledTKN + scaledInitialC + scaledInitialC : scaledfdeg)
summary(LALMS)
plot(LALMS)
coef(LALMS)
# > coef(LALMS)
# (Intercept)                 scaledTKN            scaledInitialC 
# 19.02000                  64.14935                -654.80775 
# scaledInitialC:scaledfdeg 
# 595.29912 

y<-unlist(o$LF[1])
names(y) <- o$f1$type
LFLM <- lm(y ~ Lo + InitialC + InitialC : fdeg) # perfect fit!
summary(LFLM)
par(mfrow=c(2,2))
plot(LFLM, las = 1)
coef(LFLM)
plot(fitted(LFLM))
points(y, cex=2)
# > coef(LFLM)
# (Intercept)            Lo      InitialC InitialC:fdeg 
# 18.481100      5.756141     -3.666667      3.666667 
LFLMS <- lm(unlist(o$LF[1]) ~ scaledLo + scaledInitialC + scaledInitialC : scaledfdeg)
summary(LFLMS)
plot(LFLMS)
coef(LFLMS)
plot(fitted(LFLMS))
points(y, cex=2)
# > coef(LFLMS)
# (Intercept)                  scaledLo            scaledInitialC 
# 18.4811                 1218.6494                 -935.4396 
# scaledInitialC:scaledfdeg 
# 850.4273 


