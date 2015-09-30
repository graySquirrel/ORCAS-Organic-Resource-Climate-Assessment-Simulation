source("treatmentClasses.R")
source("treatmentAnaerobicDigestion.R")
source("treatmentLandApplication.R")
source("treatmentcompost.R")
source("treatmentLandfill.R")
source("parseGlobalFactors.R")
source("baselines.R")

# o is in the env when you source baselines.R# ADLM <- lm(I(unlist(o$AD[1])) ~ I(o$f1$Bo/500) + I(o$f1$TS) + 
#                  I(o$f1$VS) + I(o$f1$TKN/20000)))
y<-unlist(o$AD[1])
names(y) <- o$f1$type
TKN<-o$f1$TKN
Lo<-o$f1$Lo
TVS<-o$f1$TVS
InitialC<-o$f1$InitialC
fdeg <- o$f1$fdeg
npert <- o$f1$Nperton
ADLM <- lm(unlist(o$AD[1]) ~ Lo + TVS + TKN + InitialC + InitialC : fdeg) # perfect fit!
summary(ADLM)
#anova(ADLM)
par(mfrow=c(2,2))
plot(ADLM)
coef(ADLM)
# > coef(ADLM)
# (Intercept)            Lo           TVS           TKN      InitialC InitialC:fdeg 
# 19.02000000   -1.11405959  455.86800000    0.00748218   -1.46666667    1.46666667 

CMLM <- lm(unlist(o$CM[1]) ~ npert + InitialC + InitialC : fdeg)# perfect fit!
summary(CMLM)
par(mfrow=c(2,2))
plot(CMLM)
coef(CMLM)
# > coef(CMLM)
# (Intercept)         npert      InitialC InitialC:fdeg 
# 38.040000      5.954970     -2.133600      2.566667 

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

y<-unlist(o$LF[1])
names(y) <- o$f1$type
LFLM <- lm(y ~ Lo + InitialC : fdeg) # pretty good fit!
summary(LFLM)
par(mfrow=c(2,2))
plot(LFLM, las = 1)
coef(LFLM)
plot(fitted(LFLM))
points(y, cex=2)
# > coef(LFLM)
# (Intercept)            Lo InitialC:fdeg 
# 24.979245      9.280776     -4.099185 