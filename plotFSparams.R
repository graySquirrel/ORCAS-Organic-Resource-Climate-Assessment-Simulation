source("treatmentClasses.R")
source("treatmentAnaerobicDigestion.R")
source("treatmentLandApplication.R")
source("treatmentcompost.R")
source("treatmentLandfill.R")
source("parseGlobalFactors.R")
source("baselines.R")

# o is in the env when you source baselines.R
par(mfrow=c(2,2))
plotplustext <- function(o,x,y,xl,yl) {
    symbols(x=x, y=y, circles=unlist(o$AD[1]/100), 
            inches=1/3, bg="steelblue2", fg=NULL,xlab=xl,ylab=yl)
    text(x=x, y=y-.02,labels = o$f1$type,cex=0.75)
}

plotplustext(o=o,x=o$f1$Bo/500,y=o$f1$TS,xl="Bo/500",yl="TS")
plotplustext(o=o,x=o$f1$VS,y=o$f1$TS,xl="VS",yl="TS")
plotplustext(o=o,x=o$f1$TKN/20000,y=o$f1$TS,xl="TKN/20000",yl="TS")
plotplustext(o=o,x=o$f1$VS,y=o$f1$Bo/500,xl="VS",yl="Bo/500")

mylm <- lm(I(unlist(o$AD[1]) ~ I(o$f1$Bo/500) + I(o$f1$TS) + 
                 I(o$f1$VS) + I(o$f1$TKN/20000)))
