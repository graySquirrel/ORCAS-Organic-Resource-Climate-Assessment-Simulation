source("treatmentClasses.R")
source("treatmentAnaerobicDigestion.R")
source("treatmentLandApplication.R")
source("treatmentcompost.R")
source("treatmentLandfill.R")
source("parseGlobalFactors.R")
source("baselines.R")

# o is in the env when you source baselines.R
plotplustext <- function(labels,path,x,y,xl,yl) {
    symbols(x=x, y=y, circles=path, 
            inches=1/3, bg=rgb(0, 0, 1, 0.5), xlab=xl,ylab=yl)
    text(x=x, y=jitter(y-.02,factor=100),labels = labels,cex=0.75)
}
makeplots <- function(zsize,tit){
    par(mfrow=c(2,2),oma=c(0,0,2,0))
    plotplustext(labels=o$f1$type,path=zsize,x=o$f1$Bo/500,y=o$f1$TS,xl="Bo/500",yl="TS")
    plotplustext(labels=o$f1$type,path=zsize,x=o$f1$VS,y=o$f1$TS,xl="VS",yl="TS")
    plotplustext(labels=o$f1$type,path=zsize,x=o$f1$TKN/20000,y=o$f1$TS,xl="TKN/20000",yl="TS")
    plotplustext(labels=o$f1$type,path=zsize,x=o$f1$VS,y=o$f1$Bo/500,xl="VS",yl="Bo/500")
    title(tit, outer=TRUE)
}
makeplots(unlist(o$AD[1]/100),"AD")
makeplots(unlist(o$CM[1]/100),"CM")
makeplots(unlist(o$LF[1]/100),"LF")
makeplots(unlist(o$LA[1]/100)+2,"LA") # size has to be positive number

