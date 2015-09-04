# varyFeedstockAroundNom will vary a feedstock factors systematically to sample
#   a hypercube over the ranges of interest
#setwd("C:/Users/febner/Documents/CourseraDataScience/fracGASM")
source("treatmentClasses.R") 
source("treatmentAnaerobicDigestion.R") 
#source("treatmentLandApplication.R") land app does not use feedstock
source("treatmentLandfill.R")
source("treatmentcompost.R")
###########################################################
createSamples <- function(dims=4,level=1,samps=10,min=0,max=1) {
    rep(0:(samps-1),times=samps^(level-1),each=samps^(dims-level))*((max-min)/(samps-1)) + min
}
###########################################################
plotit <- function(a,x,title,xlab) {
    modelQ1<-lm(a ~ x)
    newx <- seq(min(x), max(x), length.out = 10)
    plot(a ~ x,xlab=xlab,ylab=title, type = 'n')
    asum <- boxplot(a ~ x,plot= FALSE)
    polygon(c(rev(newx), newx), c(rev(asum$stats[5,]), asum$stats[1,]), col = 'grey80', border = NA)
    polygon(c(rev(newx), newx), c(rev(asum$stats[4,]), asum$stats[2,]), col = 'grey65', border = NA)
    polygon(c(rev(newx), newx), c(rev(asum$conf[2,]), asum$conf[1,]), col = 'grey50', border = NA)
    lines(asum$stats[3,]~newx,type='l',lwd=2)
}
###########################################################
####  Change these params to do a diff one.
numsampsPerDimension = 10 # Will create numsampsPerDimension^4 samples for the cube
inputs <- data.frame(createSamples(dims=4,level=1,samps=numsampsPerDimension, min=14,max=22))
inputs <- cbind(inputs, createSamples(dims=4,level=2,samps=numsampsPerDimension, min=13/18, max= 18/18))
inputs <- cbind(inputs, createSamples(dims=4,level=3,samps=numsampsPerDimension, min=750, max=950))
inputs <- cbind(inputs, createSamples(dims=4,level=4,samps=numsampsPerDimension, min=4500, max=6500))
colnames(inputs) <- c("TS","VS","Bo","TKN")

f1 <- Feedstock(type="variability", TS=inputs[,1], VS=inputs[,2], Bo=inputs[,3], TKN=inputs[,4])
g1 <- GlobalFactors()
outAD <- AnaerobicDigestionTreatmentPathway(f1, g1, debug = F)
outLF <- LandfillTreatmentPathway(f1, g1, debug = F)
outCMf <- compostTreatmentPathway(f1, g1, 'Fertilizer', debug = F)
outCMp <- compostTreatmentPathway(f1, g1, 'Peat', debug = F)

if(dev.cur() != 1) dev.off() 
par(mfrow=c(2,2))
plotit(outAD[,1], inputs$TS, "AD Net Emissions", "TS")
plotit(outLF[,1], inputs$TS, "Landfill Net Emissions", "TS")
plotit(outCMf[,1], inputs$TS, "Compost(fert) Net Emissions", "TS")
plotit(outCMp[,1], inputs$TS, "Compost(Peat) Net Emissions", "TS")

# plotit(outAD[,1], inputs$VS, "VS")
# plotit(outAD[,1], inputs$Bo, "Bo")
# plotit(outAD[,1], inputs$TKN, "TKN")
