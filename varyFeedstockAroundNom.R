# varyFeedstockAroundNom will vary a feedstock factors systematically to sample
#   a hypercube over the ranges of interest
#setwd("C:/Users/febner/Documents/CourseraDataScience/fracGASM")
source("treatmentClasses.R") 
source("treatmentAnaerobicDigestion.R") 
source("treatmentLandApplication.R") 
###########################################################
createSamples <- function(dims=4,level=1,samps=10,min=0,max=1) {
    rep(0:(samps-1),times=samps^(level-1),each=samps^(dims-level))*((max-min)/(samps-1)) + min
}
###########################################################
####  Change these params to do a diff one.
FUN=AnaerobicDigestionTreatmentPathway
numsampsPerDimension = 10 # Will create numsampsPerDimension^4 samples for the cube


inputs <- data.frame(createSamples(dims=4,level=1,samps=numsampsPerDimension, min=14,max=22))
inputs <- cbind(inputs, createSamples(dims=4,level=2,samps=numsampsPerDimension, min=13/18, max= 18/18))
inputs <- cbind(inputs, createSamples(dims=4,level=3,samps=numsampsPerDimension, min=750, max=950))
inputs <- cbind(inputs, createSamples(dims=4,level=4,samps=numsampsPerDimension, min=4500, max=6500))
colnames(inputs) <- c("TS","VS","Bo","TKN")
f1 <- Feedstock(type="variability", TS=inputs[,1], VS=inputs[,2], Bo=inputs[,3], TKN=inputs[,4])
g1 <- GlobalFactors()
out <- FUN(f1, g1, debug = F)
inout <- cbind(inputs,out)

# plot Net Emissions vs. each of the 4 inputs.
# plot(inputs$TS,out[,1])
# plot(inputs$VS,out[,1])
# plot(inputs$Bo,out[,1])
# plot(inputs$TKN,out[,1])
par(mfrow=c(2,2))
boxplot(out[,1] ~ signif(inputs$TS,2),xlab="TS",ylab="Net Emissions")
boxplot(out[,1] ~ signif(inputs$VS,2),xlab="VS",ylab="Net Emissions")
boxplot(out[,1] ~ signif(inputs$Bo,2),xlab="Bo",ylab="Net Emissions")
boxplot(out[,1] ~ signif(inputs$TKN,2),xlab="TKN",ylab="Net Emissions")