# Sensitivity Analysis sample test with AD
source("treatmentClasses.R") 
source("treatmentAnaerobicDigestion.R") 
source("treatmentLandApplication.R")
library(pse)
factors <- c("TS", "VS", "Bo", "TKN")
q <- c("qunif", "qunif", "qunif","qunif")
q.arg<- list(list(min=7, max=92), list(min=0.5,max=1), list(min=200,max=900),list(min=500,max=5600))

g1 <- GlobalFactors()

oneRun <- function (TS,VS,Bo,TKN) {
    f1 <- Feedstock(type="Sensitivity", TS=TS, VS=VS, Bo=Bo, TKN=TKN)
    res <- AnaerobicDigestionTreatmentPathway(f1, g1, debug = F)
    return (res[[1]]) # first element returned as simple number
}
modelRun <- function (my.data) {
    return(mapply(oneRun, my.data[,1], my.data[,2], my.data[,3], my.data[,4]))
}

myLHS <- LHS(modelRun, factors, 100, q, q.arg, nboot=50)
plotecdf(myLHS)
plotscatter(myLHS)
plotprcc(myLHS)
p <- pic(myLHS)
print(p[[1]]$pic)