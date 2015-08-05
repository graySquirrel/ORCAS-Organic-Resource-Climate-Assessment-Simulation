# simuTreat.R to simulate treatments
#    of different feedstocks to generate GHG and cost outputs.

library(rgl)
setwd("/Users/jacquelineebner/fracGASM")
source("treatmentClasses.R") 
source("treatmentLandApplication.R") 


f1 <- Feedstock(type="GTW",
                TS=18,
                VS=17/18,
                Bo=887,
                TKN=5600)

g1 <- GlobalFactors()
res <- LandApplicationTreatmentPathway(f1, g1, debug = T, Ninitial = f1$TKN)

print(res)