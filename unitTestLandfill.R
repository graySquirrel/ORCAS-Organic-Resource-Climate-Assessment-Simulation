# simuTreat.R to simulate treatments
#    of different feedstocks to generate GHG and cost outputs.

library(rgl)
#setwd("/Users/jacquelineebner/fracGASM")
source("treatmentClasses.R") 
source("treatmentLandfill.R") 


f1 <- Feedstock(type="GTW",
                TS=18,
                VS=17/18,
                Bo=887,
                TKN=5600)

g1 <- GlobalFactors()
res <- LandfillTreatmentPathway(f1, g1, debug = T)

print(res)