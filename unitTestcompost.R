# simuTreat.R to simulate treatments
#    of different feedstocks to generate GHG and cost outputs.

library(rgl)
#setwd("/Users/jacquelineebner/fracGASM")
source("treatmentClasses.R") 
source("treatmentLandfill.R") 


f1 <- Feedstock(type="OFMSW",TS=0.3,VS=0.90,Bo=444,TKN=5600,
                percentCarboTS = 0.76, percentProteinTS = 0.10, percentLipidTS = 0.11,
                fdeg = 0.94
                )
#print(paste("Lo ",f1$Lo, "TVS ",f1$TVS,"InitialC ",f1$InitialC))
g1 <- GlobalFactors()
res <- LandfillTreatmentPathway(f1, g1, debug = T)

print(res)