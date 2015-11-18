# simuTreat.R to simulate treatments
#    of different feedstocks to generate GHG and cost outputs.

#library(rgl)
#setwd("/Users/jacquelineebner/fracGASM")
source("treatmentClasses.R") 
source("treatmentLandApplication.R") 
source("parseGlobalFactors.R")

f1 <- Feedstock(type="OFMSW",TS=0.3,VS=0.90,Bo=334,TKN=6650,
                percentCarboTS = 0.8, percentProteinTS = 0.10, percentLipidTS = 0.10,
                fdeg = 0.841, Potassium =  3300 , Phosphorus = 1900 )

g1 <- getGlobalFactorsFromFile(doRanges = FALSE,verbose = TRUE)
res <- LandApplicationTreatmentPathway(f1, g1, debug = T, 
                                       Nremaining = f1$TKN/1000)
print(res)
# res <- LandApplicationTreatmentPathway(f1, g1, debug = T, Nremaining = f1$TKN, 
#                                        Application = 'Fertilizer')
# print(res)
# res <- LandApplicationTreatmentPathway(f1, g1, debug = T, Nremaining = f1$TKN, 
#                                        Application = 'noDisplace', sequesterCarbon = F)
# print(res)
# res <- LandApplicationTreatmentPathway(f1, g1, debug = T, Nremaining = f1$TKN, 
#                                        Application = 'Fertilizer', sequesterCarbon = F)
# print(res)