# simuTreat.R to simulate treatments
#    of different feedstocks to generate GHG and cost outputs.

#setwd("/Users/jacquelineebner/fracGASM")
source("treatmentClasses.R") 
debugSource("treatmentcompost.R") 
source("parseGlobalFactors.R")


f1 <- Feedstock(type="OFMSW",TS=0.3,VS=0.90,Bo=334,TKN=9000,
                percentCarboTS = 0.8, percentProteinTS = 0.10, percentLipidTS = 0.10,
                fdeg = 0.84)

# print(paste("Lo ",f1$Lo, "TVS ",f1$TVS,"InitialC ",f1$InitialC))
# #g1 <- GlobalFactors()
g1 <- getGlobalFactorsFromFile(doRanges = FALSE,verbose = TRUE)
# res <- compostTreatmentPathway(f1, g1, Application = 'noDisplace', debug = T)
# print(res)
# res <- compostTreatmentPathway(f1, g1, Application = 'Fertilizer', debug = T)
# print(res)
# res <- compostTreatmentPathway(f1, g1, Application = 'Peat', debug = T)
# print(res)
res <- compostTreatmentPathway(f1, g1, Application = 'Blended', debug = T)
print(res)
# res <- compostTreatmentPathway(f1, g1, Application = 'noDisplace', sequesterCarbon = F, debug = T)
# print(res)
# res <- compostTreatmentPathway(f1, g1, Application = 'Fertilizer', sequesterCarbon = F, debug = T)
# print(res)
# res <- compostTreatmentPathway(f1, g1, Application = 'Peat', sequesterCarbon = F, debug = T)
# print(res)
# res <- compostTreatmentPathway(f1, g1, Application = 'Blended', sequesterCarbon = F, debug = T)
# print(res)