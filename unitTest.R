# simuTreat.R to simulate treatments
#    of different feedstocks to generate GHG and cost outputs.
#library(rgl)
#setwd("C:/Users/febner/Documents/CourseraDataScience/fracGASM")
source("treatmentClasses.R") 
debugSource("treatmentAnaerobicDigestion.R") 
source("treatmentLandApplication.R") 
source("parseGlobalFactors.R")


f1 <- Feedstock(type="OFMSW",TS=0.3,VS=0.90,Bo=334,TKN=6650,
                percentCarboTS = 0.8, percentProteinTS = 0.10, percentLipidTS = 0.10,
                fdeg = 0.841, Potassium =  3300 , Phosphorus = 1900 )

g1 <- getGlobalFactorsFromFile(doRanges = FALSE,verbose = TRUE)
#g1 <- GlobalFactors()
#res <- AnaerobicDigestionTreatmentPathway(f1, g1, debug = T, Application = 'noDisplace')
#print(res)
res <- AnaerobicDigestionTreatmentPathway(f1, g1, debug = T, Application = 'Fertilizer')
print(res)
# res <- AnaerobicDigestionTreatmentPathway(f1, g1, debug = T, Application = 'noDisplace', sequesterCarbon = F)
# print(res)
# res <- AnaerobicDigestionTreatmentPathway(f1, g1, debug = T, Application = 'Fertilizer', sequesterCarbon = F)
# print(res)