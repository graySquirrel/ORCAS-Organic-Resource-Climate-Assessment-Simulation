# simuTreat.R to simulate treatments
#    of different feedstocks to generate GHG and cost outputs.
#library(rgl)
#setwd("C:/Users/febner/Documents/CourseraDataScience/fracGASM")
source("treatmentClasses.R") 
source("parseGlobalFactors.R")
source("treatmentAnaerobicDigestion.R") 
source("treatmentAnaerobicDigestionSimp.R") 
source("treatmentLandApplication.R") 


f1 <- Feedstock(type="OFMSW",TS=0.3,VS=0.90,Bo=334,TKN=5600,
                percentCarboTS = 0.8, percentProteinTS = 0.10, percentLipidTS = 0.10,
                fdeg = 0.841)

g1 <- getGlobalFactorsFromFile(doRanges = FALSE,verbose = FALSE)


res1 <- AnaerobicDigestionTreatmentPathway(f1, g1, debug = F, Application = 'noDisplace')
#print(res)

res2 <- AnaerobicDigestionTreatmentPathwaySimp(f1, g1, debug = F, Application = 'noDisplace')
#print(res)
assertthat::are_equal(res1,res2)