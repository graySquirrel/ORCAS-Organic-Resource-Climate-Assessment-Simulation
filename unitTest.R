# simuTreat.R to simulate treatments
#    of different feedstocks to generate GHG and cost outputs.
library(rgl)
#setwd("C:/Users/febner/Documents/CourseraDataScience/fracGASM")
source("treatmentClasses.R") 
source("treatmentAnaerobicDigestion.R") 
source("treatmentLandApplication.R") 


f1 <- Feedstock(type="GTW",
                TS=18,
                VS=17/18,
                Bo=887,
                TKN=5600)

g1 <- GlobalFactors()
res <- AnaerobicDigestionTreatmentPathway(f1, g1, debug = F)

print(res)
str(res)
res[1,1]