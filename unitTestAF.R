#    of different feedstocks to generate GHG and cost outputs.
#setwd("C:/Users/febner/Documents/CourseraDataScience/fracGASM")
source("treatmentClasses.R") 
source("treatmentAnimalFeed.R") 


f1 <- Feedstock(type="OFMSW",TS=0.3,VS=0.90,Bo=334,TKN=5600,
                percentCarboTS = 0.8, percentProteinTS = 0.10, percentLipidTS = 0.10,
                fdeg = 0.841)

g1 <- GlobalFactors()
res <- AnimalFeedTreatmentPathway(f1, g1, debug = T)
print(res)