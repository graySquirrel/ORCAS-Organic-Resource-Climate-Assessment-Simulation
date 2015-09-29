#    of different feedstocks to generate GHG and cost outputs.
#setwd("C:/Users/febner/Documents/CourseraDataScience/fracGASM")
source("treatmentClasses.R") 
source("treatmentAnimalFeed.R") 


f1 <- Feedstock(type="baked goods",TS=0.916,VS=0.979,Bo=465.3,TKN=14656,
                percentCarboTS = 0.76, percentProteinTS = 0.10, percentLipidTS = 0.11,
                fdeg = 0.94,TDN=0.89)

g1 <- GlobalFactors()
res <- AnimalFeedTreatmentPathway(f1, g1, debug = T)
print(res)