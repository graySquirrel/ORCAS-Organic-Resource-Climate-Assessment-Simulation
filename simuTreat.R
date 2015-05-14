# simuTreat.R calls treatmentClass.R objects to simulate treatments
#    of different feedstocks to generate GHG and cost outputs.

setwd("C:/Users/febner/Documents/CourseraDataScience/fracGASM")
# source the file because i not yet made package (am lazy)
if(!exists('treatmentClasses_R')){
    source("treatmentClasses.R")
    treatmentClasses_R<-T
}
source("treatmentClasses.R") #remove after debug

# go to town to create feedstocks and then treatment them.

f1 <- Feedstock(type=c("GTW","Whey","Apple Pomace","Bakery Waste"),
                TS=c(17,7,22,92),
                TVS=c(18,5,0,89),
                Bo=c(887,300,0,465),
                TKN=c(5600,580,1100,2100))


#print(paste("length is ",length(f1)))
#print(paste("type is ", typeof(f1)))

g1 <- GlobalFactors()

res <- AnaerobicDigestionTreatmentPathway(f1, g1, 20)
print (res)
