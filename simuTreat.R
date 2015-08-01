# simuTreat.R to simulate treatments
#    of different feedstocks to generate GHG and cost outputs.
library(rgl)
#setwd("C:/Users/febner/Documents/CourseraDataScience/fracGASM") setwd ("fracGasm") at start of session
source("treatmentClasses.R") 
source("treatmentAnaerobicDigestion.R") 
#TS=c(17,7,22,92)
#TVS=c(18,5,0,89)
#type=c("GTW","Whey","Apple Pomace","Bakery Waste")
#LoVec <- c(161,15,0,413)
#TKNVec <- c(5600,580,1100,2100)
# repeat TKNVec 10 times in a row
size <- 10
TKNVec <- seq(from =580, to=5600,length.out=size)
LoVec <-  seq(from = 15, to=413,length.out=size)
TKNVecRep <- rep(TKNVec,each=size)
LoVecRep  <- rep(LoVec,times=size)
f1 <- Feedstock(type="GTW",
                TS=17,
                TVS=18,
                Lo=LoVecRep,
                TKN=TKNVecRep)
#print(paste("length is ",length(f1)))
#print(paste("type is ", typeof(f1)))
g1 <- GlobalFactors()
res <- AnaerobicDigestionTreatmentPathway(f1, g1, 20,debug = F)
dfres <- data.frame(LoVecRep,TKNVecRep,res)
#plot3d(LoVecRep,TKNVecRep,res)

netEmissions <- xtabs(res ~ LoVecRep+TKNVecRep, data=dfres)
#persp(LoVec,TKNVec,netEmissions,theta=30,phi=30,shade=0.25,
#      col="lightblue",ticktype="simple")
#opens an interactive session
open3d()
bg3d("white")
material3d(col="black")
persp3d(LoVec,TKNVec,netEmissions,theta=30,phi=30,ticktype="detailed",col="lightblue")
#print (dfres)

