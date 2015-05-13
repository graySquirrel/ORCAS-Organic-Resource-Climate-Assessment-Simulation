# simuTreat.R calls treatmentClass.R objects to simulate treatments
#    of different feedstocks to generate GHG and cost outputs.

setwd("C:/Users/febner/Documents/CourseraDataScience/fracGASM_MontyPythonCarlo")
# source the file because i not yet made package (am lazy)
if(!exists('treatmentClasses_R')){
    source("treatmentClasses.R")
    treatmentClasses_R<-T
}
source("treatmentClasses.R") #remove after debug

# go to town to create feedstocks and then treatment them.

f1 <- Feedstock(mass=1,vol=2,type="fart",Ts=c(2.2,3.3),Vs=c(2.2,3.3),
                TKN=c(2.2,3.3),Bo=c(2.2,3.3),CN=c(2.2,3.3))

print(summary(f1))