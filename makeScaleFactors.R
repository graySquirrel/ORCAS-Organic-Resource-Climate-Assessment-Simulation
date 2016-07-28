# makeScaleFactors.R will create a dataframe with rows of feedstock
# and cols of pathways, and create a csv file of the output

source("treatmentClasses.R") 
source("treatmentAnaerobicDigestion.R") 
#source("treatmentLandApplication.R") 
source("treatmentLandfill.R")
source("treatmentcompost.R")
source("treatmentAnimalFeed.R")
source("parseGlobalFactors.R")

i <- read.csv(file="Feedstock.csv",sep = ",",stringsAsFactors=FALSE)
f1 <- Feedstock(type=i$Feedstock,TS=i$TS,VS=i$VS,Bo=i$Bo,TKN=i$TKN,
                percentCarboTS = i$PercentCarboTS, 
                percentLipidTS = i$PercentlipidTS,
                percentProteinTS = i$PercentproteinTS, 
                fdeg = i$fdeg,
                Phosphorus=i$Phosphorus, Potassium=i$Potassium)
g1 <- getGlobalFactorsFromFile(doRanges = FALSE)
LFs <- LandfillTreatmentPathway(f1,g1)[[1]]
ADs <- AnaerobicDigestionTreatmentPathway(f1,g1,Application = "Fertilizer")[[1]]
CMs <- compostTreatmentPathway(f1,g1,Application = "Blended")[[1]]
AFs <- AnimalFeedTreatmentPathway(f1,g1)[[1]]
scaleFactors <- data.frame(Landfill=LFs,AD=ADs,Compost=CMs,Animal.Feed=AFs)
rownames(scaleFactors) <- f1$type
write.csv(scaleFactors, file = "tonnageRepurposer/scaleFactors.csv")