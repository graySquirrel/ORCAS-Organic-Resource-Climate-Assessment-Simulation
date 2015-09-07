# create baseline results

source("treatmentClasses.R") 
source("treatmentAnaerobicDigestion.R") 
source("treatmentLandApplication.R") 
source("treatmentLandfill.R")
source("treatmentcompost.R")

i <- read.csv(file="Feedstock.csv",sep = ",",stringsAsFactors=FALSE)
f1 <- Feedstock(type=i$Feedstock,TS=i$TS,VS=i$VS,Bo=i$Bo,TKN=i$TKN,
                percentCarboTS = i$PercentCarboTS, percentLipidTS = i$PercentlipidTS,
                percentProteinTS = i$PercentproteinTS, fdeg = i$fdeg)
# f1 <- Feedstock(type="baseline",TS=0.3,VS=0.90,Bo=334,TKN=5600,
#                 percentCarboTS = 0.8, percentProteinTS = 0.10, percentLipidTS = 0.10,
#                 fdeg = 0.85)
g1 <- GlobalFactors()
o<-NULL
o$ADNoDisplace <- AnaerobicDigestionTreatmentPathway(f1, g1, Application = 'noDisplace')
o$ADFertDisplace <- AnaerobicDigestionTreatmentPathway(f1, g1, Application = 'Fertilizer')
o$LANoDisplace <- LandApplicationTreatmentPathway(f1, g1, Ninitial = f1$TKN, 
                                                Application = 'noDisplace')
o$LAFertDisplace <- LandApplicationTreatmentPathway(f1, g1, Ninitial = f1$TKN, 
                                                  Application = 'Fertilizer')
o$CMNoDisplace <- compostTreatmentPathway(f1, g1, Application = 'noDisplace')
o$CMFertDisplace <- compostTreatmentPathway(f1, g1, Application = 'Fertilizer')
o$CMPeatDisplace <- compostTreatmentPathway(f1, g1, Application = 'Peat')
o$Landfill <- LandfillTreatmentPathway(f1, g1)

result <- data.frame(f1$type,ADNoDisplace[[1]],ADFertDisplace[[1]],
                     LANoDisplace[[1]],LAFertDisplace[[1]],CMNoDisplace[[1]],
                     CMFertDisplace[[1]],CMPeatDisplace[[1]],Landfill[[1]],
                     f1$TS,f1$VS,f1$Bo,f1$TKN,f1$percentCarboTS,f1$percentLipidTS,
                     f1$percentProteinTS,f1$Lo,f1$TVS,f1$fdeg)
colnames(result) <- c("Feedstock Type","AD no displacement", 
                      "AD Fertilizer displacement",
                      "Land Application no displacement", 
                      "Land Application Fertlizer displacement",
                      "Compost no displacement", "Compost Fertilizer displacement", 
                      "Compost Peat displacement", "Landfill","TS","VS","Bo","TKN",
                      "percentCarboTS","percentLipidTS","percentProteinTS","Lo","TVS","fdeg")

write.csv(result, file="baselinesOutput.csv",row.names=FALSE)
justFoodWaste <- lapply(o, function(i) head(i, n=1)) # to check food waste outputs.
write.csv(o, file="allPathwaysBreakdowns.csv",row.names=f1$type)
