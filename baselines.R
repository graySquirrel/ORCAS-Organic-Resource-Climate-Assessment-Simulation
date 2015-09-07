# create baseline results

source("treatmentClasses.R") 
source("treatmentAnaerobicDigestion.R") 
source("treatmentLandApplication.R") 
source("treatmentLandfill.R")
source("treatmentcompost.R")

#input <- read.csv(file=baselines.csv,sep = ",")

f1 <- Feedstock(type="baseline",TS=0.3,VS=0.90,Bo=334,TKN=5600,
                percentCarboTS = 0.8, percentProteinTS = 0.10, percentLipidTS = 0.10,
                fdeg = 0.85)
g1 <- GlobalFactors()

ADNoDisplace <- AnaerobicDigestionTreatmentPathway(f1, g1, Application = 'noDisplace')
ADFertDisplace <- AnaerobicDigestionTreatmentPathway(f1, g1, Application = 'Fertilizer')
LANoDisplace <- LandApplicationTreatmentPathway(f1, g1, Ninitial = f1$TKN, 
                                                Application = 'noDisplace')
LAFertDisplace <- LandApplicationTreatmentPathway(f1, g1, Ninitial = f1$TKN, 
                                                  Application = 'Fertilizer')
CMNoDisplace <- compostTreatmentPathway(f1, g1, Application = 'noDisplace')
CMFertDisplace <- compostTreatmentPathway(f1, g1, Application = 'Fertilizer')
CMPeatDisplace <- compostTreatmentPathway(f1, g1, Application = 'Peat')
Landfill <- LandfillTreatmentPathway(f1, g1)

result <- data.frame(f1$type,ADNoDisplace[[1]],ADFertDisplace[[1]],
                     LANoDisplace[[1]],LAFertDisplace[[1]],CMNoDisplace[[1]],
                     CMFertDisplace[[1]],CMPeatDisplace[[1]],Landfill[[1]])
colnames(result) <- c("Feedstock Type","AD no displacement", 
                      "AD Fertilizer displacement",
                      "Land Application no displacement", 
                      "Land Application Fertlizer displacement",
                      "Compost no displacement", "Compost Fertilizer displacement", 
                      "Compost Peat displacement", "Landfill")

write.csv(result, file="baselinesOutput.csv",row.names=FALSE)