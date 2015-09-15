# create baseline results

source("treatmentClasses.R") 
source("treatmentAnaerobicDigestion.R") 
source("treatmentLandApplication.R") 
source("treatmentLandfill.R")
source("treatmentcompost.R")
source("parseGlobalFactors.R")

getBaselineResults <- function() {
    i <- read.csv(file="Feedstock.csv",sep = ",",stringsAsFactors=FALSE)
    f1 <- Feedstock(type=i$Feedstock,TS=i$TS,VS=i$VS,Bo=i$Bo,TKN=i$TKN,
                    percentCarboTS = i$PercentCarboTS, percentLipidTS = i$PercentlipidTS,
                    percentProteinTS = i$PercentproteinTS, fdeg = i$fdeg)
    g1 <- getGlobalFactorsFromFile(doRanges = FALSE)
    o<-NULL
    o$AD <- AnaerobicDigestionTreatmentPathway(f1, g1, Application = 'noDisplace')
    o$ADf <- AnaerobicDigestionTreatmentPathway(f1, g1, Application = 'Fertilizer')
    o$LA <- LandApplicationTreatmentPathway(f1, g1, Application = 'noDisplace')
    o$LAf <- LandApplicationTreatmentPathway(f1, g1, Application = 'Fertilizer')
    o$CM <- compostTreatmentPathway(f1, g1, Application = 'noDisplace')
    o$CMf <- compostTreatmentPathway(f1, g1, Application = 'Fertilizer')
    o$CMp <- compostTreatmentPathway(f1, g1, Application = 'Peat')
    o$LF <- LandfillTreatmentPathway(f1, g1)
    o
}

o <- getBaselineResults()
result <- data.frame(f1$type,o$AD[[1]],o$ADf[[1]],
                     o$LA[[1]],o$LAf[[1]],o$CM[[1]],
                     o$CMf[[1]],o$CMp[[1]],o$LF[[1]],
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
write.csv(o, file="allPathwaysBreakdowns.csv",row.names=f1$type)
