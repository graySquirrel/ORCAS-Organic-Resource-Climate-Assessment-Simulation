# create baseline results

source("treatmentClasses.R") 
source("treatmentAnaerobicDigestion.R") 
source("treatmentLandApplication.R") 
source("treatmentLandfill.R")
source("treatmentcompost.R")
source("treatmentAnimalFeed.R")
source("parseGlobalFactors.R")

addNorms <- function(df,ts) {
    df1 <- cbind(df,df[1]/ts)
    colnames(df1) <- c(colnames(df),"NormTS")
    df1
}

getBaselineResults <- function() {
    i <- read.csv(file="Feedstock.csv",sep = ",",stringsAsFactors=FALSE)
    f1 <- Feedstock(type=i$Feedstock,TS=i$TS,VS=i$VS,Bo=i$Bo,TKN=i$TKN,
                    percentCarboTS = i$PercentCarboTS, percentLipidTS = i$PercentlipidTS,
                    percentProteinTS = i$PercentproteinTS, fdeg = i$fdeg,TDN=i$TDN, 
                    Phosphorus=i$Phosphorus, Potassium=i$Potassium)
    g1 <- getGlobalFactorsFromFile(doRanges = FALSE)
    o<-NULL
    o$AF  <- AnimalFeedTreatmentPathway(f1, g1) # only path without a sequesterCarbon = F case
    
    o$AD  <- AnaerobicDigestionTreatmentPathway(f1, g1, Application = 'noDisplace')
    o$ADf <- AnaerobicDigestionTreatmentPathway(f1, g1, Application = 'Fertilizer')
    o$LA  <- LandApplicationTreatmentPathway(f1, g1, Application = 'noDisplace')
    o$LAf <- LandApplicationTreatmentPathway(f1, g1, Application = 'Fertilizer')
    o$CM  <- compostTreatmentPathway(f1, g1, Application = 'noDisplace')
    o$CMf <- compostTreatmentPathway(f1, g1, Application = 'Fertilizer')
    o$CMp <- compostTreatmentPathway(f1, g1, Application = 'Peat')
    o$CMb <- compostTreatmentPathway(f1, g1, Application = 'Blended')
    o$LF  <- LandfillTreatmentPathway(f1, g1)

    o$ADNoCS  <- AnaerobicDigestionTreatmentPathway(f1, g1, Application = 'noDisplace', sequesterCarbon = F)
    o$ADfNoCS <- AnaerobicDigestionTreatmentPathway(f1, g1, Application = 'Fertilizer', sequesterCarbon = F)
    o$LANoCS  <- LandApplicationTreatmentPathway(f1, g1, Application = 'noDisplace', sequesterCarbon = F)
    o$LAfNoCS <- LandApplicationTreatmentPathway(f1, g1, Application = 'Fertilizer', sequesterCarbon = F)
    o$CMNoCS  <- compostTreatmentPathway(f1, g1, Application = 'noDisplace', sequesterCarbon = F)
    o$CMfNoCS <- compostTreatmentPathway(f1, g1, Application = 'Fertilizer', sequesterCarbon = F)
    o$CMpNoCS <- compostTreatmentPathway(f1, g1, Application = 'Peat', sequesterCarbon = F)
    o$CMbNoCS <- compostTreatmentPathway(f1, g1, Application = 'Blended', sequesterCarbon = F)
    o$LFNoCS  <- LandfillTreatmentPathway(f1, g1, sequesterCarbon = F)
    
    o$LF  <- addNorms(o$LF,f1$TS)
    o$CMb <- addNorms(o$CMb,f1$TS)
    o$CMp <- addNorms(o$CMp,f1$TS)
    o$CMf <- addNorms(o$CMf,f1$TS)
    o$CM  <- addNorms(o$CM,f1$TS)
    o$LAf <- addNorms(o$LAf,f1$TS)
    o$LA  <- addNorms(o$LA,f1$TS)
    o$ADf <- addNorms(o$ADf,f1$TS)
    o$AD  <- addNorms(o$AD,f1$TS)
    o$AF  <- addNorms(o$AF,f1$TS)
    
    o$LFNoCS  <- addNorms(o$LFNoCS,f1$TS)
    o$CMbNoCS <- addNorms(o$CMbNoCS,f1$TS)
    o$CMpNoCS <- addNorms(o$CMpNoCS,f1$TS)
    o$CMfNoCS <- addNorms(o$CMfNoCS,f1$TS)
    o$CMNoCS  <- addNorms(o$CMNoCS,f1$TS)
    o$LAfNoCS <- addNorms(o$LAfNoCS,f1$TS)
    o$LANoCS  <- addNorms(o$LANoCS,f1$TS)
    o$ADfNoCS <- addNorms(o$ADfNoCS,f1$TS)
    o$ADNoCS  <- addNorms(o$ADNoCS,f1$TS)

    o$f1  <- f1
    o$g1  <- g1
    o
}

o <- getBaselineResults()
result <- data.frame(
                     o$AF[[1]],o$AD[[1]],o$ADf[[1]],
                     o$LA[[1]],o$LAf[[1]],o$CM[[1]],
                     o$CMf[[1]],o$CMp[[1]],o$CMb[[1]],o$LF[[1]],
                     o$ADNoCS[[1]],o$ADfNoCS[[1]],
                     o$LANoCS[[1]],o$LAfNoCS[[1]],o$CMNoCS[[1]],
                     o$CMfNoCS[[1]],o$CMpNoCS[[1]],o$CMbNoCS[[1]],o$LFNoCS[[1]],
                     o$f1$TS,o$f1$VS,o$f1$Bo,o$f1$TKN,o$f1$percentCarboTS,
                     o$f1$percentLipidTS,
                     o$f1$percentProteinTS,o$f1$Lo,o$f1$TVS,o$f1$fdeg,o$f1$TDN)
colnames(result) <- c("Animal Feed","AD no displacement", 
                      "AD Fertilizer displacement",
                      "Land Application no displacement", 
                      "Land Application Fertlizer displacement",
                      "Compost no displacement", "Compost Fertilizer displacement", 
                      "Compost Peat displacement", "Compost Blended displacement", 
                      "Landfill",
                      "AD no displacement noCS", 
                      "AD Fertilizer displacement noCS",
                      "Land Application no displacement noCS", 
                      "Land Application Fertlizer displacement noCS",
                      "Compost no displacement noCS", "Compost Fertilizer displacement noCS", 
                      "Compost Peat displacement noCS", "Compost Blended displacement noCS", 
                      "Landfill noCS",
                      "TS","VS","Bo","TKN",
                      "percentCarboTS","percentLipidTS","percentProteinTS","Lo","TVS","fdeg","TDN")
result <- t(result)
colnames(result) <- o$f1$type
result <- result[,order(result[1,])]

write.csv(result, file="baselinesOutput.csv",row.names=TRUE)
write.csv(o, file="allPathwaysBreakdowns.csv",row.names=o$f1$type)
