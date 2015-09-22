# create baseline results

source("treatmentClasses.R") 
source("treatmentAnaerobicDigestion.R") 
source("treatmentLandApplication.R") 
source("treatmentLandfill.R")
source("treatmentcompost.R")
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
                    percentProteinTS = i$PercentproteinTS, fdeg = i$fdeg)
    g1 <- getGlobalFactorsFromFile(doRanges = FALSE)
    o<-NULL
    o$AD <- AnaerobicDigestionTreatmentPathway(f1, g1, Application = 'noDisplace')
    o$AD <- addNorms(o$AD,f1$TS)
    o$ADf <- AnaerobicDigestionTreatmentPathway(f1, g1, Application = 'Fertilizer')
    o$ADf <- addNorms(o$ADf,f1$TS)
    o$LA <- LandApplicationTreatmentPathway(f1, g1, Application = 'noDisplace')
    o$LA <- addNorms(o$LA,f1$TS)
    o$LAf <- LandApplicationTreatmentPathway(f1, g1, Application = 'Fertilizer')
    o$LAf <- addNorms(o$LAf,f1$TS)
    o$CM <- compostTreatmentPathway(f1, g1, Application = 'noDisplace')
    o$CM <- addNorms(o$CM,f1$TS)
    o$CMf <- compostTreatmentPathway(f1, g1, Application = 'Fertilizer')
    o$CMf <- addNorms(o$CMf,f1$TS)
    o$CMp <- compostTreatmentPathway(f1, g1, Application = 'Peat')
    o$CMp <- addNorms(o$CMp,f1$TS)
    o$LF <- LandfillTreatmentPathway(f1, g1)
    o$LF <- addNorms(o$LF,f1$TS)
    o$f1 <- f1
    o$g1 <- g1
    o
}

o <- getBaselineResults()
result <- data.frame(o$f1$type,o$AD[[1]],o$ADf[[1]],
                     o$LA[[1]],o$LAf[[1]],o$CM[[1]],
                     o$CMf[[1]],o$CMp[[1]],o$LF[[1]],
                     o$f1$TS,o$f1$VS,o$f1$Bo,o$f1$TKN,o$f1$percentCarboTS,
                     o$f1$percentLipidTS,
                     o$f1$percentProteinTS,o$f1$Lo,o$f1$TVS,o$f1$fdeg)
colnames(result) <- c("Feedstock Type","AD no displacement", 
                      "AD Fertilizer displacement",
                      "Land Application no displacement", 
                      "Land Application Fertlizer displacement",
                      "Compost no displacement", "Compost Fertilizer displacement", 
                      "Compost Peat displacement", "Landfill","TS","VS","Bo","TKN",
                      "percentCarboTS","percentLipidTS","percentProteinTS","Lo","TVS","fdeg")

write.csv(result, file="baselinesOutput.csv",row.names=FALSE)
write.csv(o, file="allPathwaysBreakdowns.csv",row.names=o$f1$type)
