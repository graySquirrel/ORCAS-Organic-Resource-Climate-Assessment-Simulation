# create baseline results
source("treatmentClasses.R") 
source("treatmentAnaerobicDigestion.R") 
source("treatmentLandApplication.R") 
source("treatmentLandfill.R")
source("treatmentcompost.R")
source("treatmentAnimalFeed.R")
source("parseGlobalFactors.R")
source("baselineFuncs.R") #includes sourcing of all other important scripts.

o <- getBaselineResults(verbose = TRUE)
result <- data.frame(
                     o$AF[[1]],o$AD[[1]],o$ADf[[1]],
                     #o$LA[[1]],o$LAf[[1]],
                     o$CM[[1]],
                     o$CMf[[1]],o$CMp[[1]],o$CMb[[1]],o$LF[[1]],
                     #o$ADNoCS[[1]],o$ADfNoCS[[1]],
                     #o$LANoCS[[1]],o$LAfNoCS[[1]],
                     #o$CMNoCS[[1]],
                     #o$CMfNoCS[[1]],o$CMpNoCS[[1]],o$CMbNoCS[[1]],o$LFNoCS[[1]],
                     o$f1$TS,o$f1$VS,o$f1$Bo,o$f1$TKN,o$f1$percentCarboTS,
                     o$f1$percentLipidTS,
                     o$f1$percentProteinTS,o$f1$Lo,o$f1$TVS,o$f1$fdeg,o$f1$TDN)
colnames(result) <- c("Animal Feed","AD no displacement", 
                      "AD Fertilizer displacement",
                      #"Land Application no displacement", 
                      #"Land Application Fertlizer displacement",
                      "Compost no displacement", "Compost Fertilizer displacement", 
                      "Compost Peat displacement", "Compost Blended displacement", 
                      "Landfill",
                      #"AD no displacement noCS", 
                      #"AD Fertilizer displacement noCS",
                      #"Land Application no displacement noCS", 
                      #"Land Application Fertlizer displacement noCS",
                      #"Compost no displacement noCS", "Compost Fertilizer displacement noCS", 
                      #"Compost Peat displacement noCS", "Compost Blended displacement noCS", 
                      #"Landfill noCS",
                      "TS","VS","Bo","TKN",
                      "percentCarboTS","percentLipidTS","percentProteinTS","Lo","TVS","fdeg","TDN")
result <- t(result)
colnames(result) <- o$f1$type
result <- result[,order(result[1,])]

write.csv(result, file="baselinesOutput.csv",row.names=TRUE)
write.csv(o, file="allPathwaysBreakdowns.csv",row.names=o$f1$type)
