# for each pathway, for each feedstock, for each factor
# compute the nominal, low, and high values for each factor independently
# order by feedstock, write out csv

source("treatmentClasses.R") 
source("treatmentAnaerobicDigestion.R") 
source("treatmentLandApplication.R") 
source("treatmentcompost.R")
source("treatmentAnimalFeed.R")
source("treatmentLandfill.R")
source("parseGlobalFactors.R")

f <- read.csv(file="Feedstock.csv",sep = ",",stringsAsFactors=FALSE)
f1 <- Feedstock(type=f$Feedstock, TS=f$TS, VS=f$VS, Bo=f$Bo, TKN=f$TKN,
                percentCarboTS=f$PercentCarboTS,percentProteinTS=f$PercentproteinTS,
                percentLipidTS=f$PercentlipidTS,
                fdeg=f$fdeg,Phosphorus=f$Phosphorus, Potassium=f$Potassium)

GFmemfile <- read.csv("Globalfactors.csv",stringsAsFactors = FALSE)
glob <- getGlobalFactorsFromFile(doRanges = FALSE)

#################################################################################
singleValueSensitivityByRange <- function(pathway,
                                          factorName,
                                          FUN,
                                          f1) {
    print(paste("FACTORNAME",factorName))
    g1 <- getGlobalFactorsFromFile(doRanges = FALSE)
    nom <- as.numeric(g1[[factorName]])
    outNom <- FUN(f1, g1, debug = F)[[1]]
    low <- GFmemfile[GFmemfile$sw.name==factorName,"Low"][[1]]# elim multiple entries 
    g1[[factorName]] <- as.numeric(low) 
    outLo <- FUN(f1, g1, debug = F)[[1]]
    high <- GFmemfile[GFmemfile$sw.name==factorName,"High"][[1]]# elim multiple entries 
    g1[[factorName]] <- as.numeric(high)
    outHi <- FUN(f1, g1, debug = F)[[1]]
    data.frame(f1$type, pathway, factorName, nom, outNom, low, outLo, high, outHi)
}
##############################################################################
fl <- NULL
o <- NULL
fl$AD <- AnaerobicDigestionTreatmentPathway
fl$CM <- compostTreatmentPathway
fl$AF <- AnimalFeedTreatmentPathway
fl$LF <- LandfillTreatmentPathway
#fl$LA <- LandApplicationTreatmentPathway

for (i in 1:length(GFmemfile[,1])){
    if (GFmemfile[i,1] != "" && GFmemfile[i,5] == "Range") {
        pw <- GFmemfile[i,1]
        gf <- GFmemfile[i,"sw.name"]
        val <- glob[[gf]]
        if (length(val) != 0) { # meaning if the factor exists in globalFactors
            if (GFmemfile[i,1] == "All") {
                ifelse(is.null(o),
                       o <- singleValueSensitivityByRange("AD",gf,
                                                          AnaerobicDigestionTreatmentPathway,f1),
                       o <- rbind(o,singleValueSensitivityByRange("AD",gf,
                                                                  AnaerobicDigestionTreatmentPathway,f1)))
                o <- rbind(o,singleValueSensitivityByRange("CM",gf,compostTreatmentPathway,f1))
                o <- rbind(o,singleValueSensitivityByRange("AF",gf,AnimalFeedTreatmentPathway,f1))
                o <- rbind(o,singleValueSensitivityByRange("LF",gf,LandfillTreatmentPathway,f1))
#                o <- rbind(o,singleValueSensitivityByRange("LA",gf,LandApplicationTreatmentPathway,f1))
            } else {
                FUN <- fl[[GFmemfile[i,1]]]
                ifelse(is.null(o),
                       o <- singleValueSensitivityByRange(pw,gf,FUN,f1),
                       o <- rbind(o,singleValueSensitivityByRange(pw,gf,FUN,f1)))
            }
        }
    }
}
ord <- order(o$f1.type)
o <- o[ord,]

write.csv(o,file = "lowHighOut.csv",row.names = FALSE)


