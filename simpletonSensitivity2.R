# for each pathway, for each feedstock, for each factor
# compute the nominal, low, and high values for each factor independently
# order by feedstock, write out csv

source("treatmentClasses.R") 
source("treatmentAnaerobicDigestion.R") 
source("treatmentLandApplication.R") 
source("parseGlobalFactors.R")

f <- read.csv(file="Feedstock.csv",sep = ",",stringsAsFactors=FALSE)
f1 <- Feedstock(type=f$Feedstock, TS=f$TS, VS=f$VS, Bo=f$Bo, TKN=f$TKN,
                    percentCarboTS=f$PercentCarboTS,percentProteinTS=f$PercentproteinTS,
                    percentLipidTS=f$PercentlipidTS,
                    fdeg=f$fdeg,TDN=f$TDN, Phosphorus=f$Phosphorus, Potassium=f$Potassium)
#################################################################################
singleValueSensitivityByPercent <- function(pathway,
                                            factorName,
                                            FUN,
                                            f1) {
    GFmemfile <- read.csv("Globalfactors.csv",stringsAsFactors = FALSE)
    g1 <- getGlobalFactorsFromFile(doRanges = FALSE)
    nom <- g1[[factorName]]
    outNom <- FUN(f1, g1, debug = F)[[1]]
    low <- GFmemfile[GFmemfile$sw.name==factorName,"Range.Lo"]
    g1[[factorName]] <- low
    outLo <- FUN(f1, g1, debug = F)[[1]]
    high <- GFmemfile[GFmemfile$sw.name==factorName,"Range.High"]
    g1[[factorName]] <- high
    outHi <- FUN(f1, g1, debug = F)[[1]]
    
    o <- data.frame(f1$type, pathway, factorName, nom, outNom, low, outLo, high, outHi)
}
##############################################################################
p <- AnaerobicDigestionTreatmentPathway
o <- singleValueSensitivityByPercent("AD","EFGrid",p,f1)
o <- rbind(o,singleValueSensitivityByPercent("AD","AD_Digester_CH4Leaks",p,f1))
o <- rbind(o,singleValueSensitivityByPercent("AD","AD_Digester_conversion_KwHPerM3",p,f1))
o <- rbind(o,singleValueSensitivityByPercent("AD","AD_reductionInVS",p,f1))

ord <- order(o$f1.type)
o <- o[ord,]

write.csv(o,file = "lowHighOut.csv",row.names = FALSE)


