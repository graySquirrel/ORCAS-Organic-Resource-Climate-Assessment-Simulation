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
doFUNif <- function(FUN,f2,g2,applic2) {
  if (is.null(applic2)) {
    res <- FUN(f2, g2, debug = F)[[1]]
  }
  else {
    res <- FUN(f2, g2, Application = applic2, debug = F)[[1]]
  }
  res
}
#################################################################################
singleValueSensitivityByRange <- function(pathway,
                                          factorName,
                                          FUN,
                                          f1,
                                          applic=NULL) {
  #print(paste("FACTORNAME",factorName))
  g1 <- getGlobalFactorsFromFile(doRanges = FALSE)
  nom <- as.numeric(g1[[factorName]])
  outNom <- doFUNif(FUN,f1,g1,applic)
  low <- GFmemfile[GFmemfile$sw.name==factorName,"Low"][[1]]# elim multiple entries 
  g1[[factorName]] <- as.numeric(low) 
  outLo <- doFUNif(FUN,f1,g1,applic)
  high <- GFmemfile[GFmemfile$sw.name==factorName,"High"][[1]]# elim multiple entries 
  g1[[factorName]] <- as.numeric(high)
  outHi <- doFUNif(FUN,f1,g1,applic)
  pathway<-paste(pathway,applic)
  foo <- data.frame(f1$type, pathway, factorName, nom, outNom, low, outLo, high, outHi)
  #if (!is.null(applic)) {print (foo)}
  foo
}
##############################################################################
specialCaseSomePathways <- function(pw,gf,FUN,f1) {
    res<-NULL
    if(pw == "CM") {
        r1 <- singleValueSensitivityByRange(pw,gf,FUN,f1,applic='Peat')
        r2 <- singleValueSensitivityByRange(pw,gf,FUN,f1,applic='noDisplace')
        r3 <- singleValueSensitivityByRange(pw,gf,FUN,f1,applic='Fertilizer')
        r4 <- singleValueSensitivityByRange(pw,gf,FUN,f1,applic='Blended')
        r5 <- singleValueSensitivityByRange(pw,gf,FUN,f1,applic='LAFertilizer')
        res <- do.call("rbind",list(r1,r2,r3,r4,r5))
        #res <- rbind(r1,r2,r3,r4,r5)
    }
    else
        res <- singleValueSensitivityByRange(pw,gf,FUN,f1)
    res
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
               o <- specialCaseSomePathways("AD",gf,
                                                  AnaerobicDigestionTreatmentPathway,f1),
               o <- rbind(o,specialCaseSomePathways("AD",gf,
                                                          AnaerobicDigestionTreatmentPathway,f1)))
        o <- rbind(o,specialCaseSomePathways("CM",gf,compostTreatmentPathway,f1))
        o <- rbind(o,specialCaseSomePathways("AF",gf,AnimalFeedTreatmentPathway,f1))
        o <- rbind(o,specialCaseSomePathways("LF",gf,LandfillTreatmentPathway,f1))
        #                o <- rbind(o,singleValueSensitivityByRange("LA",gf,LandApplicationTreatmentPathway,f1))
      } else {
        FUN <- fl[[GFmemfile[i,1]]]
        ifelse(is.null(o),
               o <- specialCaseSomePathways(pw,gf,FUN,f1),
               o <- rbind(o,specialCaseSomePathways(pw,gf,FUN,f1)))
      }
    }
  }
}
ord <- order(o$pathway, o$f1.type)
o <- o[ord,]
o$outDiff <- o$outHi - o$outLo
agdf <- aggregate(abs(o$outDiff),list(pathway=o$pathway, type=o$f1.type),max)
o$maxImpact <- agdf[match(paste(o$f1.type,o$pathway),paste(agdf$type,agdf$pathway)),"x"]
o$percent <- 100 * o$outDiff / o$maxImpact
o <- o[abs(o$percent) > 0,]
write.csv(o,file = "lowHighOut.csv",row.names = FALSE)


