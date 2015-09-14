# monte carlo analysis for all pathways.
# vary input through getGlobalFactorsFromFile()
# get feedstocks from Feedstock.csv
# just do the calculations here and return processed data to caller
# plotting can be done in R or another application
source("treatmentClasses.R")
source("treatmentAnaerobicDigestion.R")
source("treatmentLandApplication.R")
source("treatmentcompost.R")
source("treatmentLandfill.R")
source("parseGlobalFactors.R")
#################################################################################
calculatePathwayMC <- function(feedstockfile="Feedstock.csv",
                             factorFile="Globalfactors.csv",
                             FUN=NULL) {
    set.seed(1234)
    stocks <- read.csv(file=feedstockfile,sep = ",",stringsAsFactors=FALSE)
    g1 <- getGlobalFactorsFromFile(file = factorFile)
    outRanges <- NULL
    for(i in 1:length(stocks$Feedstock)) {
        #print(i)
        f1 <- Feedstock(type=stocks[i,]$Feedstock,TS=stocks[i,]$TS,VS=stocks[i,]$VS,
                        Bo=stocks[i,]$Bo,TKN=stocks[i,]$TKN,
                        percentCarboTS = stocks[i,]$PercentCarboTS, 
                        percentLipidTS = stocks[i,]$PercentlipidTS,
                        percentProteinTS = stocks[i,]$PercentproteinTS, 
                        fdeg = stocks[i,]$fdeg)
        out <- FUN(f1, g1)
        q <- out[,1]
        outRanges <- cbind(outRanges, q)
    }
    colnames(outRanges) <- stocks$Feedstock
    bpstats <- boxplot(outRanges,plot=FALSE)
#     outRanges <- outRanges[,order(bpstats$stats[3,])]
#     bpstats <- boxplot(outRanges,plot=FALSE) # redo to get in same order
    confDat <- apply(outRanges,2,function(x) quantile(x,probs=c(.025,.5,.975)))
    o<-NULL
    o$confDat <- confDat
    o$bpstats <- bpstats
    o$outRanges <- outRanges
    o
}
#####################################################################
ADstats <- calculatePathwayMC(FUN=AnaerobicDigestionTreatmentPathway)
LFstats <- calculatePathwayMC(FUN=LandfillTreatmentPathway)
CMstats <- calculatePathwayMC(FUN=compostTreatmentPathway)
#####################################################################
# Now do the dirty work of plotting
library(ggplot2)
x1 <- t(ADstats$confDat)
y1<-data.frame(x1)
y1 <- cbind(rownames(y1),y1)
colnames(y1)<- c("feedstock","lo","Emissions","hi")
y1$treatment <- c("AD")
y1$feedstock <- factor(y1$feedstock)

x2 <- t(LFstats$confDat)
y2 <- data.frame(x2)
y2 <- cbind(rownames(y2),y2)
colnames(y2) <- c("feedstock","lo","Emissions","hi")
y2$treatment <- c("LF")
y2$feedstock <- factor(y2$feedstock)

x3 <- t(CMstats$confDat)
y3 <- data.frame(x3)
y3 <- cbind(rownames(y3),y3)
colnames(y3) <- c("feedstock","lo","Emissions","hi")
y3$treatment <- c("CM")
y3$feedstock <- factor(y3$feedstock)

y <- rbind(y1,y2,y3)
y$feedstock <- factor(y$feedstock, levels=y$feedstock[order(y2$Emissions)]) # order by LF

ggplot(y, aes(x=feedstock, y=Emissions,fill=treatment)) + 
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin=lo, ymax=hi), width=.3, position=position_dodge(0.9)) +
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5,size=16))


