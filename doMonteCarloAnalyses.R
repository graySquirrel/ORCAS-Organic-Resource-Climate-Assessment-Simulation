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
source("baselines.R")
#################################################################################
calculatePathwayMC <- function(feedstockfile="Feedstock.csv",
                             factorFile="Globalfactors.csv",
                             FUN=NULL,
                             Application=NULL) {
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
        if(length(Application)!=0) {
          out <- FUN(f1, g1,Application=Application)
        } else {
          out <- FUN(f1, g1)
        }
        #out <- FUN(f1, g1,Application='Fertilizer')
        #out <- FUN(f1, g1,Application='Peat')

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
app <- 'Fertilizer'
ADstats <- calculatePathwayMC(FUN=AnaerobicDigestionTreatmentPathway,Application=app)
LFstats <- calculatePathwayMC(FUN=LandfillTreatmentPathway)
CMstats <- calculatePathwayMC(FUN=compostTreatmentPathway,Application=app)
LAstats <- calculatePathwayMC(FUN=LandApplicationTreatmentPathway,Application=app)
#####################################################################
# Get baselines
b <- getBaselineResults()
#####################################################################
massageDataforPlot <- function(in1,in2,treat) {
    df <-data.frame(t(in1))
    df <- cbind(rownames(df),df,in2)
    colnames(df)<- c("feedstock","lo","Median","hi","Nominal")
    df$treatment <- treat
    df$feedstock <- factor(df$feedstock)
    df
}
# Now do the dirty work of plotting
library(ggplot2)
if(app=='Fertilizer') {
  y1 <- massageDataforPlot(ADstats$confDat, b$ADf$ADnetEmissions,"ADf")
  y2 <- massageDataforPlot(LFstats$confDat, b$LF$LandfillNetEmissions,"LF")
  y3 <- massageDataforPlot(CMstats$confDat, b$CMf$final,"CMf")
  y4 <- massageDataforPlot(LAstats$confDat, b$LAf$EMNetLandapp,"LAf")
} else if (app=='Peat') {
  y1 <- massageDataforPlot(ADstats$confDat, b$AD$ADnetEmissions,"AD")
  y2 <- massageDataforPlot(LFstats$confDat, b$LF$LandfillNetEmissions,"LF")
  y3 <- massageDataforPlot(CMstats$confDat, b$CMp$final,"CMp")
  y4 <- massageDataforPlot(LAstats$confDat, b$LA$EMNetLandapp,"LA")
} else {
  y1 <- massageDataforPlot(ADstats$confDat, b$AD$ADnetEmissions,"AD")
  y2 <- massageDataforPlot(LFstats$confDat, b$LF$LandfillNetEmissions,"LF")
  y3 <- massageDataforPlot(CMstats$confDat, b$CM$final,"CM")
  y4 <- massageDataforPlot(LAstats$confDat, b$LA$EMNetLandapp,"LA")
}

#y <- rbind(y1,y2,y3,y4)
y <- rbind(y1,y3,y4)
y$feedstock <- factor(y$feedstock, levels=y$feedstock[order(y2$Nominal)]) # order by LF

# Plot Nominal values
p1 <- ggplot(y, aes(x=feedstock, y=Nominal,fill=treatment)) + 
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin=lo, ymax=hi), width=.3, position=position_dodge(0.9)) +
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5,size=16))

# Make a second plot to show median values
p2 <- ggplot(y, aes(x=feedstock, y=Median,fill=treatment)) + 
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin=lo, ymax=hi), width=.3, position=position_dodge(0.9)) +
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5,size=16))
print(p1)
print(p2)
