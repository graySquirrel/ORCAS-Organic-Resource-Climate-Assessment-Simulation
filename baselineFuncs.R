source("treatmentClasses.R") 
source("treatmentAnaerobicDigestion.R") 
source("treatmentLandApplication.R") 
source("treatmentLandfill.R")
source("treatmentcompost.R")
source("treatmentAnimalFeed.R")
source("parseGlobalFactors.R")
library(ggplot2)

addNorms <- function(df,ts) {
    df1 <- cbind(df,df[1]/ts)
    colnames(df1) <- c(colnames(df),"NormTS")
    df1
}

getBaselineResults <- function(verbose = FALSE) {
    i <- read.csv(file="Feedstock.csv",sep = ",",stringsAsFactors=FALSE)
    f1 <- Feedstock(type=i$Feedstock,TS=i$TS,VS=i$VS,Bo=i$Bo,TKN=i$TKN,
                    percentCarboTS = i$PercentCarboTS, percentLipidTS = i$PercentlipidTS,
                    percentProteinTS = i$PercentproteinTS, fdeg = i$fdeg,TDN=i$TDN, 
                    Phosphorus=i$Phosphorus, Potassium=i$Potassium)
    g1 <- getGlobalFactorsFromFile(doRanges = FALSE, verbose = verbose)
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

#### Support funcs for monte carlo and plotting
calculatePathwayMC <- function(feedstockfile="Feedstock.csv",
                               factorFile="Globalfactors.csv",
                               FUN=NULL,
                               Application=NULL) {
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
                        fdeg = stocks[i,]$fdeg, TDN=stocks[i,]$TDN,
                        Phosphorus = stocks[i,]$Phosphorus,
                        Potassium=stocks[i,]$Potassium)
        if(length(Application)!=0) {
            out <- FUN(f1, g1,Application=Application)
        } else {
            out <- FUN(f1, g1)
        }
        q <- out[,1]
        outRanges <- cbind(outRanges, q)
    }
    colnames(outRanges) <- stocks$Feedstock
    bpstats <- boxplot(outRanges,plot=FALSE)
    confDat <- apply(outRanges,2,function(x) quantile(x,probs=c(.025,.5,.975)))
    #confDat <- apply(outRanges,2,function(x) quantile(x,probs=c(0,.5,1)))
    o<-NULL
    o$confDat <- confDat
    o$bpstats <- bpstats
    o$outRanges <- outRanges
    o$f1 <- stocks
    o$g1 <- g1
    o
}

massageDataforPlot <- function(in1,in2,treat) {
    df <-data.frame(t(in1))
    df <- cbind(rownames(df),df,in2)
    colnames(df)<- c("feedstock","lo","Median","hi","Emissions")
    df$treatment <- treat
    df$feedstock <- factor(df$feedstock)
    df
}
# Now do the dirty work of plotting
createPathwaysPlot <- function(doRanges = FALSE, CS, paths, disp) {
    b <- getBaselineResults()
    
    # Do the MC for all pathways, then pick what you want to show.
    ADstats <- calculatePathwayMC(FUN=AnaerobicDigestionTreatmentPathway)
    ADfstats <- calculatePathwayMC(FUN=AnaerobicDigestionTreatmentPathway, Application='Fertilizer')
    LFstats <- calculatePathwayMC(FUN=LandfillTreatmentPathway)
    CMstats <- calculatePathwayMC(FUN=compostTreatmentPathway)
    CMfstats <- calculatePathwayMC(FUN=compostTreatmentPathway, Application = 'Fertilizer')
    CMpstats <- calculatePathwayMC(FUN=compostTreatmentPathway, Application = 'Peat')
    CMbstats <- calculatePathwayMC(FUN=compostTreatmentPathway, Application = 'Blended')
    LAstats <- calculatePathwayMC(FUN=LandApplicationTreatmentPathway)
    LAfstats <- calculatePathwayMC(FUN=LandApplicationTreatmentPathway, Application='Fertilizer')
    AFstats <- calculatePathwayMC(FUN=AnimalFeedTreatmentPathway)
    
    
    y1 <- massageDataforPlot(ADstats$confDat, b$AD$ADnetEmissions,"AD")
    y1f <- massageDataforPlot(ADfstats$confDat, b$ADf$ADnetEmissions,"ADf")
    y2 <- massageDataforPlot(LFstats$confDat, b$LF$LandfillNetEmissions,"LF")
    y3 <- massageDataforPlot(CMstats$confDat, b$CM$final,"CM")
    y3f <- massageDataforPlot(CMfstats$confDat, b$CMf$final,"CMf")
    y3p <- massageDataforPlot(CMpstats$confDat, b$CMp$final,"CMp")
    y3b <- massageDataforPlot(CMbstats$confDat, b$CMb$final,"CMb")
    y4 <- massageDataforPlot(LAstats$confDat, b$LA$EMNetLandapp,"LA")
    y4f <- massageDataforPlot(LAfstats$confDat, b$LAf$EMNetLandapp,"LAf")
    y5 <- massageDataforPlot(AFstats$confDat,b$AF$EMAnimalFeed,"AF")
    
    #y <- rbind(y1,y1f,y2,y3,y3f,y3p,y4,y4f)
    #y <- rbind(y1,y2,y3,y4)
    y <- rbind(y1,y2,y5,y3)
    #y <- rbind(y1f,y2,y3f,y4f)
    #y <- rbind(y1,y3,y4)
    
    y$feedstock <- factor(y$feedstock, levels=y$feedstock[order(y2$Emissions)]) # order by LF
    
    if(!doRanges) {
        # Plot Nominals without ranges.
        myplot <- ggplot(y, aes(x=feedstock, y=Emissions,fill=treatment)) + 
            geom_bar(position=position_dodge(), stat="identity") +
            #geom_errorbar(aes(ymin=lo, ymax=hi), width=.3, position=position_dodge(0.9)) +
            theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5,size=16))
    } else {
        # Plot Nominal values
        myplot <- ggplot(y, aes(x=feedstock, y=Emissions,fill=treatment)) + 
            geom_bar(position=position_dodge(), stat="identity") +
            geom_errorbar(aes(ymin=lo, ymax=hi), width=.3, position=position_dodge(0.9)) +
            theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5,size=16))
    }
    myplot
}