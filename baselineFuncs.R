library(ggplot2)

addNorms <- function(df,ts) {
    df1 <- cbind(df,df[1]/ts)
    colnames(df1) <- c(colnames(df),"NormTS")
    df1
}

#NOTE: removed LA pathway calcs.
# Removed all sequesterCarbon = F cases
getBaselineResults <- function(verbose = FALSE,
                               FSmemfile=NULL,
                               GFmemfile=NULL) {
    if (is.null(FSmemfile)) {
        i <- read.csv(file="Feedstock.csv",sep = ",",stringsAsFactors=FALSE)
    }
    else i <- FSmemfile
    f1 <- Feedstock(type=i$Feedstock,TS=i$TS,VS=i$VS,Bo=i$Bo,TKN=i$TKN,
                    percentCarboTS = i$PercentCarboTS, 
                    percentLipidTS = i$PercentlipidTS,
                    percentProteinTS = i$PercentproteinTS, 
                    fdeg = i$fdeg,
                    Phosphorus=i$Phosphorus, Potassium=i$Potassium)
    if (is.null(GFmemfile)) {
        g1 <- getGlobalFactorsFromFile(doRanges = FALSE, verbose = verbose)
    }
    else g1 <- getGlobalFactorsFromFile(GFmemfile = GFmemfile, doRanges = FALSE)
    o<-NULL
    # only path without a sequesterCarbon = F case
    
    o$AF  <- AnimalFeedTreatmentPathway(f1, g1) 
    
    o$AD  <- AnaerobicDigestionTreatmentPathway(f1, g1, Application = 'noDisplace')
    o$ADf <- AnaerobicDigestionTreatmentPathway(f1, g1, Application = 'Fertilizer')
#    o$LA  <- LandApplicationTreatmentPathway(f1, g1, Application = 'noDisplace')
#    o$LAf <- LandApplicationTreatmentPathway(f1, g1, Application = 'Fertilizer')
    o$CM  <- compostTreatmentPathway(f1, g1, Application = 'noDisplace')
    o$CMLA  <- compostTreatmentPathway(f1, g1, Application = 'LAFertilizer')
    o$CMf <- compostTreatmentPathway(f1, g1, Application = 'Fertilizer')
    o$CMp <- compostTreatmentPathway(f1, g1, Application = 'Peat')
    o$CMb <- compostTreatmentPathway(f1, g1, Application = 'Blended')
    o$LF  <- LandfillTreatmentPathway(f1, g1)
    
#     o$ADNoCS  <- AnaerobicDigestionTreatmentPathway(f1, g1, 
#                                 Application = 'noDisplace', sequesterCarbon = F)
#     o$ADfNoCS <- AnaerobicDigestionTreatmentPathway(f1, g1, 
#                                 Application = 'Fertilizer', sequesterCarbon = F)
# #     o$LANoCS  <- LandApplicationTreatmentPathway(f1, g1, 
# #                                 Application = 'noDisplace', sequesterCarbon = F)
# #     o$LAfNoCS <- LandApplicationTreatmentPathway(f1, g1, 
# #                                 Application = 'Fertilizer', sequesterCarbon = F)
#     o$CMNoCS  <- compostTreatmentPathway(f1, g1, 
#                                 Application = 'noDisplace', sequesterCarbon = F)
#     o$CMfNoCS <- compostTreatmentPathway(f1, g1, 
#                                 Application = 'Fertilizer', sequesterCarbon = F)
#     o$CMpNoCS <- compostTreatmentPathway(f1, g1, 
#                                 Application = 'Peat', sequesterCarbon = F)
#     o$CMbNoCS <- compostTreatmentPathway(f1, g1,
#                                 Application = 'Blended', sequesterCarbon = F)
#     o$LFNoCS  <- LandfillTreatmentPathway(f1, g1, 
#                                 sequesterCarbon = F)
    
#     o$LF  <- addNorms(o$LF,f1$TS)
#     o$CMb <- addNorms(o$CMb,f1$TS)
#     o$CMp <- addNorms(o$CMp,f1$TS)
#     o$CMf <- addNorms(o$CMf,f1$TS)
#     o$CM  <- addNorms(o$CM,f1$TS)
# #     o$LAf <- addNorms(o$LAf,f1$TS)
# #     o$LA  <- addNorms(o$LA,f1$TS)
#     o$ADf <- addNorms(o$ADf,f1$TS)
#     o$AD  <- addNorms(o$AD,f1$TS)
#     o$AF  <- addNorms(o$AF,f1$TS)
    
#     o$LFNoCS  <- addNorms(o$LFNoCS,f1$TS)
#     o$CMbNoCS <- addNorms(o$CMbNoCS,f1$TS)
#     o$CMpNoCS <- addNorms(o$CMpNoCS,f1$TS)
#     o$CMfNoCS <- addNorms(o$CMfNoCS,f1$TS)
#     o$CMNoCS  <- addNorms(o$CMNoCS,f1$TS)
#     o$LAfNoCS <- addNorms(o$LAfNoCS,f1$TS)
#     o$LANoCS  <- addNorms(o$LANoCS,f1$TS)
#     o$ADfNoCS <- addNorms(o$ADfNoCS,f1$TS)
#     o$ADNoCS  <- addNorms(o$ADNoCS,f1$TS)
    
    o$f1  <- f1
    o$g1  <- g1
    o
}

#### Support funcs for monte carlo and plotting
calculatePathwayMC <- function(FSmemfile=NULL,
                               GFmemfile=NULL,
                               FUN=NULL,
                               Application=NULL) {
    stocks <- FSmemfile # just lazy don't want to rename stocks
    g1 <- getGlobalFactorsFromFile(GFmemfile=GFmemfile,verbose = TRUE)
    outRanges <- NULL
    for(i in 1:length(stocks$Feedstock)) {
        #print(i)
        f1 <- Feedstock(type=stocks[i,]$Feedstock,TS=stocks[i,]$TS,VS=stocks[i,]$VS,
                        Bo=stocks[i,]$Bo,TKN=stocks[i,]$TKN,
                        percentCarboTS = stocks[i,]$PercentCarboTS, 
                        percentLipidTS = stocks[i,]$PercentlipidTS,
                        percentProteinTS = stocks[i,]$PercentproteinTS, 
                        fdeg = stocks[i,]$fdeg,
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
# plot factor sensitivity
plotFS <- function(theObj,feedstock,rangeFactor,xlabl,treatment) {
    df <- data.frame(theObj$outRanges)
    var <- gsub(" ",".",feedstock)
    y <- df[[var]]
    x <- theObj$g1[[rangeFactor]]
    plot(x, y, xlab=xlabl,ylab=paste(feedstock,treatment))
    abline(lm(y ~ x),col='red')
}
plotFS2 <- function(theObj,feedstock,rangeFactor,treatment) {
    df <- data.frame(theObj$outRanges)
    var <- gsub(" ",".",feedstock)
    y <- df[[var]]
    x <- unlist(rangeFactor)
    plot(x, y, xlab=names(rangeFactor),ylab=paste(feedstock,treatment))
    abline(lm(y ~ x),col='red')
}
plotFS3 <- function(theObj,feedstock,rangeFactor,xlabl,treatment) {
    df <- data.frame(theObj$outRanges)
    var <- gsub(" ",".",feedstock)
    y <- df[[var]]
    x <- unlist(rangeFactor)
    plot(x, y, xlab=xlabl,ylab=paste(feedstock,treatment))
    abline(lm(y ~ x),col='red')
}

calcAllStats <- function(FSmemfile=NULL, GFmemfile=NULL) {
    if (is.null(FSmemfile)) {
        FSmemfile <- read.csv(file="Feedstock.csv",sep = ",",stringsAsFactors=FALSE)
    } 
    if (is.null(GFmemfile)) {
        GFmemfile <- read.csv(file="Globalfactors.csv", stringsAsFactors = FALSE)
    }
    o <- NULL
    o$b <- getBaselineResults(GFmemfile = GFmemfile, FSmemfile = FSmemfile)
    # Do the MC for all pathways, then pick what you want to show.
    # TODO: revisit if/when this becomes too slow
    o$ADstats <- calculatePathwayMC(FSmemfile=FSmemfile,GFmemfile=GFmemfile,
                                  FUN=AnaerobicDigestionTreatmentPathway)
    o$ADfstats <- calculatePathwayMC(FSmemfile=FSmemfile,GFmemfile=GFmemfile,
                                   FUN=AnaerobicDigestionTreatmentPathway, Application='Fertilizer')
    o$LFstats <- calculatePathwayMC(FSmemfile=FSmemfile,GFmemfile=GFmemfile,
                                  FUN=LandfillTreatmentPathway)
    o$CMstats <- calculatePathwayMC(FSmemfile=FSmemfile,GFmemfile=GFmemfile,
                                  FUN=compostTreatmentPathway, Application =  'noDisplace')
    o$CMfstats <- calculatePathwayMC(FSmemfile=FSmemfile,GFmemfile=GFmemfile,
                                   FUN=compostTreatmentPathway, Application = 'Fertilizer')
    o$CMpstats <- calculatePathwayMC(FSmemfile=FSmemfile,GFmemfile=GFmemfile,
                                   FUN=compostTreatmentPathway, Application = 'Peat')
    o$CMbstats <- calculatePathwayMC(FSmemfile=FSmemfile,GFmemfile=GFmemfile,
                                   FUN=compostTreatmentPathway, Application = 'Blended')
    o$CMLAstats <- calculatePathwayMC(FSmemfile=FSmemfile,GFmemfile=GFmemfile,
                                     FUN=compostTreatmentPathway, Application = 'LAFertilizer')
#     o$LAstats <- calculatePathwayMC(FSmemfile=FSmemfile,GFmemfile=GFmemfile,
#                                   FUN=LandApplicationTreatmentPathway)
#     o$LAfstats <- calculatePathwayMC(FSmemfile=FSmemfile,GFmemfile=GFmemfile,
#                                    FUN=LandApplicationTreatmentPathway, Application='Fertilizer')
    o$AFstats <- calculatePathwayMC(FSmemfile=FSmemfile,GFmemfile=GFmemfile,
                                  FUN=AnimalFeedTreatmentPathway)
    o
}
# Now do the dirty work of plotting
# y is the list of things to plot.
makePathwaysPlot <- function(doRanges = FALSE,y = NULL,title=NULL) {
    if(!doRanges) {
        # Plot Nominals without ranges.
        myplot <- ggplot(y, aes(x=feedstock, y=Emissions,fill=treatment)) + 
            geom_bar(position=position_dodge(), stat="identity") +
            theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5,size=16)) +
            ggtitle(title)
    } else {
        # Plot Nominal values
        myplot <- ggplot(y, aes(x=feedstock, y=Emissions,fill=treatment)) + 
            geom_bar(position=position_dodge(), stat="identity") +
            geom_errorbar(aes(ymin=lo, ymax=hi), width=.3, 
                          position=position_dodge(0.9)) +
            theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5,size=16)) +
            ggtitle(title)
    }
    myplot
}

# # s is the AllStats object...  
# createPathwaysPlot <- function(doRanges = FALSE,s = NULL) {
#     y1 <- massageDataforPlot(s$ADstats$confDat, s$b$AD$ADnetEmissions,"AD")
#     y1f <- massageDataforPlot(s$ADfstats$confDat, s$b$ADf$ADnetEmissions,"ADf")
#     y2 <- massageDataforPlot(s$LFstats$confDat, s$b$LF$LandfillNetEmissions,"LF")
#     y3 <- massageDataforPlot(s$CMstats$confDat, s$b$CM$final,"CM")
#     y3f <- massageDataforPlot(s$CMfstats$confDat, s$b$CMf$final,"CMf")
#     y3p <- massageDataforPlot(s$CMpstats$confDat, s$b$CMp$final,"CMp")
#     y3b <- massageDataforPlot(s$CMbstats$confDat, s$b$CMb$final,"CMb")
#     y3Special <- massageDataforPlot(s$CMpstats$confDat, s$b$CMb$final,"CM")
# #     y4 <- massageDataforPlot(s$LAstats$confDat, s$b$LA$EMNetLandapp,"LA")
# #     y4f <- massageDataforPlot(s$LAfstats$confDat, s$b$LAf$EMNetLandapp,"LAf")
#     y5 <- massageDataforPlot(s$AFstats$confDat,s$b$AF$EMAnimalFeed,"AF")
#     
#     
#     #y <- rbind(y1,y1f,y2,y3,y3f,y3p,y4,y4f)
#     #y <- rbind(y1,y2,y3,y4)
#     y <- rbind(y1,y2,y5,y3Special)
#     #y <- rbind(y1,y5,y3Special)
#     #y <- rbind(y1f,y2,y3f,y4f)
#     #y <- rbind(y1,y3,y4)
#     
#     # order by LF
#     y$feedstock <- factor(y$feedstock, levels=y$feedstock[order(y2$Emissions)]) 
#     
#     if(!doRanges) {
#         # Plot Nominals without ranges.
#         myplot <- ggplot(y, aes(x=feedstock, y=Emissions,fill=treatment)) + 
#             geom_bar(position=position_dodge(), stat="identity") +
#             theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5,size=16))
#     } else {
#         # Plot Nominal values
#         myplot <- ggplot(y, aes(x=feedstock, y=Emissions,fill=treatment)) + 
#             geom_bar(position=position_dodge(), stat="identity") +
#             geom_errorbar(aes(ymin=lo, ymax=hi), width=.3, 
#                           position=position_dodge(0.9)) +
#             theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5,size=16))
#     }
#     myplot
# }
# 
# 
# # This is used for sensitivity analysis of compost
# #NOTE: Compost_Peat_Displacement must be changed to a value in the GlobalFactors.csv file
# # s is the AllStats object...  
# createPathwaysCMPlot <- function(doRanges = FALSE,s = NULL) {
# 
#   y3 <- massageDataforPlot(s$CMstats$confDat, s$b$CM$final,"CM")
#   y3f <- massageDataforPlot(s$CMfstats$confDat, s$b$CMf$final,"CMf")
#   y3p <- massageDataforPlot(s$CMpstats$confDat, s$b$CMp$final,"CMp")
#   y3b <- massageDataforPlot(s$CMbstats$confDat, s$b$CMb$final,"CMb")
#   y3la <- massageDataforPlot(s$CMLAstats$confDat, s$b$CMLA$final,"CMLA")
#   
#   
#   y <- rbind(y3,y3f, y3p, y3b, y3la) 
#  
#   
#   # order by emissions
#   y$feedstock <- factor(y$feedstock, levels=y$feedstock[order(y3$Emissions)]) 
#   
#   if(!doRanges) {
#     # Plot Nominals without ranges.
#     myplot <- ggplot(y, aes(x=feedstock, y=Emissions,fill=treatment)) + 
#       geom_bar(position=position_dodge(), stat="identity") +
#       theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5,size=16))
#   } else {
#     # Plot Nominal values
#     myplot <- ggplot(y, aes(x=feedstock, y=Emissions,fill=treatment)) + 
#       geom_bar(position=position_dodge(), stat="identity") +
#       geom_errorbar(aes(ymin=lo, ymax=hi), width=.3, 
#                     position=position_dodge(0.9)) +
#       theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5,size=16))
#   }
#   myplot
# }