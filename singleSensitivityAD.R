# singleSensitivityAD.R will vary a factor independently until a change in output of
#    a chosen percentage is reached.  Factors come from globalFactors in this analysis.
#setwd("C:/Users/febner/Documents/CourseraDataScience/fracGASM")
source("treatmentClasses.R") 
source("treatmentAnaerobicDigestion.R") 
source("treatmentLandApplication.R") 
#################################################################################
singleValueSensitivityByPercent <- function(feedstock, outPercentThresh, numsamp,
                                            rangePercent, elementName, FUN) 
{
    g1 <- GlobalFactors()
    # input always has single number nominal values.
    # replace it with a vector of samples.
    facVary <- g1[[elementName]] +
        -numsamp:numsamp*rangePercent*g1[[elementName]]/(numsamp*100)
    print(paste("Varying ", elementName, "with nom", g1[[elementName]], "from ", facVary[1], 
                "to", facVary[length(facVary)]))
    g1[[elementName]] <- facVary
    #print(facVary)
    out <- FUN(feedstock, g1, debug = F)
    #print(out)
    nomOut <- out[numsamp+1,1]
    percentdiff <- 100*(out[,1]-nomOut)/nomOut
    final <- cbind(percentdiff,out)
    #print(final)
    within <- which(abs(final[,1]) < outPercentThresh)
    #print(within)
    if (length(within)==dim(out)[[1]]) {
        print("search range too small - never reached difference threshold")
        print(paste("largest percent difference value is",abs(final[1,1])))
        stop("out of range")
    }
    inputNom <- g1[[elementName]][numsamp+1]
    inputAtThresh <- g1[[elementName]][within[1]]
    #print(within)
    #print(final[within[1],])
    outputAtThresh <- final[within[1],1]
    print(paste("Result: for", abs(outputAtThresh), 
                "percent output difference, the factor changed", 
                abs(100*(inputAtThresh-inputNom)/inputNom), "percent."))
    
}
##############################################################################
f1 <- Feedstock(type="GTW",TS=18,VS=17/18,Bo=887,TKN=5600)
singleValueSensitivityByPercent(feedstock=f1,outPercentThresh=20,numsamp=10000,
                                rangePercent=100,elementName='AD_Digester_utilizationFactor',
                                FUN=AnaerobicDigestionTreatmentPathway)