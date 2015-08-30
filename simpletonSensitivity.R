# simpletonSensitivity.R will vary a factor independently until a change in output of
#    a chosen percentage is reached.
#setwd("C:/Users/febner/Documents/CourseraDataScience/fracGASM")
source("treatmentClasses.R") 
source("treatmentAnaerobicDigestion.R") 
source("treatmentLandApplication.R") 

#################################################################################
singleValueSensitivityByPercent <- function(percentThresh,
                                            numsamp,
                                            rangePercent,
                                            nominal,
                                            element,
                                            FUN) {
    g1 <- GlobalFactors()
    my.data<-NULL
    for (i in 1:4) {
        ifelse (element==i, 
                s <- nominal[i] + -numsamp:numsamp*rangePercent*nominal[i]/(numsamp*100),
                s <- nominal[i])
        ifelse (i==1, my.data<- data.frame(s), my.data<-cbind(my.data,s))
    }
    f1 <- Feedstock(type="Sensitivity", my.data[,1], my.data[,2], my.data[,3], my.data[,4])
    out <- cbind(my.data,FUN(f1, g1, debug = F)[[1]])
    #print(head(out))
    nomOut <- out[numsamp+1,5]
    percentdiff <- 100*(out[,5]-nomOut)/nomOut
    final <- cbind(out,percentdiff)
    colnames(final) <- c("TS","VS", "Bo","TKN","out","percentDiff")
    within <- which(abs(final[,6]) < percentThresh)
    if (length(within)==dim(out)[[1]]) {
        print("search range too small - never reached difference threshold")
        print(paste("largest percent difference value is",abs(final[1,6])))
        stop("out of range")
    }
    inputNom <- final[numsamp+1,element]
    inputAtThresh <- final[within[1],element]
    #print(within)
    #print(final[within[1],])
    outputAtThresh <- final[within[1],6]
    print(paste("Result: for", abs(outputAtThresh), "percent output difference, the factor changed", 
                abs(100*(inputAtThresh-inputNom)/inputNom), "percent."))
    
}
##############################################################################
singleValueSensitivityByPercent(20,1000,100,c(18,17/18,887,5600),1,
                                AnaerobicDigestionTreatmentPathway)
singleValueSensitivityByPercent(20,1000,100,c(18,17/18,887,5600),2,
                                AnaerobicDigestionTreatmentPathway)
singleValueSensitivityByPercent(20,1000,100,c(18,17/18,887,5600),3,
                                AnaerobicDigestionTreatmentPathway)
singleValueSensitivityByPercent(1,1000,1000,c(18,17/18,887,5600),4,
                                AnaerobicDigestionTreatmentPathway)

