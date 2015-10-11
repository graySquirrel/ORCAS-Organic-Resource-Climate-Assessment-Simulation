# Parse GlobalFactor.csv and create a global factors variable in the current env
# maybe later we will add it as a function
#
source("treatmentClasses.R") 
getGlobalFactorsFromFile <- function(file = "Globalfactors.csv",
                                     doRanges=TRUE,
                                     verbose=FALSE) {
    r <- read.csv(file=file,stringsAsFactors = FALSE)
    # We create a new global factors object to match names
    g1 <- GlobalFactors()
    for (i in 1:length(r$sw.name)) {
        #     print(r$sw.name[i])
        #     print(g1[[r$sw.name[i]]])
        currentName  <- r$sw.name[i]
        currentValue <- g1[[currentName]]
        newUse   <- r[[i,"Use"]]
        newValue <- r[[i,"Value"]]
        newRangeLo <- r[[i,"Range.Lo"]]
        newRangeHigh <- r[[i,"Range.High"]]
        if (length(currentValue) != 0) {
            #        print(paste("val for: ",r$sw.name[i], " is ", currentValue))
            if(newUse == "Value" && is.na(newValue)) 
                stop(paste("Bad Value for",currentName))
            if (newUse == "Range" && (is.na(newRangeLo) || is.na(newRangeHigh)))
                stop(paste("Bad Range (lo or high) for",currentName))
            switch (newUse,
                    "Value" = {
                        if(verbose) print(paste("setting ",currentName, "from",
                                           currentValue, "to", newValue))
                        g1[[currentName]] <- newValue
                        },
                    "Range" = {
                        if (doRanges) {
                            if(verbose) print(paste("ranging ",currentName," to ",
                                        numSamps,"samples between",
                                        r[[i,"Range.Lo"]], "and", r[[i,"Range.High"]]))
                            g1[[r$sw.name[i]]] <- runif(n = numSamps,
                                                        min = r[[i,"Range.Lo"]],
                                                        max = r[[i,"Range.High"]])}
                        else {
                            if(verbose) print(paste("setting ",currentName, "from",
                                        currentValue, "to", newValue))
                            g1[[currentName]] <- newValue
                        }},
                    {if(verbose) print(paste("!no action on",currentName))}
            )
        } else {
            if(verbose) print(paste("!No Variable match for:", r$Global.Factors[i]))
        }
    }
    g1
}