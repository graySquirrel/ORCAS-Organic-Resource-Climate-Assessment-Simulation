# Parse GlobalFactor.csv and create a global factors variable in the current env
# maybe later we will add it as a function
#
getGlobalFactorsFromFile <- function(GFmemfile = NULL,
                                     doRanges=TRUE,
                                     verbose=FALSE) {
    # We are passing in an in-memory representation of the file contents
    # but if its null, then pull it in from the default file
    if (is.null(GFmemfile)) {
        r <- read.csv(file="GlobalFactors.csv",stringsAsFactors = FALSE)
    }
    else r <- GFmemfile
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
                                           currentValue, "to", as.numeric(newValue)))
                        g1[[currentName]] <- as.numeric(newValue)
                        },
                    "Range" = {
                        if (doRanges) {
                            if(verbose) print(paste("ranging ",currentName," to ",
                                        numSamps,"samples between",
                                        r[[i,"Range.Lo"]], "and", r[[i,"Range.High"]]))
                            g1[[r$sw.name[i]]] <- runif(n = numSamps,
                                                        min = as.numeric(r[[i,"Range.Lo"]]),
                                                        max = as.numeric(r[[i,"Range.High"]]))}
                        else {
                            if(verbose) print(paste("setting ",currentName, "from",
                                        currentValue, "to", as.numeric(newValue)))
                            g1[[currentName]] <- as.numeric(newValue)
                        }},
                    {if(verbose) print(paste("!no action on",currentName))}
            )
        } else {
            if(verbose) print(paste("!No Variable match for:", r$Global.Factors[i]))
        }
    }
    g1
}