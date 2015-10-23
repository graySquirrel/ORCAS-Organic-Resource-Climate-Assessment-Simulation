# foodwasteTreatmentSim
R scripts to support waste treatment simulations to produce GHG estimates

structure of scripts:


##higher level functions:
####baselines.R
    create outputfile for each feedstock using nominal param values.
####doMonteCarloAnalyses.R
    create nominal and +-95% limits for each feedstock and pathway and make lots of plots
####linModTest.R (and .Rmd)
    create linear models that simplify each pathway to a single equation (for a given set of global parameters)
####sensitivityAnalysisGF.Rmd
    sensitivity analysis of global factors using the pse package
####simpletonSensitivity2.R
    For each pathway, for each feedstock, for each factor, compute the nominal, low, and high values for each factor independently

various other scripts to explore the space
###################################
##unit tests:
    Each pathway can be tested and debugged using the unit test scripts:
####unitTest.R (for AD)
####unitTestAF.R
####unitTestLA.R
####unitTestLandfill.R
####unitTestcompost.R
###################################
##input files:
####GlobalFactors.csv
    file of pathway parameters and their ranges
####Feedstock.csv
    file of feedstock parameters and their ranges
###################################
##output files:
####baselinesOutput.csv
    output of baseline for each pathway
####allPathwaysBreakdowns.csv
    more detailed output includes sub-parts
####lowHighOut.csv
    output of simpletonSensitivity2.R
###################################
##Treatment Pathways:  functions to implement pathways
    All take in a GlobalFactors object and a Feedstock object
    outputs are a dataframe that describe total GHG emissions as well as GHG from different sub-processes. 
    Other optional parameters for displacement and carbon storage.
    Examples of how to use these methods are shown in each respective unitTest.R file

####treatmentAnaerobicDigestion.R
####treatmentcompost.R
####treatmentLandApplication.R
####treatmentLandfill.R
####treatmentAnimalFeed.R
####################################
##Base Classes:
####treatmentClasses.R
    Define base classes used in lots of places:
        GlobalFactors: set of parameters that related to a pathway that are set and varied
        Feedstock: set of parameters that define a feedstock
Examples:  
    i <- read.csv(file="Feedstock.csv",sep = ",",stringsAsFactors=FALSE)  
    f1 <- Feedstock(type=i$Feedstock,TS=i$TS,VS=i$VS,Bo=i$Bo,TKN=i$TKN,
                    percentCarboTS = i$PercentCarboTS, 
                    percentLipidTS = i$PercentlipidTS,
                    percentProteinTS = i$PercentproteinTS, 
                    fdeg = i$fdeg,TDN=i$TDN, 
                    Phosphorus=i$Phosphorus, Potassium=i$Potassium)  
    g1 <- GlobalFactors()
####################################
##Utilities:
####baselineFuncs.R:
    a set of utilities to calculate pathway emissions and to help in plotting
Example:  
   o <- getBaselineResults(verbose = TRUE) # looks for "Feedstock.csv" and "GlobalFactors.csv" in .
####parseGlobalFactors.R:
    a function to read the GlobalFactors csv file, and set the GlobalFactors 
        object to a value or a set of values (used for Monte Carlo)
Example:  
   g1 <- getGlobalFactorsFromFile(doRanges = FALSE, verbose = verbose) # looks for "GlobalFactors.csv" in .
