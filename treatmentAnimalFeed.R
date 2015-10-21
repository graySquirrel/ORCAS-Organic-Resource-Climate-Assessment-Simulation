# Animal Feed pathway.
# Functions
#   AnimalFeedTreatmentPathway(Feedstock, GlobalFactors, debug = F)
#       returns data.frame of factor vs. outputs, labeled with row and col names
#
############################################################################3
AnimalFeedTreatmentPathway <- function(Feedstock, GlobalFactors, debug = F)
{
    AF_DisplacementFactor <- (Feedstock$TS * Feedstock$TDN) /
        (GlobalFactors$CornTS * GlobalFactors$CornTDN)
    if (debug) print(paste("AF_DisplacementFactor", AF_DisplacementFactor))
    EMAnimalFeed <- (1-GlobalFactors$AF_loss) *
        AF_DisplacementFactor * GlobalFactors$EFCornDisplacement
    
    result <- data.frame(EMAnimalFeed)
}