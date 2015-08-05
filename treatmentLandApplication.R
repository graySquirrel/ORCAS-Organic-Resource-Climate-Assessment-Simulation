# Landd Application function.
#
# Functions
#   LandApplicationTreatmentPathway(Feedstock, GlobalFactors, debug = F)
#       returns data.frame of factor vs. outputs, labeled with row and col names
#
################# Treatment Functions
LandApplicationTreatmentPathway <- function(Feedstock, GlobalFactors, debug = F, Ninitial = 0)
{
 
  Nremaining = Ninitial
  LandApplication_FracGasM =0.2
  LandApplication_EF1 = 0.0125
  LandApplication_OtherNFactor = 0.02
  LandApp_NAvailabiltiy_Factor = 0.4
  LA_DisplacedFertilizer_Production_Factor = -6.8
  LA_DisplacedFertilizer_Direct_Indirect = -5.4
  xportToField = 20

    # Step 1: Calculate Land Application  kgCO2e/MT
    EMspread           <- 1.5 * xportToField/20
    if(debug) print(paste("EMspread ",EMspread))
    EMN20_LandApp_direct         <- Nremaining * LandApplication_EF1 *
        GlobalFactors$N20N_to_N20 * GlobalFactors$GWPN20 / 1000
    if(debug) print(paste("EMN20_LandApp_direct ",EMN20_LandApp_direct))
    EMN20_LandApp_indirect       <- Nremaining * LandApplication_FracGasM * GlobalFactors$IPCC_EF4 *
        GlobalFactors$N20N_to_N20 * GlobalFactors$GWPN20 / 1000
    if(debug) print(paste("EMN20_LandApp_indirect ",EMN20_LandApp_indirect))
    EMN20_LandApp    <- EMN20_LandApp_direct + EMN20_LandApp_indirect
    if(debug) print(paste("EMN20_LandApp ",EMN20_LandApp))
    EMLandApp <- EMspread + EMN20_LandApp
    if(debug) print(paste("EMLandApp ",EMLandApp))
    
    # Step 4: Displaced fertilizer kgCO2e/MT
    Nremaining      <- Nremaining - 
        Nremaining * LandApplication_EF1 -
        Nremaining * 0.2
    effectiveNapplied <- Nremaining * 
        LandApp_NAvailabiltiy_Factor
    avoidedNfert    <- LA_DisplacedFertilizer_Production_Factor *
        effectiveNapplied/1000
    avoidedInorganicFertdirectandIndirect <- LA_DisplacedFertilizer_Direct_Indirect *
        effectiveNapplied/1000
    displacedFertilizer <- avoidedNfert + avoidedInorganicFertdirectandIndirect
    if(debug) print(paste("displacedFertilizer ",displacedFertilizer))
    
    # Step 5: Carbon Sequestration kgCO2e/MT
    
    # Add together
    netEmissions <- 
        EMLandApp +
        displacedFertilizer
    names(netEmissions) <- Feedstock$type
    return(netEmissions)
}
