

# Anaerobic Digestion function.
#    included in simuTreat.R
# Functions
#   AnaerobicDigestionTreatmentPathway(Feedstock, GlobalFactors, 
#                           XportToField = 20, debug = F)
#       returns data.frame of factor vs. outputs, labeled with row and col names
#
################# Treatment Functions
AnaerobicDigestionTreatmentPathway <- function(Feedstock, GlobalFactors, 
                                               XportToField = 20, debug = F)
{
    # Step 1: calculate Digester emissions kgCO2e/MT
    meth       <- Feedstock$Lo * GlobalFactors$AD_Digester_utilizationFactor
    if(debug) print(paste("meth ",meth))
    leakTmp    <- meth * GlobalFactors$AD_Digester_CH4Leaks
    CH4Leaks   <- leakTmp * GlobalFactors$density_CH4 * GlobalFactors$GWPCH4
    incompTmp  <- meth * GlobalFactors$AD_Digester_incompleteCombustion
    incomplete <- incompTmp * GlobalFactors$density_CH4 * GlobalFactors$GWPCH4
    elecTmpTmp <- GlobalFactors$AD_Digester_conversionEfficiency * meth/1000
    if(debug) print(paste("elecTmpTmp ",elecTmpTmp))
    elecTmp    <- elecTmpTmp * (1 - 0.12)
    if(debug) print(paste("elecTmp ",elecTmp))
    avoidedElecEmissions <- elecTmp * GlobalFactors$AD_Digester_avoidedElectricityEmissions
    if(debug) print(paste("CH4Leaks ",CH4Leaks," incomplete ",incomplete,
                          " avoided ",avoidedElecEmissions))
    Digester   <- CH4Leaks + incomplete + avoidedElecEmissions
    if(debug) print(paste("Digester ",Digester))
    
    # Step2: calculate Storage emissions kgCO2e/MT
    TVSinDigestate    <- Feedstock$TVS * GlobalFactors$AD_Storage_reductionInVS
    if(debug) print(paste("TVSinDigestate ",TVSinDigestate))    
    residualMethane   <- TVSinDigestate * GlobalFactors$AD_Storage_residualMethane
    if(debug) print(paste("residualMethane ",residualMethane))    
    effluentEmissions <- residualMethane * GlobalFactors$density_CH4 * GlobalFactors$GWPCH4
    if(debug) print(paste("effluentEmissions ",effluentEmissions))
    directN20         <- Feedstock$TKN * GlobalFactors$N20N_to_N20 * 
        GlobalFactors$GWPN20 * GlobalFactors$AD_Storage_IPCC_EF3 / 1000
    if(debug) print(paste("directN20 ",directN20))
    indirectN20       <- GlobalFactors$AD_Storage_IPCC_EF4timesFracGasm *
        Feedstock$TKN * GlobalFactors$N20N_to_N20 * GlobalFactors$GWPN20 / 1000
    if(debug) print(paste("indirectN20 ",indirectN20))
    N20Emissions      <- directN20 + indirectN20
    if(debug) print(paste("N20Emissions ",N20Emissions))
    Storage           <- effluentEmissions + N20Emissions
    if(debug) print(paste("Storage ",Storage))
    
    # Step 3: Calculate Land Application  kgCO2e/MT
    xport           <- 1.5 * XportToField/20
    if(debug) print(paste("xport ",xport))
    Nremaining      <- Feedstock$TKN - GlobalFactors$AD_Storage_IPCC_EF3 * Feedstock$TKN -
        Feedstock$TKN*0.26
    direct2         <- Nremaining * GlobalFactors$AD_LandApplication_mysteryFactor1 *
        GlobalFactors$N20N_to_N20 * GlobalFactors$GWPN20 / 1000
    if(debug) print(paste("direct2 ",direct2))
    indirect2       <- Nremaining * GlobalFactors$AD_LandApplication_mysteryFactor2 *
        GlobalFactors$N20N_to_N20 * GlobalFactors$GWPN20 / 1000
    if(debug) print(paste("indirect2 ",indirect2))
    N20Emissions    <- direct2 + indirect2
    if(debug) print(paste("N20Emissions ",N20Emissions))
    LandApplication <- xport + N20Emissions
    if(debug) print(paste("LandApplication ",LandApplication))
    
    # Step 4: Displaced fertilizer kgCO2e/MT
    Nremaining      <- Nremaining - 
        Nremaining * GlobalFactors$AD_LandApplication_mysteryFactor1 -
        Nremaining * 0.2
    effectiveNapplied <- Nremaining * 
        GlobalFactors$AD_DisplacedFertilizer_mineralizationFactor
    avoidedNfert    <- GlobalFactors$AD_DisplacedFertilizer_WoodCowieEmissionFactor *
        effectiveNapplied/1000
    avoidedNfertDirectAndIndirect <- GlobalFactors$AD_DisplacedFertilizer_mysteryFactor1 *
        effectiveNapplied/1000
    displacedFertilizer <- avoidedNfert + avoidedNfertDirectAndIndirect
    if(debug) print(paste("displacedFertilizer ",displacedFertilizer))
    
    # Add together
    netEmissions <- 
        Digester +
        Storage +
        LandApplication +
        displacedFertilizer
    names(netEmissions) <- Feedstock$type
    return(netEmissions)
}
