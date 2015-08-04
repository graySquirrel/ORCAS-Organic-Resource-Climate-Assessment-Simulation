# Anaerobic Digestion function.
#    included in simuTreat.R
# Functions
#   AnaerobicDigestionTreatmentPathway(Feedstock, GlobalFactors, 
#                           XportToField = 20, debug = F)
#       returns data.frame of factor vs. outputs, labeled with row and col names
#
################# Treatment Functions
AnaerobicDigestionTreatmentPathway <- function(Feedstock, GlobalFactors, debug = F)
{
    # step 0: Hauling
    # TODO
    # EMTransport <- 
    # Step 1: calculate Digester emissions kgCO2e/MT
    # CH4Produced = Lo
    # CH4Flared = CH4Produced * (1 - GlobalFactors$AD_Digester_utilizationFactor)
    CH4Utilized       <- Feedstock$Lo * GlobalFactors$AD_Digester_utilizationFactor
    if(debug) print(paste("CH4Utilized ",CH4Utilized))
    CH4LeaksM3PerT    <- CH4Utilized * GlobalFactors$AD_Digester_CH4Leaks
    EMLeaks   <- CH4LeaksM3PerT * GlobalFactors$density_CH4 * GlobalFactors$GWPCH4
    CH4ICM3PerT  <- CH4Utilized * GlobalFactors$AD_Digester_CH4incompleteCombustion
    
    EMIC <- CH4ICM3PerT * GlobalFactors$density_CH4 * GlobalFactors$GWPCH4 # +
    #    GlobalFactors$AD_Digester_N20incompleteCombustion * CH4Utilized * GlobalFactors$GWPN20/1000
    
    electricityGenerated <- GlobalFactors$AD_Digester_conversion_KwHPerM3 * CH4Utilized/1000
    if(debug) print(paste("electricityGenerated ",electricityGenerated))
    electricityAvoided    <- electricityGenerated * (1 - GlobalFactors$AD_Digester_parasiticLoad)
    if(debug) print(paste("electricityAvoided ",electricityAvoided))
    EMAvoidedGrid <- electricityAvoided * GlobalFactors$EFGrid
    if(debug) print(paste("EMLeaks ",EMLeaks," EMIC ",EMIC,
                          " avoided ",EMAvoidedGrid))
    EMDigester   <- EMLeaks + EMIC + EMAvoidedGrid
    if(debug) print(paste("EMDigester ",EMDigester))
    
    # Step2: calculate Storage emissions kgCO2e/MT
    TVSDigestate    <- Feedstock$TVS * GlobalFactors$AD_Storage_reductionInVS
    if(debug) print(paste("TVSDigestate ",TVSDigestate))    
    CH4StorageDigestate   <- TVSDigestate * GlobalFactors$AD_Storage_EFresidualMethaneM3CH4PerKgVS
    if(debug) print(paste("CH4StorageDigestate ",CH4StorageDigestate))    
    EMCH4DigestateEmissions <- CH4StorageDigestate * GlobalFactors$density_CH4 * GlobalFactors$GWPCH4
    if(debug) print(paste("EMCH4DigestateEmissions ",EMCH4DigestateEmissions))
    EMN20_storage_Direct         <- Feedstock$TKN * GlobalFactors$N20N_to_N20 * 
        GlobalFactors$GWPN20 * GlobalFactors$AD_Storage_IPCC_EF3 / 1000
    if(debug) print(paste("EMN20_Storage_Direct ",EMN20_storage_Direct))
    EMN20_storage_Indirect       <- GlobalFactors$AD_Storage_IPCC_EF4*GlobalFactors$AD_Storage_IPCC_FracGasMS*
        Feedstock$TKN * GlobalFactors$N20N_to_N20 * GlobalFactors$GWPN20 / 1000
    if(debug) print(paste("EMN20_storage_Indirect ",EMN20_storage_Indirect))
    EMN20_Storage     <- EMN20_storage_Direct + EMN20_storage_Indirect
    if(debug) print(paste("EMN20_Storage ",EMN20_Storage))
    EMStorage           <- EMCH4DigestateEmissions + EMN20_Storage
    if(debug) print(paste("EMStorage ",EMStorage))
    
    # Step 3: Calculate Land Application  kgCO2e/MT
    xport           <- 1.5 * GlobalFactors$XportToField/20
    if(debug) print(paste("xport ",xport))
    Nremaining      <- Feedstock$TKN - GlobalFactors$AD_Storage_IPCC_EF3 * Feedstock$TKN -
        Feedstock$TKN*GlobalFactors$AD_Storage_IPCC_FracGasMS-Feedstock$TKN*GlobalFactors$AD_LandApplication_OtherNFactor
    direct2         <- Nremaining * GlobalFactors$AD_LandApplication_EF1 *
        GlobalFactors$N20N_to_N20 * GlobalFactors$GWPN20 / 1000
    if(debug) print(paste("direct2 ",direct2))
    indirect2       <- Nremaining * GlobalFactors$AD_LandApplication_FracGasM *
        GlobalFactors$N20N_to_N20 * GlobalFactors$GWPN20 / 1000
    if(debug) print(paste("indirect2 ",indirect2))
    EMN20_LandApp    <- direct2 + indirect2
    if(debug) print(paste("EMN20_LandApp ",EMN20_LandApp))
    EMLandApp <- xport + EMN20_LandApp
    if(debug) print(paste("EMLandApp ",EMLandApp))
    
    # Step 4: Displaced fertilizer kgCO2e/MT
    Nremaining      <- Nremaining - 
        Nremaining * GlobalFactors$AD_LandApplication_EF1 -
        Nremaining * 0.2
    effectiveNapplied <- Nremaining * 
        GlobalFactors$AD_LandApp_NAvailabiltiy_Factor
    avoidedNfert    <- GlobalFactors$AD_DisplacedFertilizer_Production_Factor *
        effectiveNapplied/1000
    avoidedInorganicFertdirectandIndirect <- GlobalFactors$AD_DisplacedFertilizer_Direct_Indirect *
        effectiveNapplied/1000
    displacedFertilizer <- avoidedNfert + avoidedInorganicFertdirectandIndirect
    if(debug) print(paste("displacedFertilizer ",displacedFertilizer))
    
    # Step 5: Carbon Sequestration kgCO2e/MT
    
    # Add together
    netEmissions <- 
        EMDigester +
        EMStorage +
        EMLandApp +
        displacedFertilizer
    names(netEmissions) <- Feedstock$type
    return(netEmissions)
}
