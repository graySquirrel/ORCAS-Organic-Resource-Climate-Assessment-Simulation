# Anaerobic Digestion function.
#    included in simuTreat.R
# Functions
#   AnaerobicDigestionTreatmentPathway(Feedstock, GlobalFactors, debug = F)
#       returns data.frame of factor vs. outputs, labeled with row and col names
#
################# Treatment Functions
AnaerobicDigestionTreatmentPathway <- function(Feedstock, GlobalFactors, debug = F)
{
 
  #EFfreight_kgCO2ePERtonKm = 0.107,
  AD_Digester_utilizationFactor = 0.84
  AD_Digester_CH4Leaks = 0.03
  AD_Digester_CH4incompleteCombustion = 0.005
  #AD_Digester_N20incompleteCombustion = 0.03
  AD_Digester_conversion_KwHPerM3 = 4.19318820416827
  AD_Digester_parasiticLoad = 0.12
  AD_reductionInVS = 0.55
  AD_Storage_EFresidualMethaneM3CH4PerKgVS = 0.043 # 0.054
  AD_Storage_IPCC_EF3 = 0.005
  AD_Storage_IPCC_FracGasMS = 0.26
  AD_LandApplication_FracGasM =0.2
  AD_LandApplication_EF1 = 0.0125
  AD_LandApplication_OtherNFactor = 0.02
  AD_LandApp_NAvailabiltiy_Factor = 0.4
  AD_DisplacedFertilizer_Production_Factor = -6.8
  AD_DisplacedFertilizer_Direct_Indirect = -5.4
  xportToField = 20
  
# step 0: Hauling
    # TODO
    # EMTransport <- 
    # Step 1: calculate Digester emissions kgCO2e/MT
    # CH4Produced = Lo
    # CH4Flared = CH4Produced * (1 - AD_Digester_utilizationFactor)
    CH4Utilized       <- Feedstock$Lo * AD_Digester_utilizationFactor
    if(debug) print(paste("CH4Utilized ",CH4Utilized))
    CH4LeaksM3PerT    <- CH4Utilized * AD_Digester_CH4Leaks
    EMLeaks   <- CH4LeaksM3PerT * GlobalFactors$density_CH4 * GlobalFactors$GWPCH4
    CH4ICM3PerT  <- CH4Utilized * AD_Digester_CH4incompleteCombustion
    
    EMIC <- CH4ICM3PerT * GlobalFactors$density_CH4 * GlobalFactors$GWPCH4 # +
    #    AD_Digester_N20incompleteCombustion * CH4Utilized * GlobalFactors$GWPN20/1000
    
    electricityGenerated <- AD_Digester_conversion_KwHPerM3 * CH4Utilized/1000
    if(debug) print(paste("electricityGenerated ",electricityGenerated))
    electricityAvoided    <- electricityGenerated * (1 - AD_Digester_parasiticLoad)
    if(debug) print(paste("electricityAvoided ",electricityAvoided))
    EMAvoidedGrid <- electricityAvoided * GlobalFactors$EFGrid
    if(debug) print(paste("EMLeaks ",EMLeaks," EMIC ",EMIC,
                          " avoided ",EMAvoidedGrid))
    EMDigester   <- EMLeaks + EMIC + EMAvoidedGrid
    if(debug) print(paste("EMDigester ",EMDigester))
    
    # Step2: calculate Storage emissions kgCO2e/MT
    TVSDigestate    <- Feedstock$TVS * AD_reductionInVS
    if(debug) print(paste("TVSDigestate ",TVSDigestate))    
    CH4StorageDigestate   <- TVSDigestate * AD_Storage_EFresidualMethaneM3CH4PerKgVS
    if(debug) print(paste("CH4StorageDigestate ",CH4StorageDigestate))    
    EMCH4DigestateEmissions <- CH4StorageDigestate * GlobalFactors$density_CH4 * GlobalFactors$GWPCH4
    if(debug) print(paste("EMCH4DigestateEmissions ",EMCH4DigestateEmissions))
    EMN20_storage_Direct         <- Feedstock$TKN * GlobalFactors$N20N_to_N20 * 
        GlobalFactors$GWPN20 * AD_Storage_IPCC_EF3 / 1000
    if(debug) print(paste("EMN20_Storage_Direct ",EMN20_storage_Direct))
    EMN20_storage_Indirect       <- GlobalFactors$IPCC_EF4*AD_Storage_IPCC_FracGasMS*
        Feedstock$TKN * GlobalFactors$N20N_to_N20 * GlobalFactors$GWPN20 / 1000
    if(debug) print(paste("EMN20_storage_Indirect ",EMN20_storage_Indirect))
    EMN20_Storage     <- EMN20_storage_Direct + EMN20_storage_Indirect
    if(debug) print(paste("EMN20_Storage ",EMN20_Storage))
    EMStorage           <- EMCH4DigestateEmissions + EMN20_Storage
    if(debug) print(paste("EMStorage ",EMStorage))
    
    # Send to land application trerment
    Nremaining      <- Feedstock$TKN * (1- AD_Storage_IPCC_EF3 - AD_Storage_IPCC_FracGasMS-AD_LandApplication_OtherNFactor)
    EMLandApp <- LandApplicationTreatmentPathway(f1, g1, debug, Ninitial = Nremaining)
    
#     if(debug) print(paste("Nremaining ",Nremaining))
#     ADtreatment (FALSE)
#     EMspread           <- 1.5 * xportToField/20
#     if(debug) print(paste("EMspread ",EMspread))
#     EMN20_LandApp_direct         <- Nremaining * AD_LandApplication_EF1 *
#         GlobalFactors$N20N_to_N20 * GlobalFactors$GWPN20 / 1000
#     if(debug) print(paste("EMN20_LandApp_direct ",EMN20_LandApp_direct))
#     EMN20_LandApp_indirect       <- Nremaining * AD_LandApplication_FracGasM * IPCC_EF4 *
#         GlobalFactors$N20N_to_N20 * GlobalFactors$GWPN20 / 1000
#     if(debug) print(paste("EMN20_LandApp_indirect ",EMN20_LandApp_indirect))
#     EMN20_LandApp    <- EMN20_LandApp_direct + EMN20_LandApp_indirect
#     if(debug) print(paste("EMN20_LandApp ",EMN20_LandApp))
#     EMLandApp <- EMspread + EMN20_LandApp
#     if(debug) print(paste("EMLandApp ",EMLandApp))
#     
#     # Step 4: Displaced fertilizer kgCO2e/MT
#     Nremaining      <- Nremaining - 
#         Nremaining * AD_LandApplication_EF1 -
#         Nremaining * 0.2
#     effectiveNapplied <- Nremaining * 
#         AD_LandApp_NAvailabiltiy_Factor
#     avoidedNfert    <- AD_DisplacedFertilizer_Production_Factor *
#         effectiveNapplied/1000
#     avoidedInorganicFertdirectandIndirect <- AD_DisplacedFertilizer_Direct_Indirect *
#         effectiveNapplied/1000
#     displacedFertilizer <- avoidedNfert + avoidedInorganicFertdirectandIndirect
#     if(debug) print(paste("displacedFertilizer ",displacedFertilizer))
    
    # Step 5: Carbon Sequestration kgCO2e/MT
    
    # Add together
    netEmissions <- 
        EMDigester +
        EMStorage +
        EMLandApp #+
        #displacedFertilizer
    names(netEmissions) <- Feedstock$type
    return(netEmissions)
}
