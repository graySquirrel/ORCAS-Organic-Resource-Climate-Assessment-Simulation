# Anaerobic Digestion function.
#    included in simuTreat.R
# Functions
#   AnaerobicDigestionTreatmentPathway(Feedstock, GlobalFactors, debug = F)
#       returns data.frame of factor vs. outputs, labeled with row and col names
#
# Application enumeration:  'noDisplace' = no displacement
#                           'Fertilizer' = Fertilizer displacement
################# Treatment Functions
AnaerobicDigestionTreatmentPathway <- function(Feedstock, GlobalFactors, debug = F,
                                               Application = 'noDisplace')
{
 
  #EFfreight_kgCO2ePERtonKm = 0.107,
  AD_Digester_utilizationFactor = GlobalFactors$AD_Digester_utilizationFactor
  AD_Digester_CH4Leaks = GlobalFactors$AD_Digester_CH4Leaks
  AD_Digester_CH4incompleteCombustion = GlobalFactors$AD_Digester_CH4incompleteCombustion
  #AD_Digester_N20incompleteCombustion = 0.03
  AD_Digester_conversion_KwHPerM3 = GlobalFactors$AD_Digester_conversion_KwHPerM3
  AD_Digester_parasiticLoad = GlobalFactors$AD_Digester_parasiticLoad
  AD_reductionInVS = GlobalFactors$AD_reductionInVS
  AD_Storage_EFresidualMethaneM3CH4PerKgVS = 
      GlobalFactors$AD_Storage_EFresidualMethaneM3CH4PerKgVS
  AD_Storage_IPCC_EF3 = GlobalFactors$AD_Storage_IPCC_EF3
  AD_Storage_IPCC_FracGasMS = GlobalFactors$AD_Storage_IPCC_FracGasMS
  #AD_LandApplication_FracGasM = GlobalFactors$AD_LandApplication_FracGasM
  #AD_LandApplication_EF1 = GlobalFactors$AD_LandApplication_EF1
  AD_LandApplication_OtherNFactor = GlobalFactors$AD_LandApplication_OtherNFactor
  #AD_LandApp_NAvailabiltiy_Factor = GlobalFactors$AD_LandApp_NAvailabiltiy_Factor
  #AD_DisplacedFertilizer_Production_Factor = 
  #    GlobalFactors$AD_DisplacedFertilizer_Production_Factor
  #AD_DisplacedFertilizer_Direct_Indirect = 
  #    GlobalFactors$AD_DisplacedFertilizer_Direct_Indirect
  #xportToField = GlobalFactors$xportToField
  
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
    TVSDigestate    <- Feedstock$TVS * (1-AD_reductionInVS)*1000
    if(debug) print(paste("TVSDigestate ",TVSDigestate))    
    CH4StorageDigestate   <- TVSDigestate * AD_Storage_EFresidualMethaneM3CH4PerKgVS
    if(debug) print(paste("CH4StorageDigestate ",CH4StorageDigestate))    
    EMCH4DigestateEmissions <- CH4StorageDigestate * GlobalFactors$density_CH4 * 
        GlobalFactors$GWPCH4
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
    Nremaining      <- Feedstock$TKN * 
        (1- AD_Storage_IPCC_EF3 - AD_Storage_IPCC_FracGasMS-AD_LandApplication_OtherNFactor)
    EMLandApp <- LandApplicationTreatmentPathway(Feedstock, GlobalFactors, debug, 
                                                 Ninitial = Nremaining, 
                                                 Application = Application)
    # Add together
    netEmissions <- 
        EMDigester +
        EMStorage +
        EMLandApp$EMNetLandapp
    
    result <- data.frame(netEmissions,EMDigester,EMStorage)
    colnames(result) <- c("ADnetEmissions","EMDigester", "EMStorage")
    result <- cbind(result,EMLandApp)
    result
}
