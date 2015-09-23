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
 
    # Step 1: calculate Digester emissions kgCO2e/MT

    CH4Utilized       <- Feedstock$Lo *  GlobalFactors$AD_MCFactor
    if(debug) print(paste("CH4Utilized ",CH4Utilized))
    CH4LeaksM3PerT    <- CH4Utilized * GlobalFactors$AD_Digester_CH4Leaks
    EMLeaks   <- CH4LeaksM3PerT * GlobalFactors$density_CH4 * GlobalFactors$GWPCH4
    CH4ICM3PerT  <- CH4Utilized * GlobalFactors$AD_Digester_CH4incompleteCombustion
    
    EMIC <- CH4ICM3PerT * GlobalFactors$density_CH4 * GlobalFactors$GWPCH4 # +
    #    AD_Digester_N20incompleteCombustion * CH4Utilized * 
    #       GlobalFactors$GWPN20/1000
    
    electricityGenerated <- GlobalFactors$AD_Digester_conversion_KwHPerM3 * 
        CH4Utilized/1000
    if(debug) print(paste("electricityGenerated ",electricityGenerated))
    electricityAvoided    <- electricityGenerated * 
        (1 - GlobalFactors$AD_Digester_parasiticLoad)
    if(debug) print(paste("electricityAvoided ",electricityAvoided))
    EMAvoidedGrid <- electricityAvoided * GlobalFactors$EFGrid
    if(debug) print(paste("EMLeaks ",EMLeaks," EMIC ",EMIC,
                          " avoided ",EMAvoidedGrid))
    EMDigester   <- EMLeaks + EMIC + EMAvoidedGrid
    if(debug) print(paste("EMDigester ",EMDigester))
    
    # Step2: calculate Storage emissions kgCO2e/MT
    TVSDigestate    <- Feedstock$TVS * (1-GlobalFactors$AD_reductionInVS)*1000
    if(debug) print(paste("TVSDigestate ",TVSDigestate))    
    CH4StorageDigestate   <- TVSDigestate *
        GlobalFactors$AD_Storage_EFresidualMethaneM3CH4PerKgVS
    if(debug) print(paste("CH4StorageDigestate ",CH4StorageDigestate))    
    EMCH4DigestateEmissions <- CH4StorageDigestate * GlobalFactors$density_CH4 * 
        GlobalFactors$GWPCH4
    if(debug) print(paste("EMCH4DigestateEmissions ",EMCH4DigestateEmissions))
    EMN20_storage_Direct         <- Feedstock$TKN * GlobalFactors$N20N_to_N20 * 
        GlobalFactors$GWPN20 *GlobalFactors$AD_Storage_IPCC_EF3 / 1000
    if(debug) print(paste("EMN20_Storage_Direct ",EMN20_storage_Direct))
    EMN20_storage_Indirect       <- GlobalFactors$IPCC_EF4 * 
      GlobalFactors$AD_Storage_IPCC_FracGasMS *
        Feedstock$TKN * GlobalFactors$N20N_to_N20 * GlobalFactors$GWPN20 / 1000
    if(debug) print(paste("EMN20_storage_Indirect ",EMN20_storage_Indirect))
    EMN20_Storage     <- EMN20_storage_Direct + EMN20_storage_Indirect
    if(debug) print(paste("EMN20_Storage ",EMN20_Storage))
    EMStorage           <- EMCH4DigestateEmissions + EMN20_Storage
    if(debug) print(paste("EMStorage ",EMStorage))
    
    #Send to land application trerment
    Nremaining      <- Feedstock$TKN * 
      (1 - GlobalFactors$AD_Storage_IPCC_EF3 - 
         GlobalFactors$AD_Storage_IPCC_FracGasMS - 
         GlobalFactors$AD_LandApplication_OtherNFactor)
    #EMLandApp <- LandApplicationTreatmentPathway(Feedstock, GlobalFactors, debug, 
    #Nremaining = Nremaining, 
    # Application = Application)
    
    # Step 3 Land Application
    EMspread           <- GlobalFactors$DieselspreadLpertkm * GlobalFactors$AD_xportTofield*
      (GlobalFactors$DieselprovisionkgCO2eperL+GlobalFactors$DieselcombustionkgCO2eperL)
    if(debug) print(paste("EMspread ",EMspread))
    EMN20_LandApp_direct         <- Nremaining * GlobalFactors$LandApplication_EF1 *
      GlobalFactors$N20N_to_N20 * GlobalFactors$GWPN20 / 1000
    if(debug) print(paste("EMN20_LandApp_direct ",EMN20_LandApp_direct))
    EMN20_LandApp_indirect       <- Nremaining * 
      GlobalFactors$LandApplication_FracGasM * 
      GlobalFactors$IPCC_EF4 *
      GlobalFactors$N20N_to_N20 * GlobalFactors$GWPN20 / 1000
    if(debug) print(paste("EMN20_LandApp_indirect ",EMN20_LandApp_indirect))
    EMN20_LandApp    <- EMN20_LandApp_direct + EMN20_LandApp_indirect
    if(debug) print(paste("EMN20_LandApp ",EMN20_LandApp))
    EMLandApp <- EMspread + EMN20_LandApp
    if(debug) print(paste("EMLandApp ",EMLandApp))
    
    # Step 4: Carbon Sequestration kgCO2e/MT
    CStorage<-Feedstock$InitialC*(1-Feedstock$fdeg)*(GlobalFactors$AD_CSfactor)
    if(debug) print(paste("InitialC ",Feedstock$InitialC))
    EMCstorage<-CStorage*(-44/12)
    if(debug) print(paste("EMCstorage ",EMCstorage))
    if(debug) print(paste("fdeg ",Feedstock$fdeg))
    
    # Step 5: Displaced fertilizer kgCO2e/MT
    Nremaining      <- Nremaining - 
      Nremaining * GlobalFactors$LandApplication_EF1 -
      Nremaining * 0.02 - Nremaining * 0.2
    effectiveNapplied <- Nremaining * GlobalFactors$NAvailability_Factor
    if(debug) print(paste("effectiveNapplied ",effectiveNapplied))
    
    avoidedNfert    <- GlobalFactors$LA_DisplacedFertilizer_Production_Factor *
      effectiveNapplied/1000
    if(debug) print(paste("avoidedNfert ",avoidedNfert))
    
    avoidedInorganicFertdirectandIndirect <- 
      GlobalFactors$LA_DisplacedFertilizer_Direct_Indirect *
      effectiveNapplied/1000
    if(debug) print(paste("avoidedInorganicFertdirectandIndirect ",
                          avoidedInorganicFertdirectandIndirect))
    
    EMdisplacedFertilizer <- avoidedNfert + avoidedInorganicFertdirectandIndirect
    if(debug) print(paste("displacedFertilizer ",EMdisplacedFertilizer))
    
    # Add together
    EMNetLandapp <- switch(Application,
                           'noDisplace' = EMLandApp + EMCstorage,
                           'Fertilizer' = EMLandApp + EMCstorage + EMdisplacedFertilizer)
    result <- data.frame(EMNetLandapp, Application, EMLandApp, EMCstorage, 
                         EMdisplacedFertilizer)
    result
    

    # Add together
    netEmissions <- 
        EMDigester +
        EMStorage +
        EMNetLandapp 
     
    
    result <- data.frame(netEmissions,EMDigester,EMStorage)
    colnames(result) <- c("ADnetEmissions","EMDigester", "EMStorage")
    result <- cbind(result,EMLandApp)
    result
}
