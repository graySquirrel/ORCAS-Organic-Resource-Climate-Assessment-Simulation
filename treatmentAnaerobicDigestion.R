# Anaerobic Digestion function.
#       returns data.frame of factor vs. outputs, labeled with row and col names
#
# Application enumeration:  'noDisplace' = no displacement
#                           'Fertilizer' = Fertilizer displacement
################# Treatment Functions
AnaerobicDigestionTreatmentPathway <- function(Feedstock, GlobalFactors, debug = F,
                                               Application = 'noDisplace',
                                               sequesterCarbon = TRUE)
{
    # Step 0: Grinding
  EMgrind=GlobalFactors$Dieselgrindpert*
    (GlobalFactors$DieselprovisionkgCO2eperL+GlobalFactors$DieselcombustionkgCO2eperL)
  
  
  #Step 1: calculate Digester emissions kgCO2e/MT
    # Correction from lab scale BMP to commercial AD
    CH4generated <-Feedstock$Lo *  GlobalFactors$AD_MCFactor
    # Deduct leaks and flared methane
    CH4LeaksM3PerT    <- CH4generated * GlobalFactors$AD_Digester_CH4Leaks
    CH4flareM3perT <- CH4generated * GlobalFactors$AD_flared
    CH4Utilized       <- CH4generated-  CH4LeaksM3PerT - CH4flareM3perT

    if(debug) print(paste("CH4Utilized ",CH4Utilized))
    # Digester emissions inlcude leaks and incomplete combustion of methane
    # (N2O emissions are assumed small and neglected) 
    
    EMLeaks   <- CH4LeaksM3PerT * GlobalFactors$density_CH4 * GlobalFactors$GWPCH4
    CH4ICM3PerT  <- CH4Utilized * GlobalFactors$AD_Digester_CH4incompleteCombustion
    EMIC <- CH4ICM3PerT * GlobalFactors$density_CH4 * GlobalFactors$GWPCH4 # +
    #    AD_Digester_N20incompleteCombustion * CH4Utilized * 
    #       GlobalFactors$GWPN20/1000
    
    # A credit is given for displacement of electricity exported to the grid
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
    if (sequesterCarbon == TRUE) {
        #CStorageold<-Feedstock$InitialC*(1-Feedstock$fdeg)*(GlobalFactors$AD_CSfactor)
        
        kgCH4Cproduced <-CH4Utilized* GlobalFactors$density_CH4 *12/16
        # Assumes 60% methane content of biogas
        kgCO2Cproduced <-CH4Utilized/0.6*0.4 *1.98 *12/44
        # Digestate C applied is calculated through mass balance
        DigestateC <-Feedstock$InitialC- CH4LeaksM3PerT -CH4flareM3perT -
          kgCH4Cproduced -kgCO2Cproduced -CH4ICM3PerT -CH4StorageDigestate
        CStorage <-DigestateC * GlobalFactors$AD_CSfactor
        EMCstorage<-CStorage*(-44/12)
        
    } else {
        CStorage <- EMCstorage <- 0
    }
    if(debug) print(paste("InitialC ",Feedstock$InitialC))
    if(debug) print(paste("EMCstorage ",EMCstorage))
    if(debug) print(paste("fdeg ",Feedstock$fdeg))
    if(debug) print(paste("fdeg ",Feedstock$fdeg))
    
    # Step 5: Displaced fertilizer kgCO2e/MT
    Nremaining      <- Nremaining - 
      Nremaining * GlobalFactors$LandApplication_EF1 -
      Nremaining * 0.02 - Nremaining * 0.2
    effectiveNapplied <- Nremaining * GlobalFactors$N_availabilityfactor
    if(debug) print(paste("effectiveNapplied ",effectiveNapplied))
    
    #only considers N displacement at this time
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

    # Add together
    netEmissions <- 
        EMgrind +
        EMDigester +
        EMStorage +
        EMNetLandapp 
     
    
    result <- data.frame(netEmissions,EMDigester,EMStorage)
    colnames(result) <- c("ADnetEmissions","EMDigester", "EMStorage")
    result <- cbind(result,EMLandApp,EMCstorage,EMdisplacedFertilizer)
    result
}
