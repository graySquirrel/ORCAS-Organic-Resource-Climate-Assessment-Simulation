# Anaerobic Digestion function.
#       returns data.frame of factor vs. outputs, labeled with row and col names
#
# Application enumeration:  'noDisplace' = no displacement
#                           'Fertilizer' = Fertilizer displacement
################# Treatment Functions
AnaerobicDigestionTreatmentPathwaySimp <- function(Feedstock, GlobalFactors, debug = F,
                                                   Application = 'noDisplace',
                                                   sequesterCarbon = TRUE)
{
    #Step 1: calculate Digester emissions kgCO2e/MT
    # Correction from lab scale BMP to commercial AD
    
    # Deduct leaks and flared methane
    
    
    # Digester emissions inlcude leaks and incomplete combustion of methane
    # (N2O emissions are assumed small and neglected) 
    
    # A credit is given for displacement of electricity exported to the grid
    
    # Step2: calculate Storage emissions kgCO2e/MT
    
    #Send to land application trerment
    
    # Step 3 Land Application
    
    
    EMLandApp <- (GlobalFactors$DieselspreadLpertkm * GlobalFactors$AD_xportTofield * (GlobalFactors$DieselprovisionkgCO2eperL+GlobalFactors$DieselcombustionkgCO2eperL)) + ((Feedstock$TKN * (1 - GlobalFactors$AD_Storage_IPCC_EF3 - GlobalFactors$AD_Storage_IPCC_FracGasMS - GlobalFactors$AD_LandApplication_OtherNFactor)) * GlobalFactors$LandApplication_EF1 * GlobalFactors$N20N_to_N20 * GlobalFactors$GWPN20 / 1000) + ((Feedstock$TKN * (1 - GlobalFactors$AD_Storage_IPCC_EF3 - GlobalFactors$AD_Storage_IPCC_FracGasMS - GlobalFactors$AD_LandApplication_OtherNFactor)) * GlobalFactors$LandApplication_FracGasM * GlobalFactors$IPCC_EF4 * GlobalFactors$N20N_to_N20 * GlobalFactors$GWPN20 / 1000)
    
    
    # Step 4: Carbon Sequestration kgCO2e/MT
    # Digestate C applied is calculated through mass balance
    
    EMCstorage <-(Feedstock$InitialC - ((Feedstock$Lo *  GlobalFactors$AD_MCFactor)* GlobalFactors$density_CH4 /GlobalFactors$CtoCH4)- ((((Feedstock$Lo *  GlobalFactors$AD_MCFactor)/GlobalFactors$CH4content) - (Feedstock$Lo *  GlobalFactors$AD_MCFactor))*GlobalFactors$density_CH4 /GlobalFactors$CtoCH4) - (((Feedstock$TVS * (1-GlobalFactors$AD_reductionInVS)*1000) * GlobalFactors$AD_Storage_EFresidualMethaneM3CH4PerKgVS) *GlobalFactors$density_CH4 /GlobalFactors$CtoCH4)) * GlobalFactors$AD_CSfactor*(-44/12)
    
    
    # Step 5: Displaced fertilizer kgCO2e/MT
    
    #only considers N displacement at this time
    
    EMdisplacedFertilizer <- ((GlobalFactors$N_availabilityfactor * ((Feedstock$TKN * (1 - GlobalFactors$AD_Storage_IPCC_EF3 - GlobalFactors$AD_Storage_IPCC_FracGasMS - GlobalFactors$AD_LandApplication_OtherNFactor)) - (Feedstock$TKN * (1 - GlobalFactors$AD_Storage_IPCC_EF3 - GlobalFactors$AD_Storage_IPCC_FracGasMS - GlobalFactors$AD_LandApplication_OtherNFactor)) * GlobalFactors$LandApplication_EF1 - (Feedstock$TKN * (1 - GlobalFactors$AD_Storage_IPCC_EF3 - GlobalFactors$AD_Storage_IPCC_FracGasMS - GlobalFactors$AD_LandApplication_OtherNFactor)) * 0.02 - (Feedstock$TKN * (1 - GlobalFactors$AD_Storage_IPCC_EF3 - GlobalFactors$AD_Storage_IPCC_FracGasMS - GlobalFactors$AD_LandApplication_OtherNFactor)) * 0.2)) * GlobalFactors$LA_DisplacedFertilizer_Production_Factor/1000) + ((GlobalFactors$N_availabilityfactor * ((Feedstock$TKN * (1 - GlobalFactors$AD_Storage_IPCC_EF3 - GlobalFactors$AD_Storage_IPCC_FracGasMS - GlobalFactors$AD_LandApplication_OtherNFactor)) - (Feedstock$TKN * (1 - GlobalFactors$AD_Storage_IPCC_EF3 - GlobalFactors$AD_Storage_IPCC_FracGasMS - GlobalFactors$AD_LandApplication_OtherNFactor)) * GlobalFactors$LandApplication_EF1 - (Feedstock$TKN * (1 - GlobalFactors$AD_Storage_IPCC_EF3 - GlobalFactors$AD_Storage_IPCC_FracGasMS - GlobalFactors$AD_LandApplication_OtherNFactor)) * 0.02 - (Feedstock$TKN * (1 - GlobalFactors$AD_Storage_IPCC_EF3 - GlobalFactors$AD_Storage_IPCC_FracGasMS - GlobalFactors$AD_LandApplication_OtherNFactor)) * 0.2)) * GlobalFactors$LA_DisplacedFertilizer_Direct_Indirect/1000)
    
    
    # Add together
    EMNetLandapp <- switch(Application,
                           'noDisplace' = EMLandApp + EMCstorage,
                           'Fertilizer' = EMLandApp + EMCstorage + EMdisplacedFertilizer)
    
    # Add together - the reaplacement equation is for noDisplace case.  this is the equation we want to fit with lm
    gf <- GlobalFactors
    fs <- Feedstock
    netEmissions <- 
        gf$Dieselgrindpert * gf$DieselprovisionkgCO2eperL + gf$Dieselgrindpert * gf$DieselcombustionkgCO2eperL +    # all constants
        fs$Lo *  gf$AD_MCFactor * gf$AD_Digester_CH4Leaks * gf$density_CH4 * gf$GWPCH4 +                            # AD_MCFactor : AD_Digester_CH4Leaks 
        fs$Lo *  gf$AD_MCFactor * gf$AD_Digester_CH4incompleteCombustion * gf$density_CH4 * gf$GWPCH4 -             # AD_MCFactor : AD_Digester_CH4incompleteCombustion
        fs$Lo *  gf$AD_MCFactor * gf$AD_Digester_CH4incompleteCombustion * gf$AD_Digester_CH4Leaks * gf$density_CH4 * gf$GWPCH4 - # AD_MCFactor : AD_Digester_CH4incompleteCombustion : AD_Digester_CH4Leaks
        fs$Lo *  gf$AD_MCFactor * gf$AD_Digester_CH4incompleteCombustion * gf$AD_flared * gf$density_CH4 * gf$GWPCH4 +  # AD_MCFactor : AD_Digester_CH4incompleteCombustion : AD_flared
        fs$Lo *  gf$AD_MCFactor * gf$AD_Digester_conversion_KwHPerM3 * gf$AD_Digester_OneMinusparasiticLoad * gf$EFGrid /1000 -  # AD_MCFactor : AD_Digester_conversion_KwHPerM3 : AD_Digester_OneMinusparasiticLoad : EFGrid
        fs$Lo *  gf$AD_MCFactor * gf$AD_Digester_conversion_KwHPerM3 * gf$AD_Digester_OneMinusparasiticLoad * gf$EFGrid * gf$AD_Digester_CH4Leaks /1000 - # AD_MCFactor : AD_Digester_conversion_KwHPerM3 : AD_Digester_OneMinusparasiticLoad : EFGrid : AD_Digester_CH4Leaks
        fs$Lo *  gf$AD_MCFactor * gf$AD_Digester_conversion_KwHPerM3 * gf$AD_Digester_OneMinusparasiticLoad * gf$EFGrid * gf$AD_flared /1000  - # AD_MCFactor : AD_Digester_conversion_KwHPerM3 : AD_Digester_OneMinusparasiticLoad : EFGrid : AD_flared
        #fs$Lo *  gf$AD_MCFactor * gf$AD_CSfactor * (-44/12) * gf$density_CH4 / gf$CtoCH4 -                  # 
        fs$Lo *  gf$AD_MCFactor * gf$AD_CSfactor * (-44/12) / gf$CH4content * gf$density_CH4 / gf$CtoCH4 +   # AD_MCFactor : AD_CSfactor
        #fs$Lo *  gf$AD_MCFactor * gf$AD_CSfactor * (-44/12) * gf$density_CH4 / gf$CtoCH4 +                  # 
        
        gf$DieselspreadLpertkm * gf$AD_xportTofield * gf$DieselprovisionkgCO2eperL +                        # DieselspreadLpertkm : AD_xportTofield
        gf$DieselspreadLpertkm * gf$AD_xportTofield * gf$DieselcombustionkgCO2eperL + 
        
        fs$TKN * gf$N20N_to_N20 * gf$GWPN20 *gf$AD_Storage_IPCC_EF3 / 1000 +                                # AD_Storage_IPCC_EF3
        fs$TKN * gf$IPCC_EF4 * gf$AD_Storage_IPCC_FracGasMS * gf$N20N_to_N20 * gf$GWPN20 / 1000 +           # AD_Storage_IPCC_FracGasMS
        fs$TKN * gf$LandApplication_EF1 * gf$N20N_to_N20 * gf$GWPN20 / 1000  -                              # LandApplication_EF1
        fs$TKN * gf$AD_Storage_IPCC_EF3* gf$LandApplication_EF1 * gf$N20N_to_N20 * gf$GWPN20 / 1000 -       # AD_Storage_IPCC_EF3 : LandApplication_EF1
        fs$TKN * gf$AD_Storage_IPCC_FracGasMS* gf$LandApplication_EF1 * gf$N20N_to_N20 * gf$GWPN20 / 1000 - # AD_Storage_IPCC_FracGasMS : LandApplication_EF1
        fs$TKN * gf$AD_LandApplication_OtherNFactor* gf$LandApplication_EF1 * gf$N20N_to_N20 * gf$GWPN20 / 1000 + #
        fs$TKN * gf$LandApplication_FracGasM * gf$IPCC_EF4 * gf$N20N_to_N20 * gf$GWPN20 / 1000  -           # 
        fs$TKN * gf$AD_Storage_IPCC_EF3 * gf$LandApplication_FracGasM * gf$IPCC_EF4 * gf$N20N_to_N20 * gf$GWPN20 / 1000 - # AD_Storage_IPCC_EF3 : LandApplication_FracGasM
        fs$TKN * gf$AD_Storage_IPCC_FracGasMS * gf$LandApplication_FracGasM * gf$IPCC_EF4 * gf$N20N_to_N20 * gf$GWPN20 / 1000 - # AD_Storage_IPCC_FracGasMS : LandApplication_FracGasM
        fs$TKN * gf$AD_LandApplication_OtherNFactor * gf$LandApplication_FracGasM * gf$IPCC_EF4 * gf$N20N_to_N20 * gf$GWPN20 / 1000 + # LandApplication_FracGasM
        
        gf$AD_CSfactor * (-44/12) * fs$InitialC -                                                           # AD_CSfactor
        fs$TVS * gf$AD_OneMinusreductionInVS*1000 * gf$AD_Storage_EFresidualMethaneM3CH4PerKgVS *           # AD_OneMinusreductionInVS : AD_Storage_EFresidualMethaneM3CH4PerKgVS : AD_CSfactor
        gf$density_CH4 * gf$AD_CSfactor * (-44/12)  / gf$CtoCH4 + 
        fs$TVS * gf$AD_OneMinusreductionInVS*1000 * gf$AD_Storage_EFresidualMethaneM3CH4PerKgVS *           # AD_OneMinusreductionInVS : AD_Storage_EFresidualMethaneM3CH4PerKgVS
        gf$density_CH4 * gf$GWPCH4
        
    
    
    result <- data.frame(netEmissions,((((Feedstock$Lo *  GlobalFactors$AD_MCFactor) * GlobalFactors$AD_Digester_CH4Leaks) * GlobalFactors$density_CH4 * GlobalFactors$GWPCH4) + ((((Feedstock$Lo *  GlobalFactors$AD_MCFactor)-  ((Feedstock$Lo *  GlobalFactors$AD_MCFactor) * GlobalFactors$AD_Digester_CH4Leaks) - ((Feedstock$Lo *  GlobalFactors$AD_MCFactor) * GlobalFactors$AD_flared)) * GlobalFactors$AD_Digester_CH4incompleteCombustion) * GlobalFactors$density_CH4 * GlobalFactors$GWPCH4 ) + (GlobalFactors$AD_Digester_conversion_KwHPerM3 *          ((Feedstock$Lo *  GlobalFactors$AD_MCFactor)-  ((Feedstock$Lo *  GlobalFactors$AD_MCFactor) * GlobalFactors$AD_Digester_CH4Leaks) - ((Feedstock$Lo *  GlobalFactors$AD_MCFactor) * GlobalFactors$AD_flared))/1000) * (1 - GlobalFactors$AD_Digester_parasiticLoad) * GlobalFactors$EFGrid),((((Feedstock$TVS * (1-GlobalFactors$AD_reductionInVS)*1000) * GlobalFactors$AD_Storage_EFresidualMethaneM3CH4PerKgVS) * GlobalFactors$density_CH4 * GlobalFactors$GWPCH4) + (Feedstock$TKN * GlobalFactors$N20N_to_N20 * GlobalFactors$GWPN20 *GlobalFactors$AD_Storage_IPCC_EF3 / 1000) + (GlobalFactors$IPCC_EF4 * GlobalFactors$AD_Storage_IPCC_FracGasMS * Feedstock$TKN * GlobalFactors$N20N_to_N20 * GlobalFactors$GWPN20 / 1000)))
    
    
    colnames(result) <- c("ADnetEmissions","EMDigester", "EMStorage")
    result <- cbind(result,EMLandApp,EMCstorage,EMdisplacedFertilizer)
    result
}
