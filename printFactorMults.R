source("treatmentClasses.R")
source("treatmentcompost.R")
source("treatmentAnaerobicDigestion.R")
source("treatmentAnimalFeed.R")
source("treatmentLandfill.R")
source("treatmentLandApplication.R")
source("parseGlobalFactors.R")
source("baselineFuncs.R")

o <- getBaselineResults(verbose = FALSE)
fs <- o$f1
gf <- o$g1

print(paste("L1",gf$AD_MCFactor * gf$AD_Digester_CH4Leaks * gf$density_CH4 * gf$GWPCH4))
print(paste("L2",gf$AD_MCFactor * gf$AD_Digester_CH4incompleteCombustion * gf$density_CH4 * gf$GWPCH4))
print(paste("L3",gf$AD_MCFactor * gf$AD_Digester_CH4incompleteCombustion * gf$AD_Digester_CH4Leaks * gf$density_CH4 * gf$GWPCH4))
print(paste("L4",gf$AD_MCFactor * gf$AD_Digester_CH4incompleteCombustion * gf$AD_flared * gf$density_CH4 * gf$GWPCH4))
print(paste("L5",gf$AD_MCFactor * gf$AD_Digester_conversion_KwHPerM3 * gf$AD_Digester_OneMinusparasiticLoad * gf$EFGrid /1000))
print(paste("L6",gf$AD_MCFactor * gf$AD_Digester_conversion_KwHPerM3 * gf$AD_Digester_OneMinusparasiticLoad * gf$EFGrid * gf$AD_Digester_CH4Leaks /1000))
print(paste("L7",gf$AD_MCFactor * gf$AD_Digester_conversion_KwHPerM3 * gf$AD_Digester_OneMinusparasiticLoad * gf$EFGrid * gf$AD_flared /1000))
print(paste("L8",gf$AD_MCFactor * gf$AD_CSfactor * (-44/12) / gf$CH4content * gf$density_CH4 / gf$CtoCH4))
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