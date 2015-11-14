# Landd Application function.
#
# Functions
#   LandApplicationTreatmentPathway(Feedstock, GlobalFactors, debug = F)
#       returns data.frame of factor vs. outputs, labeled with row and col names
#
# Application enumeration:  'noDisplace' = no displacement
#                           'Fertilizer' = Fertilizer displacement
################# Treatment Functions
LandApplicationTreatmentPathway <- function(Feedstock, GlobalFactors, 
                                            debug = F, Nremaining = Feedstock$TKN,
                                            Application = 'noDisplace',
                                            sequesterCarbon = TRUE)
{
    # Step 1: Calculate Land Application  kgCO2e/MT
    EMspread           <- GlobalFactors$DieselspreadLpertkm * GlobalFactors$LA_xportToField*
      (GlobalFactors$DieselprovisionkgCO2eperL+GlobalFactors$DieselcombustionkgCO2eperL)
    if(debug) print(paste("EMspread ",EMspread))
    EMN2O_LandApp_direct         <- Nremaining * GlobalFactors$LandApplication_EF1 *
        GlobalFactors$N2ON_to_N2O * GlobalFactors$GWPN2O / 1000
    if(debug) print(paste("EMN2O_LandApp_direct ",EMN2O_LandApp_direct))
    EMN2O_LandApp_indirect       <- Nremaining * 
        GlobalFactors$LandApplication_FracGasM * 
        GlobalFactors$IPCC_EF4 *
        GlobalFactors$N2ON_to_N2O * GlobalFactors$GWPN2O / 1000
    if(debug) print(paste("EMN2O_LandApp_indirect ",EMN2O_LandApp_indirect))
    EMN2O_LandApp    <- EMN2O_LandApp_direct + EMN2O_LandApp_indirect
    if(debug) print(paste("EMN2O_LandApp ",EMN2O_LandApp))
    EMLandApp <- EMspread + EMN2O_LandApp
    if(debug) print(paste("EMLandApp ",EMLandApp))
    
    # Step 4: Carbon Sequestration kgCO2e/MT
    if (sequesterCarbon == TRUE) {
        CStorage<-Feedstock$InitialC*(1-Feedstock$fdeg)*(GlobalFactors$LA_CSfactor)
        EMCstorage<-CStorage*(-44/12)
    } else {
        EMCstorage <- CStorage <- 0
    }
    if(debug) print(paste("EMCstorage ",EMCstorage))
    
    # Step 5: Displaced fertilizer kgCO2e/MT
    Nremaining      <- Nremaining - 
        Nremaining * GlobalFactors$LandApplication_EF1 -
        Nremaining * 0.02 - Nremaining * 0.2
    effectiveNapplied <- Nremaining * GlobalFactors$Compost_N_Availability
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
}
