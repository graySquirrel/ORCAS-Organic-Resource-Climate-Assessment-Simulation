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
                                            Application = 'noDisplace')
{
    # Step 1: Calculate Land Application  kgCO2e/MT
    EMspread           <- 1.5 * GlobalFactors$xportToField/20
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
    CStorage<-Feedstock$InitialC*(1-Feedstock$fdeg)*(1-GlobalFactors$LA_CSfactor)
    
    EMCstorage<-CStorage*(-44/12)
    if(debug) print(paste("EMCstorage ",EMCstorage))
    
    # Step 5: Displaced fertilizer kgCO2e/MT
    Nremaining      <- Nremaining - 
        Nremaining * GlobalFactors$LandApplication_EF1 -
        Nremaining * 0.02 - Nremaining * 0.2
    effectiveNapplied <- Nremaining * GlobalFactors$LandApp_NAvailabiltiy_Factor
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
