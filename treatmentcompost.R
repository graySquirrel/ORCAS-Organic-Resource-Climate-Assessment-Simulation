# compost function.
# Functions
#   compostTreatmentPathway(Feedstock, GlobalFactors, debug = F)
#       returns data.frame of factor vs. outputs, labeled with row and col names
#
# Application enumeration:  'noDisplace' = no displacement
#                           'Fertilizer' = Fertilizer displacement
#                           'Peat' = Peat displacement
################# Treatment Functions
compostTreatmentPathway <- function(Feedstock, GlobalFactors, Application = 'noDisplace', debug = F)
{
  Compost_EF1 = 0.015 # Boldrin
  Compost_dieseLlpert<-3
  #Boldrin,2009
  CompostPercentCdegraded<-0.58
  #Boldrin,2009
  Compost_degradedC_CH4<-0.02
  #Boldrin,2009
  Compost_N2OperN<-0.015
  Compost_storage_factor<-0.00
# Additional decay beyond AD degradation tests due to organisms and fungi
  Compost_mass_reduction=0.4
  Compost_spread_diesellpert<-0.4
  Compost_N_remaining<-0.4
  Compost_NAvailabiltiy_Factor<-0.4
  xportToField<-60
  Peatdisplacementfactor<-1
  EF_Peat_kgCO2eperton<- -970
# Hauling of the waste to the compost facility not included at this time
# step 1: Compost operation
  EMCompostoperation<-Compost_dieseLlpert*
    (GlobalFactors$DieselprovisionkgCO2eperL+GlobalFactors$DieselcombustionkgCO2eperL)
  if(debug) {print(paste("EMCompostoperation", (EMCompostoperation)))}
  
# step 2: Biological emissions
   EMCompost_CH4<-CompostPercentCdegraded*Compost_degradedC_CH4*Feedstock$InitialC*
     GlobalFactors$CtoCH4*GlobalFactors$GWPCH4
   if(debug) {print(paste("Compost_degradedC_CH4", Compost_degradedC_CH4))}
   
   EMCompost_N2O=Feedstock$Nperton*Compost_N2OperN*GlobalFactors$N20N_to_N20*
     GlobalFactors$GWPN20
   if(debug) {print(paste("EMCompost_CH4", (EMCompost_CH4),"EMCompost_N2O",(EMCompost_N2O)))}
   EMBio <- EMCompost_N2O + EMCompost_CH4
  
#Step 3 Carbon storage
    CStorage<-Feedstock$InitialC*(1-(Feedstock$fdeg+Compost_storage_factor))
    #Assuming that the same amount is stored long term as AD degradability test
    if(debug) {print(paste("CStorage", (CStorage)))}
    EMCstorage<-CStorage*-44/12
    if(debug) {print(paste("EMCstorage", (EMCstorage)))}
    
#Step 4 Compost application
    Compost_mass<- 1000*Compost_mass_reduction
    EMspread           <- 1.5 * xportToField/20
    if(debug) print(paste("EMspread ",EMspread))
    Nremaining<-Feedstock$Nperton*Compost_N_remaining
    if(debug) print(paste("Nremaining 1 ",Nremaining))
    
    EMN20_LandApp_direct         <- Nremaining * Compost_EF1 *
      GlobalFactors$N20N_to_N20 * GlobalFactors$GWPN20
    if(debug) print(paste("EMN20_LandApp_direct ",EMN20_LandApp_direct))
    #EMN20_LandApp_indirect       <- Nremaining * GlobalFactors$LandApplication_FracGasM * 
    #  GlobalFactors$IPCC_EF4 * GlobalFactors$N20N_to_N20 * GlobalFactors$GWPN20 / 1000
    #if(debug) print(paste("EMN20_LandApp_indirect ",EMN20_LandApp_indirect))
    EMN20_LandApp    <- EMN20_LandApp_direct # + EMN20_LandApp_indirect
    if(debug) print(paste("EMN20_LandApp ",EMN20_LandApp))
    EMLandApp <- EMspread + EMN20_LandApp
    if(debug) print(paste("EMLandApp ",EMLandApp))
    
    EMCompost <- EMCompostoperation + EMBio + EMCstorage + EMLandApp
    
    # Step 5: Displaced fertilizer kgCO2e/MT
    effectiveNapplied <- Nremaining * 
      Compost_NAvailabiltiy_Factor
    avoidedNfert    <- GlobalFactors$LA_DisplacedFertilizer_Production_Factor *
      effectiveNapplied
    avoidedInorganicFertdirectandIndirect <- GlobalFactors$LA_DisplacedFertilizer_Direct_Indirect *
      effectiveNapplied
    EM_displacedFertilizer <- avoidedNfert + avoidedInorganicFertdirectandIndirect
    if(debug) print(paste("displacedFertilizer ",EM_displacedFertilizer))
    
    # Step 6: Displaced peat kgCO2e/Mt
   EM_displaced_Peat<-Peatdisplacementfactor*Compost_mass*EF_Peat_kgCO2eperton/1000
    
   final <- switch(Application,
          'noDisplace' = EMCompost,
          'Fertilizer' = EMCompost + EM_displacedFertilizer,
          'Peat' = EMCompost + EM_displaced_Peat)
   result <- data.frame(final, Application, EMCompost, EMCompostoperation, 
                        EMBio, EMCstorage, EMLandApp, EM_displaced_Peat, EM_displacedFertilizer)
}
    

