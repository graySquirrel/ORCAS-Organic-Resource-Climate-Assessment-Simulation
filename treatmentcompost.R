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
 
# Hauling of the waste to the compost facility not included at this time
# step 1: Compost operation
  EMCompostoperation<-GlobalFactors$Compost_dieseLlpert*
    (GlobalFactors$DieselprovisionkgCO2eperL+GlobalFactors$DieselcombustionkgCO2eperL)
  if(debug) {print(paste("EMCompostoperation", (EMCompostoperation)))}
  
# step 2: Biological emissions
  #From Boldrin, 2009 based upon percentage of degraded C 
   EMCompost_CH4<-GlobalFactors$CompostPercentCdegraded*GlobalFactors$Compost_degradedC_CH4*Feedstock$InitialC*
     GlobalFactors$CtoCH4*GlobalFactors$GWPCH4
   if(debug) {print(paste("Compost_degradedC_CH4",GlobalFactors$Compost_degradedC_CH4))}
   
   #From Boldrin, 2009, based upon N content
   EMCompost_N2O=Feedstock$Nperton*GlobalFactors$Compost_N2OperN*GlobalFactors$N20N_to_N20*
     GlobalFactors$GWPN20
   if(debug) {print(paste("GlobalFactors$Compost_N2OperN", (GlobalFactors$Compost_N2OperN),"Nperton",(Feedstock$Nperton)))}
   EMBio <- EMCompost_N2O + EMCompost_CH4
   if(debug) {print(paste("EMCompost_CH4", (EMCompost_CH4),"EMCompost_N2O",(EMCompost_N2O)))}
   EMBio <- EMCompost_N2O + EMCompost_CH4
  
#Step 3 Carbon storage
    CStorage<-Feedstock$InitialC * (1-Feedstock$fdeg) * (GlobalFactors$Compost_CS_factor)
    #Assuming that the same amount is stored long term as AD degradability test
    if(debug) {print(paste("CStorage", (CStorage)))}
    EMCstorage<-CStorage * -44/12
    if(debug) {print(paste("EMCstorage", (EMCstorage)))}
    
#Step 4 Compost application
    Compost_mass<- 1000*GlobalFactors$Compost_mass_reduction
    EMspread           <- GlobalFactors$DieselspreadLpertkm * GlobalFactors$Compost_xportToField*
      (GlobalFactors$DieselprovisionkgCO2eperL+GlobalFactors$DieselcombustionkgCO2eperL)
    if(debug) print(paste("EMspread ",EMspread))
    
    Nremaining<-Feedstock$Nperton*(1-GlobalFactors$Compost_N_loss)
    if(debug) print(paste("Nremaining 1 ",Nremaining))
    
    EMN20_CompApp_direct         <- Nremaining * GlobalFactors$Compost_EF1 *
      GlobalFactors$N20N_to_N20 * GlobalFactors$GWPN20
    if(debug) print(paste("EMN20_CompApp_direct ",EMN20_CompApp_direct))
    EMN20_CompApp_indirect       <- Nremaining * GlobalFactors$Compost_FracGasC * 
     GlobalFactors$IPCC_EF4 * GlobalFactors$N20N_to_N20 * GlobalFactors$GWPN20 / 1000
    if(debug) print(paste("EMN20_CompApp_indirect ",EMN20_CompApp_indirect))
    EMN20_CompApp    <- EMN20_CompApp_direct  + EMN20_CompApp_indirect
    if(debug) print(paste("EMN20_CompApp ",EMN20_CompApp))
    EMCompApp <- EMspread + EMN20_CompApp
    if(debug) print(paste("EMCompApp ",EMCompApp))
    
    EMCompost <- EMCompostoperation + EMBio + EMCstorage + EMCompApp
    
    # Step 5: Displaced fertilizer kgCO2e/MT
    effectiveNapplied <- Nremaining * 
      GlobalFactors$NAvailability_Factor
    avoidedNfert    <- GlobalFactors$LA_DisplacedFertilizer_Production_Factor *
      effectiveNapplied
    avoidedInorganicFertdirectandIndirect <- 
        GlobalFactors$LA_DisplacedFertilizer_Direct_Indirect *
      effectiveNapplied
    EM_displacedFertilizer <- avoidedNfert + avoidedInorganicFertdirectandIndirect
    if(debug) print(paste("displacedFertilizer ",EM_displacedFertilizer))
    
    # Step 6: Displaced peat kgCO2e/Mt
   EM_displaced_Peat<-GlobalFactors$Peatdisplacementfactor*Compost_mass*GlobalFactors$EF_Peat_kgCO2eperton/1000
    
   final <- switch(Application,
          'noDisplace' = EMCompost,
          'Fertilizer' = EMCompost + EM_displacedFertilizer,
          'Peat' = EMCompost + EM_displaced_Peat)
   result <- data.frame(final, Application, EMCompost, EMCompostoperation, 
                        EMBio, EMCstorage, EMCompApp, EM_displaced_Peat, 
                        EM_displacedFertilizer)
}
    

