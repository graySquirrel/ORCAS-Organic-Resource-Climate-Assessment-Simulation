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
                                            debug = F, 
                                            Nremaining = Feedstock$TKN/1000)
{
  EMN2O_LandApp_direct         <- Nremaining * 
    (GlobalFactors$LandApplication_EF1 - GlobalFactors$MF_N2O) *
    GlobalFactors$N2ON_to_N2O * GlobalFactors$GWPN2O / 1000
  if(debug) print(paste("EMN2O_LandApp_direct ",EMN2O_LandApp_direct))
  EMN2Ovol       <- Nremaining * 
    (GlobalFactors$AD_LA_FracGasD -GlobalFactors$MF_NH3) *  
    GlobalFactors$IPCC_EF4 *
    GlobalFactors$N2ON_to_N2O * 
    GlobalFactors$GWPN2O / 1000
  EMN2OLRO <- (GlobalFactors$AD_LA_FracLeachD-GlobalFactors$MF_ROL) *
    GlobalFactors$IPCC_EF4 *
    GlobalFactors$N2ON_to_N2O * 
    GlobalFactors$GWPN2O / 1000 
  EMN2O_LandApp_indirect <-EMN2Ovol + EMN2OLRO
  
  if(debug) print(paste("EMN2O_LandApp_indirect ",EMN2O_LandApp_indirect))
  EMN2O_LandApp    <- EMN2O_LandApp_direct + EMN2O_LandApp_indirect
  if(debug) print(paste("EMN2O_LandApp ",EMN2O_LandApp))
  EMLandApp <- EMN2O_LandApp
  if(debug) print(paste("EMLandApp ",EMLandApp))
  
  # Step 2: Carbon Sequestration kgCO2e/MT
  
    CStorage <-Feedstock$InitialC * GlobalFactors$AD_CSfactor
    EMCstorage<-CStorage*(-GlobalFactors$CtoCO2)
    
  
    if(debug) print(paste("InitialC ",Feedstock$InitialC))
    if(debug) print(paste("CStorage ",CStorage))
    if(debug) print(paste("GlobalFactors$CtoCO2 ",GlobalFactors$CtoCO2))
    if(debug) print(paste("EMCstorage ",EMCstorage))
    if(debug) print(paste("fdeg ",Feedstock$fdeg))
  
  # Step 5: Displaced fertilizer kgCO2e/MT
  
  effectiveNapplied <- Nremaining * GlobalFactors$Compost_N_Availability
  if(debug) print(paste("effectiveNapplied ",effectiveNapplied))
  
  EMavoidedNfert    <- (-GlobalFactors$Displaced_N_Production_Factor) * 
    GlobalFactors$N_displacement * effectiveNapplied
  
  # Limit nutrient displacement to nutrient requirements based upon ratio to N 
  
  effectiveKapplied <- Feedstock$Potassium/1000 * GlobalFactors$K_Availability
  MaxK <- effectiveNapplied * GlobalFactors$K_Nratio
  
  for(i in 1:length(effectiveKapplied)) {
    if (effectiveKapplied[i] > MaxK[i])effectiveKapplied[i] <- MaxK[i]
  }
  
  EMavoidedKfert   <- (-GlobalFactors$Displaced_K_Production_Factor) * 
    effectiveKapplied
  
  if(debug) print(paste("EMavoidedKfert ",EMavoidedKfert))
  
  effectivePapplied <- Feedstock$Phosphorus/1000 * GlobalFactors$P_Availability
  if(debug) print(paste("effectivePapplied ",effectivePapplied))
  MaxP <- effectivePapplied * GlobalFactors$P_Nratio
  for(i in 1:length(effectivePapplied)) {
    if (effectivePapplied[i] > MaxP[i])effectivePapplied[i] <- MaxP[i]
  }
  EMavoidedPfert   <-(-GlobalFactors$Displaced_P_Production_Factor) * 
    effectivePapplied  
  
  (-GlobalFactors$LA_DisplacedFertilizer_Direct_Indirect) *
    effectiveNapplied/1000
  
  EMdisplacedFertilizer <- EMavoidedNfert + EMavoidedPfert + EMavoidedKfert
  if(debug) print(paste("displacedFertilizer ",EMdisplacedFertilizer))
  
  # Add together
  EMNetLandapp <-   EMLandApp + EMCstorage + EMdisplacedFertilizer
  

  
  result <- data.frame(EMNetLandapp,EMLandApp,EMCstorage,EMdisplacedFertilizer)
  colnames(result) <- c("EMNetLandapp","EMLandApp","EMCstorage",
                        "EMdisplacedFertilizer")
  result
  }
   
  
