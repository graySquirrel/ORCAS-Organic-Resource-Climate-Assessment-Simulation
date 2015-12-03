# compost function.
# Functions
#   compostTreatmentPathway(Feedstock, GlobalFactors, debug = F)
#       returns data.frame of factor vs. outputs, labeled with row and col names
#
# Application enumeration:  'noDisplace' = no displacement
#                           'Fertilizer' = Fertilizer displacement
#                           'Peat' = Peat displacement
#                           'Blended' = 21% peat, 18% fertilizer, 61% no displacement
#  'LAFertilizer' = LA Fertilizer displacement

################# Treatment Functions
compostTreatmentPathway <- function(Feedstock, GlobalFactors, Application = 'Blended', 
                                    debug = F,
                                    sequesterCarbon = TRUE)
{
    
    # Step 1: Compost process-Direct and indirect upstream fossil fuel
    EMCompostoperation<-GlobalFactors$Compost_dieseLlpert*
        (GlobalFactors$DieselprovisionkgCO2eperL+GlobalFactors$DieselcombustionkgCO2eperL) + 
        GlobalFactors$Compost_electpert * GlobalFactors$EFGrid/1000
    if(debug) {print(paste("EMCompostoperation", (EMCompostoperation)))}
    
    # Step 2: Coompost process -Biological emissions
    #CH4 based upon percentage of degraded C 
    EMCompost_CH4<-GlobalFactors$CompostPercentCdegraded * 
        GlobalFactors$Compost_degradedC_CH4 *
        Feedstock$InitialC * GlobalFactors$CtoCH4*GlobalFactors$GWPCH4
    if(debug) {print(paste("Compost_degradedC_CH4",GlobalFactors$Compost_degradedC_CH4))}
    
    #N2O direct based upon N content
    EMCompost_N2Odirect <-Feedstock$Nperton*GlobalFactors$Compost_N2OperN*
        GlobalFactors$N2ON_to_N2O*GlobalFactors$GWPN2O
    #N2O indirect assuming NH3 is 50% of nonN20 
    Nloss <- Feedstock$Nperton * GlobalFactors$Compost_N_loss-
        Feedstock$Nperton * GlobalFactors$Compost_N2OperN
    NH3 <- GlobalFactors$Compost_NH3ofloss * Nloss
    EMCompost_N2Oindirect <-NH3 * GlobalFactors$IPCC_EF4 *
        GlobalFactors$N2ON_to_N2O*GlobalFactors$GWPN2O
    
    EMCompost_N2O <- EMCompost_N2Odirect + EMCompost_N2Oindirect
    if(debug) {print(paste("GlobalFactors$Compost_N2OperN", (GlobalFactors$Compost_N2OperN),"Nperton",(Feedstock$Nperton)))}
    
    EMBio <- EMCompost_N2O + EMCompost_CH4
    if(debug) {print(paste("EMCompost_CH4", (EMCompost_CH4),"EMCompost_N2O",(EMCompost_N2O)))}
    
    
    EMCompost <- EMCompostoperation + EMBio
    
    ## #Displacement due to compost use
    
    # Step 3: Displaced fertilizer kgCO2e/MT
    Nremaining<-Feedstock$Nperton * (1-GlobalFactors$Compost_N_loss)
    if(debug) print(paste("Nremaining 1 ",Nremaining))
    
    
    #Calculates the amount of plant available nutrient
    
    effectiveNapplied <- Nremaining * GlobalFactors$Compost_N_Availability
    effectiveKapplied <- Feedstock$Potassium/1000 * GlobalFactors$K_Availability
    effectivePapplied <- Feedstock$Phosphorus/1000 * GlobalFactors$P_Availability
    
    # Assume displacement of all available nutrients
    
    EM_displacedFertilizer   <- 
        (-GlobalFactors$Displaced_N_Production_Factor) * effectiveNapplied +
        (-GlobalFactors$Displaced_P_Production_Factor) * effectivePapplied +
        (-GlobalFactors$Displaced_K_Production_Factor) * effectiveKapplied
    
    if(debug) print(paste("EM_displacedFertilizer ",EM_displacedFertilizer))
    
    
    # Step 4: Displaced peat kgCO2e/Mt
    Compost_mass<- 1000*(1-GlobalFactors$Compost_mass_reduction)
    EM_displaced_Peat <- (GlobalFactors$Peat_substitution) * 
        Compost_mass*(-GlobalFactors$EF_Peat_kgCO2eperton)/1000
    
    
    # Step 5: Land applied to displace fertilizer in agriculture
   
    #Assumes that fertilizer spreading was similar impact to commercial fertilizer 
    # and thus not included
    # EMspread  <- GlobalFactors$DieselspreadLpertkm * GlobalFactors$Compost_xportToField     # *(GlobalFactors$DieselprovisionkgCO2eperL + 
    #GlobalFactors$DieselcombustionkgCO2eperL)
    #if(debug) print(paste("EMspread ",EMspread))
    
    # Nitrous losses relative to commercial fertilizers
    # Direct N2O emissions 
    EMN2O_CompApp_direct  <- Nremaining * 
      (GlobalFactors$Compost_EF1-GlobalFactors$MF_N2O) *
      GlobalFactors$N2ON_to_N2O * 
      GlobalFactors$GWPN2O
    if(debug) print(paste("EMN2O_CompApp_direct ",EMN2O_CompApp_direct))
    # Indirect N2O
    
    # NH3 volatilization compost vs commercial fertilizer
    EMN2O_CompApp_indirectvol <- Nremaining * 
      (GlobalFactors$Compost_FracGasC-GlobalFactors$MF_NH3) * 
      GlobalFactors$IPCC_EF4 * 
      GlobalFactors$N2ON_to_N2O * 
      GlobalFactors$GWPN2O / 1000
    # Leaching and Runoff versus commercial fertilizer
    EMN2O_CompApp_indirectLRO <-Nremaining * 
      (GlobalFactors$Compost_LRO-GlobalFactors$MF_ROL) * 
      GlobalFactors$IPCC_EF4 * 
      GlobalFactors$N2ON_to_N2O * 
      GlobalFactors$GWPN2O / 1000
    
    EMN2O_CompApp_indirect <- EMN2O_CompApp_indirectvol + EMN2O_CompApp_indirectLRO
    
    if(debug) print(paste("EMN2O_CompApp_indirect ",EMN2O_CompApp_indirect))
    if(debug) print(paste("EMN2O_CompApp_indirectvol ",EMN2O_CompApp_indirectvol))
    if(debug) print(paste("EMN2O_CompApp_indirectLRO ",EMN2O_CompApp_indirectLRO))
    EMN2O_CompApp    <- EMN2O_CompApp_direct  + EMN2O_CompApp_indirect
    if(debug) print(paste("EMN2O_CompApp ",EMN2O_CompApp))
    
    #Don't need if don't count spreading
    #EMCompApp <- EMN2O_CompApp
    #if(debug) print(paste("EMCompApp ",EMCompApp))
    
    # Subtract out N losses due to land application
    NremainingLA <- Nremaining * (1- GlobalFactors$Compost_EF1 - 
                                      GlobalFactors$Compost_FracGasC - 
                                      GlobalFactors$Compost_LRO )
    #     if (NremainingLA < 0) {
    #       NremainingLA = 0
    #     }
    NremainingLA[NremainingLA < 0] <- 0 # how to do the if with vectors
    
    effectiveNappliedLA <- NremainingLA * GlobalFactors$Compost_N_Availability
    if(debug) print(paste("effectiveNappliedLA ",effectiveNappliedLA))
    
    # N_displacement is a factor from 0 to 1 to set N fert substitution
    EMavoidedNfertLA    <- (-GlobalFactors$Displaced_N_Production_Factor) * 
      effectiveNappliedLA * GlobalFactors$N_displacement 
    if(debug) print(paste("EMavoidedNfertLA ",EMavoidedNfertLA))
    
    
    # Limit nutrient displacement to nutrient requirements based upon ratio to N  
    MaxK <- effectiveNapplied * GlobalFactors$K_Nratio
    for(i in 1:length(effectiveKapplied))
        if (effectiveKapplied[i] > MaxK[i]) effectiveKapplied[i] <- MaxK[i]
    
    EMavoidedKfert   <- (-GlobalFactors$Displaced_K_Production_Factor) * 
      effectiveKapplied
    
    if(debug) print(paste("EMavoidedKfert ",EMavoidedKfert))
    
    
    MaxP <- effectivePapplied * GlobalFactors$P_Nratio
    for(i in 1:length(effectivePapplied))
        if (effectivePapplied[i] > MaxP[i]) effectivePapplied[i] <- MaxP[i]
    
    EMavoidedPfert   <- (-GlobalFactors$Displaced_P_Production_Factor)  * 
        effectivePapplied 
    
    #avoidedInorganicFertdirectandIndirect <- 
        #GlobalFactors$LA_DisplacedFertilizer_Direct_Indirect *
       # effectiveNapplied
    #if(debug) print(paste("avoidedInorganicFertdirectandIndirect ",
                         # avoidedInorganicFertdirectandIndirect))
    
    
    EMdisplacedFertilizerLA <- EMavoidedNfertLA + EMavoidedPfert + EMavoidedKfert
    #+ avoidedInorganicFertdirectandIndirect +
       
    if(debug) print(paste("displacedFertilizer ",EMdisplacedFertilizerLA))
    
    #Step 5 Carbon storage
    if (sequesterCarbon == TRUE) {
      CompostC <- Feedstock$InitialC * (1-GlobalFactors$CompostPercentCdegraded)
      CStorage<- CompostC * (GlobalFactors$Compost_CS_factor)
      #Assuming that the same amount is stored long term as AD degradability test
      EMCstorage<-CStorage * -44/12
    } else {
      EMCstorage <- CStorage <- 0
    }
    if(debug) {print(paste("CStorage", (CStorage)))}
    if(debug) {print(paste("EMCstorage", (EMCstorage)))}
    
    #avoidedInorganicFertdirectandIndirect <- 
        #GlobalFactors$LA_DisplacedFertilizer_Direct_Indirect * effectiveNappliedLA
    
#     EM_displacedFertilizerLA <- avoidedNfertLA + avoidedInorganicFertdirectandIndirect # + EMspread 
    # if(debug) print(paste("displacedFertilizer ",EM_displacedFertilizer))
    
    final <- switch(Application,
                    'noDisplace' = EMCompost,
                    'Fertilizer' = EMCompost + EM_displacedFertilizer,
                    'Peat' = EMCompost + EM_displaced_Peat *
                        GlobalFactors$Compost_Peat_Displacement,
                    'Blended' = EMCompost + 0.21*EM_displaced_Peat + 
                      0.18*EM_displacedFertilizer,
                    'LAFertilizer' =EMCompost + EMdisplacedFertilizerLA)
    result <- data.frame(final, Application, EMCompost, EMCompostoperation, 
                         EMBio, EMCstorage,  EM_displaced_Peat, 
                         EM_displacedFertilizer, EMdisplacedFertilizerLA)
}


