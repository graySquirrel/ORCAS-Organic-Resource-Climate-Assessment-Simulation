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
    #only if high solids not implemented
    # EMgrind <- switch(TS,
    # < '.07',= 0,
    # > '0.7', =GlobalFactors$Dieselgrindpert*
    #  (GlobalFactors$DieselprovisionkgCO2eperL+GlobalFactors$DieselcombustionkgCO2eperL)
    
    EMgrind=GlobalFactors$Dieselgrindpert*
        (GlobalFactors$DieselprovisionkgCO2eperL+GlobalFactors$DieselcombustionkgCO2eperL)
    
    
    #Step 1: calculate Digester emissions kgCO2e/MT
    # Correction from lab scale BMP to commercial AD
    CH4prod <-Feedstock$Lo *  GlobalFactors$AD_Cf
    # Deduct leaks and flared methane
    CH4LeaksM3PerT    <- CH4prod * GlobalFactors$AD_Digester_CH4Leaks
    CH4flareM3perT <- CH4prod * GlobalFactors$AD_flared
    CH4Utilized       <- CH4prod-  CH4LeaksM3PerT - CH4flareM3perT
    
    if(debug) print(paste("CH4Utilized ",CH4Utilized))
    # Digester emissions inlcude leaks and incomplete combustion of methane
    # (N2O emissions are assumed small and neglected) 
    
    EMLeaks   <- CH4LeaksM3PerT * GlobalFactors$density_CH4 * 
        GlobalFactors$GWPCH4
    CH4ICM3PerT  <- CH4Utilized * GlobalFactors$AD_Digester_CH4IC
    EMIC <- CH4ICM3PerT * GlobalFactors$density_CH4 * GlobalFactors$GWPCH4 # +
    #    AD_Digester_N2OincompleteCombustion * CH4Utilized * 
    #       GlobalFactors$GWPN2O/1000
    
    # A credit is given for displacement of electricity exported to the grid
    electricityGenerated <- GlobalFactors$AD_Digester_CE * 
        CH4Utilized/1000
    if(debug) print(paste("electricityGenerated ",electricityGenerated))
    electricityAvoided    <- electricityGenerated * 
        (1 - GlobalFactors$AD_Digester_parasiticLoad)
    if(debug) print(paste("electricityAvoided ",electricityAvoided))
    EMAvoidedGrid <- electricityAvoided * (-GlobalFactors$EFGrid)
    if(debug) print(paste("EMLeaks ",EMLeaks," EMIC ",EMIC,
                          " avoided ",EMAvoidedGrid))
    EMDigester   <- EMLeaks + EMIC + EMAvoidedGrid
    if(debug) print(paste("EMDigester ",EMDigester))
    
    # Step2: calculate Storage emissions kgCO2e/MT
    TVSDigestate    <- Feedstock$TVS * (1-GlobalFactors$AD_reductionInVS)*1000
    if(debug) print(paste("TVSDigestate ",TVSDigestate))    
    CH4StorageDigestate   <- TVSDigestate *
        GlobalFactors$AD_Storage_residualCH4
    if(debug) print(paste("CH4StorageDigestate ",CH4StorageDigestate))    
    EMCH4DigestateEmissions <- CH4StorageDigestate * GlobalFactors$density_CH4 * 
        GlobalFactors$GWPCH4
    if(debug) print(paste("EMCH4DigestateEmissions ",EMCH4DigestateEmissions))
    EMN2O_storage_Direct         <- Feedstock$TKN * GlobalFactors$N2ON_to_N2O * 
        GlobalFactors$GWPN2O *GlobalFactors$AD_Storage_EF3 / 1000
    if(debug) print(paste("EMN2O_Storage_Direct ",EMN2O_storage_Direct))
    EMN2O_storage_Indirect       <- GlobalFactors$IPCC_EF4 * 
        GlobalFactors$AD_Storage_FracGasMS *
        Feedstock$TKN * GlobalFactors$N2ON_to_N2O * GlobalFactors$GWPN2O / 1000
    if(debug) print(paste("EMN2O_storage_Indirect ",EMN2O_storage_Indirect))
    EMN2O_Storage     <- EMN2O_storage_Direct + EMN2O_storage_Indirect
    if(debug) print(paste("EMN2O_Storage ",EMN2O_Storage))
    EMStorage           <- EMCH4DigestateEmissions + EMN2O_Storage
    if(debug) print(paste("EMStorage ",EMStorage))
    
    # Mass balance to calculate N applied to field
    Nremaining      <- Feedstock$Nperton * 
        (1 - GlobalFactors$AD_Storage_EF3 - 
             GlobalFactors$AD_Storage_FracGasMS -
             GlobalFactors$AD_LandApplication_OtherNFactor)
    #EMLandApp <- LandApplicationTreatmentPathway(Feedstock, GlobalFactors, debug, 
    #Nremaining = Nremaining, 
    # Application = Application)
    
    # Step 3 Land Application
    EMspread           <- GlobalFactors$DieselspreadLpertkm * GlobalFactors$AD_xportTofield*
        (GlobalFactors$DieselprovisionkgCO2eperL+GlobalFactors$DieselcombustionkgCO2eperL)
    if(debug) print(paste("EMspread ",EMspread))
    EMN2O_LandApp_direct         <- Nremaining * GlobalFactors$LandApplication_EF1 *
        GlobalFactors$N2ON_to_N2O * GlobalFactors$GWPN2O / 1000
    if(debug) print(paste("EMN2O_LandApp_direct ",EMN2O_LandApp_direct))
    EMN2O_LandApp_indirect       <- Nremaining * 
        GlobalFactors$AD_LA_FracGasD *  GlobalFactors$IPCC_EF4 *
        GlobalFactors$N2ON_to_N2O * GlobalFactors$GWPN2O / 1000 +
        GlobalFactors$AD_LA_FracLeachD * GlobalFactors$IPCC_EF4 *
        GlobalFactors$N2ON_to_N2O * GlobalFactors$GWPN2O / 1000 
    if(debug) print(paste("EMN2O_LandApp_indirect ",EMN2O_LandApp_indirect))
    EMN2O_LandApp    <- EMN2O_LandApp_direct + EMN2O_LandApp_indirect
    if(debug) print(paste("EMN2O_LandApp ",EMN2O_LandApp))
    EMLandApp <- EMspread + EMN2O_LandApp
    if(debug) print(paste("EMLandApp ",EMLandApp))
    
    # Step 4: Carbon Sequestration kgCO2e/MT
    if (sequesterCarbon == TRUE) {
        # Digestate C applied is calculated through mass balance
        BiogasM3pert<-CH4prod/GlobalFactors$CH4content
        kgCH4Cbiogas <-CH4prod* GlobalFactors$density_CH4 / GlobalFactors$CtoCH4
        CO2M3biogas <- BiogasM3pert - CH4prod
        kgCO2Cbiogas <- CO2M3biogas*GlobalFactors$density_CO2 / GlobalFactors$CtoCO2
        kgCH4StorageDigestate <- CH4StorageDigestate *GlobalFactors$density_CH4 /GlobalFactors$CtoCH4
        
        DigestateC <- Feedstock$InitialC - kgCH4Cbiogas- kgCO2Cbiogas - kgCH4StorageDigestate 
        - CH4ICM3PerT * GlobalFactors$density_CH4
        CStorage <-DigestateC * GlobalFactors$AD_CSfactor
        EMCstorage<-CStorage*(-GlobalFactors$CtoCO2)
        
    } else {
        CStorage <- EMCstorage <- 0
    }
    if(debug) print(paste("InitialC ",Feedstock$InitialC))
    if(debug) print(paste("EMCstorage ",EMCstorage))
    if(debug) print(paste("fdeg ",Feedstock$fdeg))
    
    # Step 5: Displaced fertilizer kgCO2e/MT
    Nremaining      <- Nremaining *(1-
                                        GlobalFactors$LandApplication_EF1 -
                                        GlobalFactors$AD_LA_FracGasD -
                                        GlobalFactors$AD_LA_FracLeachD)
    
    #         if (Nremaining < 0) {
    #           Nremaining = 0
    #           }
    Nremaining[Nremaining < 0] <- 0 # set all non pos to 0 in vector.
    
    effectiveNapplied <- Nremaining * GlobalFactors$AD_N_Availability
    if(debug) print(paste("effectiveNapplied ",effectiveNapplied))
    
    avoidedNfert    <- GlobalFactors$Displaced_N_Production_Factor * 
        GlobalFactors$N_displacement * effectiveNapplied
    
    # Limit nutrient displacement to nutrient requirements based upon ratio to N 
    
    effectiveKapplied <- Feedstock$Potassium/1000 * GlobalFactors$K_Availability
    MaxK <- effectiveNapplied * GlobalFactors$K_Nratio
    for(i in 1:length(effectiveKapplied)) {
        if (effectiveKapplied[i] > MaxK[i])effectiveKapplied[i] <- MaxK[i]
    }
    
    avoidedKfert   <-GlobalFactors$Displaced_K_Production_Factor * 
        effectiveKapplied
    
    if(debug) print(paste("avoidedKfert ",avoidedKfert))
    
    effectivePapplied <- Feedstock$Phosphorus/1000 * GlobalFactors$P_Availability
    if(debug) print(paste("effectivePapplied ",effectivePapplied))
    MaxP <- effectivePapplied * GlobalFactors$P_Nratio
    for(i in 1:length(effectivePapplied)) {
        if (effectivePapplied[i] > MaxP[i])effectivePapplied[i] <- MaxP[i]
    }
    
    avoidedPfert   <-GlobalFactors$Displaced_P_Production_Factor * 
        effectivePapplied  
    
    
    
    avoidedInorganicFertdirectandIndirect <- 
        GlobalFactors$LA_DisplacedFertilizer_Direct_Indirect *
        effectiveNapplied/1000
    if(debug) print(paste("avoidedInorganicFertdirectandIndirect ",
                          avoidedInorganicFertdirectandIndirect))
    
    EMdisplacedFertilizer <- avoidedNfert + avoidedInorganicFertdirectandIndirect +
        avoidedPfert + avoidedKfert
    if(debug) print(paste("displacedFertilizer ",EMdisplacedFertilizer))
    
    # Add together
    EMNetLandapp <- switch(Application,
                           'noDisplace' = EMLandApp + EMCstorage,
                           'Fertilizer' = EMLandApp + EMCstorage + EMdisplacedFertilizer)
    
    #If canned goods includes a recovery step to reclaim EMrecycle <- switch(Feedstock$type,
    #     'canned goods', =
    #       , )
    
    # Add together
    netEmissions <- 
        EMgrind +
        EMDigester +
        EMStorage +
        EMNetLandapp 
    # + EMrecycle
    
    
    result <- data.frame(netEmissions,EMDigester,EMStorage)
    colnames(result) <- c("ADnetEmissions","EMDigester", "EMStorage")
    result <- cbind(result,EMLandApp,EMCstorage,EMdisplacedFertilizer)
    result
}
