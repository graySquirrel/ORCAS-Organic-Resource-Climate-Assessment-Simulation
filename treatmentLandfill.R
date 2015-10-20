# Landfill function.
# Functions
#   LandfillTreatmentPathway(Feedstock, GlobalFactors, debug = T)
#       returns data.frame of factor vs. outputs, labeled with row and col names
#
#library(gtools)
################# Treatment Functions
# Impacts are assessed over 100 years 
# The LandGem model is used to calculate methane generation due to the waste
# based upon a First Order Decay model summed in deciyears
# Methane generation  are calculated 
# In order to sum over deciyears have to  use an inner loop because input factors
# are vectors, and this calcs over orthogonal vector (years). don't know a better way.
calcOverMaxYears <- function(onek,oneLCEMax,oneOxidationFactor,debug=FALSE) {
    Max_Years=100
    bytens <- FALSE
#deciyears can be turned on or off

    
    if(bytens == TRUE){
        tcol <- seq(from=0.1,to=Max_Years,by=0.1)
        Q10x<-onek*0.1*exp(-tcol*onek)
        #Q is portion of LF_Lo generated in a given period
        # sum up the 10 intra-year steps before multiplying LCE
        Q <- running(Q10x, fun=sum, width=10, align="left", by=10)
    } else {
        tcol<-1:Max_Years
      
        Q<-onek*exp(-tcol*onek)
    }
   
   # In a given year a portion of the methane generated is captured based upon 
   # the phased implementation of the landfill gas capture system as defined by 
   # the landfill gas capture efficiency LCE per Levis and Barlaz, 2011
    if(debug) {print(paste("Q len ",length(Q)));print(Q)}
    years0_2 <- c(0,0)
    years3_5 <- c(0.5,0.5,0.5)
    years6_15 <- c(rep(0.75,10))
    #years16_17 <- c(0.825,0.825,0.825)
    yearsFinal <- rep(oneLCEMax,Max_Years-15)
    Landfill_LCE<- c(years0_1,years2_4,years5_14,years15_17,yearsFinal)
    if(debug) {print(paste("Landfill_LCE len ",length(Landfill_LCE)));
        print(Landfill_LCE)}
    #a portion of Q is oxidized or captured
    Q_Released<-Q*(1-oneOxidationFactor)*(1- Landfill_LCE)
    Q_Captured<-Q*(1-oneOxidationFactor)* Landfill_LCE
    if(debug) {print(paste("Q_Released len ",length(Q_Released)));
        print(Q_Released)}
    #Fraction emitted is the cummulative portion of LF_Lo emitted each year
    Fraction_Emitted<-sum(Q_Released)
    if(debug) {print(paste("Fraction_Emitted ",Fraction_Emitted))}
    Fraction_recovered<-sum(Q_Captured)
    if(debug) {print(paste("Fraction_recovered ",Fraction_recovered))}
    o<-NULL
    o$Fraction_Emitted <- Fraction_Emitted
    o$Fraction_recovered <- Fraction_recovered
    o
}
############################################################################
LandfillTreatmentPathway <- function(Feedstock, GlobalFactors, debug = F,
                                     sequesterCarbon = TRUE)
{
 
  
# step 1: Landfill operation
  EMLFoperation<-GlobalFactors$LFDieseluseLpert*
      (GlobalFactors$DieselprovisionkgCO2eperL+GlobalFactors$DieselcombustionkgCO2eperL)
  if (debug) print(paste("sum diesel",
                         (GlobalFactors$DieselprovisionkgCO2eperL+
                              GlobalFactors$DieselcombustionkgCO2eperL)))
# step 2: Methane Production
    # inputs will either be vectors of N or 1.
    maxvec <- max(length(GlobalFactors$k),length(GlobalFactors$LCEMax),
                  length(GlobalFactors$Landfill_Oxidation_Factor))
    if(length(GlobalFactors$k)==1){GlobalFactors$k<-rep(GlobalFactors$k,maxvec)}
    if(length(GlobalFactors$LCEMax)==1){GlobalFactors$LCEMax<-rep(GlobalFactors$LCEMax,maxvec)}
    if(length(GlobalFactors$Landfill_Oxidation_Factor)==1){GlobalFactors$Landfill_Oxidation_Factor<-
        rep(GlobalFactors$Landfill_Oxidation_Factor,maxvec)}
    outfe<-NULL
    outfr<-NULL
    for (i in 1:maxvec) {
        if(debug){print(paste(GlobalFactors$k[i],GlobalFactors$LCEMax[i],
                              GlobalFactors$Landfill_Oxidation_Factor[i]))}
        onef<-calcOverMaxYears(onek=GlobalFactors$k[i],oneLCEMax=GlobalFactors$LCEMax[i],
                               oneOxidationFactor=GlobalFactors$Landfill_Oxidation_Factor[i],
                               debug=debug)
        outfe<-c(outfe,onef$Fraction_Emitted)
        outfr<-c(outfr,onef$Fraction_recovered)
    }
    #Allows for a correction factor to adjust LF_Lo based upon BMP and Cho et al., 
    LF_Lo<-Feedstock$Lo*GlobalFactors$BMP_Correctionfactor
    #GW indicator is calculated as kgCO2e
    EMLandfillCH4 <- outfe * GlobalFactors$density_CH4 *
        GlobalFactors$GWPCH4 * LF_Lo
    # A portion of methane captured isrecovered for electricity production
    # based upon the capacity factor accounts for availability, operating load 
    # and parasitic losses of generating unit
    Landfill_recovered_CH4 <- outfr * LF_Lo * GlobalFactors$Landfill_CF
    if(debug) {print(paste("Landfill_recovered_CH4 ",Landfill_recovered_CH4))}
    
    # Electricity generated based upon heat rate of LFG to energy conversion
    # per EPA LMOP 
    Landfill_Kwh_t<-Landfill_recovered_CH4*
        GlobalFactors$Energy_content_methane_BTUperm3CH4/GlobalFactors$Heating_value
    # Displaced emissions kgCO2e using non baseload grid displacement factor
    EM_displaced_grid<-Landfill_Kwh_t*GlobalFactors$EFGrid/1000
    
    #Step 3 Carbon storage
    # Kg C resistant to anaerobic digestion assumed to remain after 100 yrs
    if (sequesterCarbon == TRUE) {
        CStorage<-Feedstock$InitialC*(1-Feedstock$fdeg)
        EMCstorage<-CStorage*(-44/12)
    } else {
        EMCstorage <- CStorage <- 0
    }
    if(debug) {print(paste("EMCstorage ",EMCstorage))}
    if(debug) {print(paste("EMLFoperation", (EMLFoperation)))}
    if(debug) {print(paste("EMLandfillCH4 ",EMLandfillCH4))}
    if(debug) {print(paste("EM_displaced_grid ",EM_displaced_grid))}
    if(debug) {print(paste("Landfill_Kwh_t",Landfill_Kwh_t))}
    
    # Net landfill emissions
    EMLandfill <- EM_displaced_grid + EMLFoperation + EMLandfillCH4 + EMCstorage
    result <- data.frame(EMLandfill,EM_displaced_grid,EMLFoperation,
                         EMLandfillCH4,EMCstorage)
    colnames(result) <- c("LandfillNetEmissions","EM_displaced_grid", 
                          "EMLFoperation", "EMLandfillCH4", "EMCstorage")
    result
}

