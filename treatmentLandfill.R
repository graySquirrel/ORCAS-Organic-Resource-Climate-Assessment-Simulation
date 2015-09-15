# Landfill function.
# Functions
#   LandfillTreatmentPathway(Feedstock, GlobalFactors, debug = F)
#       returns data.frame of factor vs. outputs, labeled with row and col names
#
#library(gtools)
################# Treatment Functions
# this is an inner loop over many years.  have to do this because input factors
# are vectors, and this calcs over orthogonal vector (years). don't know a better way.
calcOverMaxYears <- function(onek,oneLCEMax,oneOxidationFactor,debug=FALSE) {
    Max_Years=50
    bytens <- FALSE
    
    if(bytens == TRUE){
        tcol <- seq(from=0.1,to=Max_Years,by=0.1)
        Q10x<-onek*0.1*exp(-tcol*onek)
        # sum up the 10 intra-year steps before multiplying LCE
        Q <- running(Q10x, fun=sum, width=10, align="left", by=10)
    } else {
        tcol<-1:Max_Years
        Q<-onek*exp(-tcol*onek)
    }
    #Q is methane produced
    if(debug) {print(paste("Q len ",length(Q)));print(Q)}
    years0_1 <- c(0)
    years2_4 <- c(0.5,0.5,0.5)
    years5_14 <- c(rep(0.75,10))
    years15_17 <- c(0.825,0.825,0.825)
    yearsFinal <- rep(oneLCEMax,Max_Years-17)
    Landfill_LCE<- c(years0_1,years2_4,years5_14,years15_17,yearsFinal)
    if(debug) {print(paste("Landfill_LCE len ",length(Landfill_LCE)));
        print(Landfill_LCE)}
    Q_Released<-Q*(1-oneOxidationFactor)*(1- Landfill_LCE)
    Q_Captured<-Q*(1-oneOxidationFactor)* Landfill_LCE
    if(debug) {print(paste("Q_Released len ",length(Q_Released)));
        print(Q_Released)}
    Fraction_Emitted<-sum(Q_Released)
    if(debug) {print(paste("Fraction_Emitted ",Fraction_Emitted))}
    Fraction_recovered<-sum(Q_Captured)
    if(debug) {print(paste("Fraction_recovered ",Fraction_recovered))}
    o<-NULL
    o$Fraction_Emitted <- Fraction_Emitted
    o$Fraction_recovered <- Fraction_recovered
    o
}
############################################################################3
LandfillTreatmentPathway <- function(Feedstock, GlobalFactors, debug = F)
{
# Hauling of the waste to the landfill not included at this time
#EFfreight_kgCO2ePERtonKm = 0.107,
  Landfill_Oxidation_Factor = GlobalFactors$Landfill_Oxidation_Factor
  # could be an array 
  #Landfill_Oxidation_Factor<-c(.10,rep(0.2,16),rep(0.25,Max_Years-17))
  LFDieseluseLpert =GlobalFactors$LFDieseluseLpert
   #Based upon Warm v.13 model 0.7gal/t for landfill equipment operation
  #DieselprovisionkgCO2eperL=0.45
  # Taken from Fruergaard et al. (2009)
  #DieselcombustionkgCO2eperL=2.720494342
  #Calculated from GREET 2014 CO2, CH4 and N2O emissions w IPCC AR5 GWF
 
  #Landfill_GC=0.85
  #NYS penetration of landfills with LFG recovery-not used at this time. 
  # Assumed that landfill has LFG recovery system
  
  Landfill_CF = GlobalFactors$Landfill_CF
  #3% flare and not used to generate electricity
  k= GlobalFactors$k
  
# step 1: Landfill operation
  EMLFoperation<-LFDieseluseLpert*
      (GlobalFactors$DieselprovisionkgCO2eperL+GlobalFactors$DieselcombustionkgCO2eperL)
  if (debug) print(paste("sum diesel",
                         (GlobalFactors$DieselprovisionkgCO2eperL+
                              GlobalFactors$DieselcombustionkgCO2eperL)))
# step 2: Methane Production
    LCEMax = GlobalFactors$LCEMax
    # inputs will either be vectors of N or 1.
    maxvec <- max(length(k),length(LCEMax),length(Landfill_Oxidation_Factor))
    if(length(k)==1){k<-rep(k,maxvec)}
    if(length(LCEMax)==1){LCEMax<-rep(LCEMax,maxvec)}
    if(length(Landfill_Oxidation_Factor)==1){Landfill_Oxidation_Factor<-
        rep(Landfill_Oxidation_Factor,maxvec)}
    outfe<-NULL
    outfr<-NULL
    for (i in 1:maxvec) {
        if(debug){print(paste(k[i],LCEMax[i],Landfill_Oxidation_Factor[i]))}
        onef<-calcOverMaxYears(onek=k[i],oneLCEMax=LCEMax[i],
                               oneOxidationFactor=Landfill_Oxidation_Factor[i],debug=debug)
        outfe<-c(outfe,onef$Fraction_Emitted)
        outfr<-c(outfr,onef$Fraction_recovered)
    }
    EMLandfillCH4 <- outfe * GlobalFactors$density_CH4 *
        GlobalFactors$GWPCH4 * Feedstock$Lo
    Landfill_recovered_CH4 <- outfr * Feedstock$Lo * Landfill_CF
    if(debug) {print(paste("Landfill_recovered_CH4 ",Landfill_recovered_CH4))}
    
    #Landfill gas that is recovered for electricity, net of gas flared
    Landfill_Kwh_t<-Landfill_recovered_CH4*
        GlobalFactors$Energy_content_methane_BTUperm3CH4/GlobalFactors$Heating_value
    EM_displaced_grid<-Landfill_Kwh_t*GlobalFactors$EFGrid/1000
    
    #Step 3 Carbon storage
    CStorage<-Feedstock$InitialC*(1-Feedstock$fdeg)
    EMCstorage<-CStorage*(-44/12)
    if(debug) {print(paste("EMCstorage ",EMCstorage))}
    if(debug) {print(paste("EMLFoperation", (EMLFoperation)))}
    if(debug) {print(paste("EMLandfillCH4 ",EMLandfillCH4))}
    if(debug) {print(paste("EM_displaced_grid ",EM_displaced_grid))}
    if(debug) {print(paste("Landfill_Kwh_t",Landfill_Kwh_t))}
    
    EMLandfill <- EM_displaced_grid + EMLFoperation + EMLandfillCH4 + EMCstorage
    result <- data.frame(EMLandfill,EM_displaced_grid,EMLFoperation,
                         EMLandfillCH4,EMCstorage)
    colnames(result) <- c("LandfillNetEmissions","EM_displaced_grid", 
                          "EMLFoperation", "EMLandfillCH4", "EMCstorage")
    result
}

