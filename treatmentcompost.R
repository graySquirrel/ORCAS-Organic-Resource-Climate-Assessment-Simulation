# Landfill function.
# Functions
#   LandfillTreatmentPathway(Feedstock, GlobalFactors, debug = F)
#       returns data.frame of factor vs. outputs, labeled with row and col names
#
################# Treatment Functions
library(gtools)
LandfillTreatmentPathway <- function(Feedstock, GlobalFactors, debug = F)
{
# Hauling of the waste to the landfill not included at this time
#EFfreight_kgCO2ePERtonKm = 0.107,
  Landfill_Oxidation_Factor = 0.10
  # could be an array 
  #Landfill_Oxidation_Factor<-c(.10,rep(0.2,16),rep(0.25,Max_Years-17))
  DieseluseLpert =5.8
   #Based upon Warm v.13 model 0.7gal/t for landfill equipment operation
  DieselprovisionkgCO2eperL=0.45
  # Taken from Fruergaard et al. (2009)
  DieselcombustionkgCO2eperL=2.720494342
  #Calculated from GREET 2014 CO2, CH4 and N2O emissions w IPCC AR5 GWF
 
  Landfill_GC=0.85
  #NYS penetration of landfills with LFG recovery-not used at this time. Assumed that landfill has LFG recovery system
  
  Landfill_CF = 0.97
  #3% flare and not used to generate electricity
  k= 0.144
  
# step 1: Landfill operation
  EMLFoperation<-DieseluseLpert*(DieselprovisionkgCO2eperL+DieselcombustionkgCO2eperL)
  if(debug) {print(paste("EMLFoperation", (EMLFoperation)))}
  
# step 2: Methane Production
    Max_Years=50
    bytens <- FALSE
    if(bytens == TRUE){
        tcol <- seq(from=0.1,to=Max_Years,by=0.1)
        Q10x<-k*0.1*exp(-tcol*k)
        # sum up the 10 intra-year steps before multiplying LCE
        Q <- running(Q10x, fun=sum, width=10, align="left", by=10)
    } else {
        tcol<-1:Max_Years
        Q<-k*exp(-tcol*k)
    }
    #Q is methane produced
    if(debug) {print(paste("Q len ",length(Q)));
                     print(Q)}
    years0_1 <- c(0)
    years2_4 <- c(0.5,0.5,0.5)
    years5_14 <- c(rep(0.75,10))
    years15_17 <- c(0.825,0.825,0.825)
    yearsFinal <- rep(0.9,Max_Years-17)
    
    Landfill_LCE<- c(years0_1,years2_4,years5_14,years15_17,yearsFinal)
    if(debug) {print(paste("Landfill_LCE len ",length(Landfill_LCE)));
        print(Landfill_LCE)}
    #Percent_Captured<-(Landfill_GC*Landfill_LCE)
    #Percent_Released<-(1-Percent_Captured)
    Q_Released<-Q*(1-Landfill_Oxidation_Factor)*(1- Landfill_LCE)
    Q_Captured<-Q*(1-Landfill_Oxidation_Factor)* Landfill_LCE
    if(debug) {print(paste("Q_Released len ",length(Q_Released)));
      print(Q_Released)}
    Fraction_Emitted<-sum(Q_Released)
    if(debug) {print(paste("Fraction_Emitted ",Fraction_Emitted))}
    Fraction_recovered<-sum(Q_Captured)
    if(debug) {print(paste("Fraction_recovered len ",Fraction_recovered))}
    Landfill_EM_CH4<-Fraction_Emitted*GlobalFactors$density_CH4*GlobalFactors$GWPCH4*Feedstock$Lo
}
    Landfill_recovered_CH4<-Fraction_recovered*Feedstock$Lo*Landfill_CF
    #Landfill gas that is recovered for electricity, net of gas flared
    Landfill_Kwh_t<-Landfill_recovered_CH4*GlobalFactors$Energy_content_methane_BTUperm3CH4/GlobalFactors$Heating_value
    EM_displaced_grid<-Landfill_Kwh_t*GlobalFactors$EFGrid/1000
    if(debug) {print(paste("EM_displaced_grid ",EM_displaced_grid))}
   
    
    #Step 3 Carbon storage
    #CStorage<-InitialC*(1-fdeg)
    #EMCstorage<-Cstorage*44/12
    
    

