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
  Landfill_Oxidation_Factor = 0.20
  DieseluseLpert =5.8
   #Based upon Warm v.13 model 0.7gal/t for landfill equipment operation
  DieselprovisionkgCO2eperL=0.45
  # Taken from Fruergaard et al. (2009)
  DieselcombustionkgCO2eperL=2.63
  #Calculated from GREET 2014 CO2, CH4 and N2O emissions w IPCC AR5 GWF
 
  Landfill_GC = 0.95
  k= 0.144
  
# step 1: Landfill operation
  EMLFoperation<-DieseluseLpert*(DieselprovisionkgCO2eperL+DieselcombustionkgCO2eperL)

  
# step 2: Methane Production

    Max_Years=50
    bytens <- TRUE
    if(bytens == TRUE){
        tcol <- seq(from=0.1,to=Max_Years,by=0.1)
        Q10x<-Feedstock$Lo*k*0.1*exp(-tcol*k)
        # sum up the 10 intra-year steps before multiplying LCE
        Q <- running(Q10x, fun=sum, width=10, align="left", by=10)
    } else {
        tcol<-1:Max_Years
        Q<-Feedstock$Lo*k*exp(-tcol*k)
    }
    #Q is methane produced
    if(debug) {print(paste("Q len ",length(Q)));
                     print(Q)}
    Landfill_LCE<- c(0,0.5,0.5,0.5,0.5,0.75,0.75,0.75,0.75,0.75,0.75,0.75,0.75,0.75,0.75,
                     0.825,0.825,rep(0.95,Max_Years-17))
    if(debug) {print(paste("Landfill_LCE len ",length(Landfill_LCE)));
        print(Landfill_LCE)}
    Percent_Captured<-(Landfill_GC*Landfill_LCE)
    Percent_Released<-(1-Percent_Captured)
    Q_Released<-Q*(1-Landfill_Oxidation_Factor)* Percent_Released
    Q_Captured<-Q*(1-Landfill_Oxidation_Factor)* Percent_Captured
    Cum_Released<-sum(Q_Released)
    Cum_Captured<-sum(Q_Captured)
    Landfill_EM_CH4<-Cum_Released*GlobalFactors$density_CH4*GlobalFactors$GWPCH4
 
}
