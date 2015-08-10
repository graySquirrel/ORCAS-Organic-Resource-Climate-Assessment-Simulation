# Landfill function.
# Functions
#   LandfillTreatmentPathway(Feedstock, GlobalFactors, debug = F)
#       returns data.frame of factor vs. outputs, labeled with row and col names
#
################# Treatment Functions
LandfillTreatmentPathway <- function(Feedstock, GlobalFactors, debug = F)
{
 
  #EFfreight_kgCO2ePERtonKm = 0.107,
  Landfill_Oxidation_Factor = 0.10
  Landfill_GC = 0.95
  k= 0.144
  

  
# step 1: Methane Production
    # TODO
    # EMTransport <- 
    # Step 1: calculate Digester emissions kgCO2e/MT
    Max_Years=50
    tcol<-1:Max_Years
    #Q is methane produced
    Q<-exp(-tcol*k)
    Landfill_LCE<- c(0,0.45,0.6, 0.65,0.70,0.75,0.75,0.75,0.75,0.75,0.75,
                     0.79,0.83,0.87,0.91,0.95,rep(0.95,Max_Years-16))
    Percent_Captured<-(Landfill_GC*Landfill_LCE)
    Percent_Released<-(1-Percent_Captured)
    Q_Released<-Q*k*(1-Landfill_Oxidation_Factor)* Percent_Released
    Q_Captured<-Q*k*(1-Landfill_Oxidation_Factor)* Percent_Captured
    Cum_Released<-sum(Q_Released)
    Cum_Captured<-sum(Q_Captured)
    Landfill_EM_CH4<-Cum_Released*Feedstock$Lo*GlobalFactors$density_CH4*GlobalFactors$GWPCH4
  
    
    
    

 
}
