# compost function.
# Functions
#   compostTreatmentPathway(Feedstock, GlobalFactors, debug = F)
#       returns data.frame of factor vs. outputs, labeled with row and col names
#
################# Treatment Functions
compostTreatmentPathway <- function(Feedstock, GlobalFactors, debug = F)
{
  Compost_dieseLlpert<-3
  #Boldrin,2009
  CompostPercentCdegraded<-0.58
  #Boldrin,2009
  Compost_degradedC_CH4<-0.02
  #Boldrin,2009
  Compost_N2OperN<-0.015
# Hauling of the waste to the compost facility not included at this time
# step 1: Compost operation
  EMCompostoperation<-Compost_dieseLlpert*(GlobalFactors$DieselprovisionkgCO2eperL+GlobalFactors$DieselcombustionkgCO2eperL)
  if(debug) {print(paste("EMCompostoperation", (EMCompostoperation)))}
  
# step 2: Biological emissions
   EMCompost_CH4<-CompostPercentCdegraded*Compost_degradedC_CH4*Feedstock$InitialC*
     GlobalFactors$CtoCH4*GlobalFactors$GWPCH4
   EMCompost_N2O=Feedstock$Nperton*Compost_N2OperN*GlobalFactors$N20N_to_N20*
     GlobalFactors$GWPN20
   if(debug) {print(paste("EMCompost_CH4", (EMCompost_CH4),"EMCompost_N2O",(EMCompost_N2O)))}
   
  
#Step 3 Carbon storage
    #CStorage<-InitialC*(1-fdeg)
    #EMCstorage<-Cstorage*44/12
    
}
    

