# Landfill function.
# Functions
#   LandfillTreatmentPathway(Feedstock, GlobalFactors, debug = T)
#       returns data.frame of factor vs. outputs, labeled with row and col names
#
#library(gtools)
################# Treatment Functions
# The function calcOverMax years is called in Step 2
# It is an inner loop over many years.  have to do this because input factors
# are vectors, and this calcs over orthogonal vector (years). don't know a better way.
calcOverMaxYears <- function(onek,oneLCEMax,oneOXMax,debug=FALSE) {
    Max_Years=100
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
   
    #LCE varies by year as the LFG recovery system is implemented
    if(debug) {print(paste("Q len ",length(Q)));print(Q)}
    years1_2 <- c(0,0) 
    years3_5 <- c(0.5,0.5,0.5)
    years6_15 <- c(rep(0.75,10))
    yearsFinal <- rep(oneLCEMax,Max_Years-15) 
    Landfill_LCE<- c(years1_2,years3_5,years6_15,yearsFinal)
    # Oxidation factor also varies by year as cover is applied
    years1_2 <- c(.1,.1)
    years3_10 <- c(rep(.2,7))
    yearsFinal <- rep(oneOXMax, Max_Years-9)
    Landfill_OX<-c(years1_2,years3_10,yearsFinal)
    if(debug) {print(paste("Landfill_LCE len ",length(Landfill_LCE)));
      print(Landfill_LCE)}    
    if(debug) {print(paste("Landfill_OX len ",length(Landfill_OX)));
        print(Landfill_OX)}
    Q_Released<-Q*(1-Landfill_OX)*(1- Landfill_LCE)
    Q_Captured<-Q*(1-Landfill_OX)* Landfill_LCE
    if(debug) {print(paste("Q_Released len ",length(Q_Released)));
        print(Q_Released)}
    #Fraction emitted is the cummulative portion of LF_Lo not captured
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
  EMLFoperation <- GlobalFactors$LFDieseluseLpert *
      (GlobalFactors$DieselprovisionkgCO2eperL + GlobalFactors$DieselcombustionkgCO2eperL)
  if (debug) print(paste("sum diesel",
                         (GlobalFactors$DieselprovisionkgCO2eperL+
                              GlobalFactors$DieselcombustionkgCO2eperL)))
# step 2: Methane Production
    # inputs will either be vectors of N or 1.
    maxvec <- max(length(GlobalFactors$k),length(GlobalFactors$LCEMax),
                  length(GlobalFactors$Landfill_OX_Max))
    if(length(GlobalFactors$k)==1){GlobalFactors$k<-rep(GlobalFactors$k,maxvec)}
    if(length(GlobalFactors$LCEMax)==1){GlobalFactors$LCEMax<-rep(GlobalFactors$LCEMax,maxvec)}
    if(length(GlobalFactors$Landfill_OX_Max)==1){GlobalFactors$Landfill_OX_Max<-
        rep(GlobalFactors$Landfill_OX_Max,maxvec)}
    outfe<-NULL
    outfr<-NULL
    for (i in 1:maxvec) {
        if(debug){print(paste(GlobalFactors$k[i],GlobalFactors$LCEMax[i],
                              GlobalFactors$Landfill_OX_Max[i]))}
        onef<-calcOverMaxYears(onek=GlobalFactors$k[i],oneLCEMax=GlobalFactors$LCEMax[i],
                               oneOXMax =GlobalFactors$Landfill_OX_Max[i],
                               debug=debug)
        outfe<-c(outfe,onef$Fraction_Emitted)
        outfr<-c(outfr,onef$Fraction_recovered)
    }
    #Allows for a correction factor CF to adjust LF_Lo based upon BMP and Cho et al., 
    LF_Lo<-Feedstock$Lo*GlobalFactors$BMP_Correctionfactor
    
    EMLandfillCH4 <- outfe * GlobalFactors$density_CH4 *
        GlobalFactors$GWPCH4 * LF_Lo
    Landfill_recovered_CH4 <- outfr * LF_Lo * GlobalFactors$Landfill_CF
    if(debug) {print(paste("Landfill_recovered_CH4 ",Landfill_recovered_CH4))}
    
    #Landfill gas that is recovered for electricity, net of gas flared
    Landfill_Kwh_t<-Landfill_recovered_CH4*
        GlobalFactors$Energy_content_methane_BTUperm3CH4/GlobalFactors$Heating_value
    EM_displaced_grid<-Landfill_Kwh_t*GlobalFactors$EFGrid/1000
    
    #Step 3 Carbon storage
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
    
    EMLandfill <- EM_displaced_grid + EMLFoperation + EMLandfillCH4 + EMCstorage
    result <- data.frame(EMLandfill,EM_displaced_grid,EMLFoperation,
                         EMLandfillCH4,EMCstorage)
    colnames(result) <- c("LandfillNetEmissions","EM_displaced_grid", 
                          "EMLFoperation", "EMLandfillCH4", "EMCstorage")
    result
}

