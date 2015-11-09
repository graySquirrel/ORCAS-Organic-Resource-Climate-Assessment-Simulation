# R script defining classes and functions for waste treatment
#    sourced by simuTreat.R or other files that need these classes
#
# Classes
#   GlobalFactors: a 'static'ish collection of global variables
#      members: See below.  too many to list here.
#      methods: GlobalFactors()
#                   returns GlobalFactors object
#   Feedstock:
#      members: type,TS,VS,Bo,TKN)
#      methods: Feedstock(type="string",TS=0,VS=0,Bo=0,TKN=0)
#                   returns Feedstock object
#
########### other globals
numSamps <- 1001
set.seed(1234)
########### GlobalFactors stuff
GlobalFactors <- function()
{
    me <- list(
        # Conversion constants
        N20N_to_N20 = 44/28, 
        density_CH4 = 0.67,
        density_CO2 = 1.799,
        CtoCH4=16/12,
        CtoCO2=44/12,
        Energy_content_methane_BTUperm3CH4 = 35315, 
        # AR5 GWP factors,
        GWPN20 = 265, 
        GWPCH4 = 28, 
        # Emission factors
        EFGrid = 692.15,
        #EFfreight_kgCO2ePERtonKm = 0.107,
        DieselprovisionkgCO2eperL=0.45,
        # Taken from Fruergaard et al. (2009)
        DieselcombustionkgCO2eperL=2.720494342,
        #Calculated from GREET 2014 CO2, CH4 and N2O emissions w IPCC AR5 GWF
        # Land Application 
        IPCC_EF4 = 0.01,
        DieselspreadLpertkm = 0.30,
        Dieselgrindpert = 2.65,
        
        #Compost
        Compost_dieseLlpert = 3,
        Compost_electpert = 15,
        CompostPercentCdegraded = 0.58,
        #Boldrin,2009
        Compost_degradedC_CH4 = 0.02,
        #Boldrin,2009
        Compost_N2OperN = 0.005,
        Compost_CS_factor = 0.2,
        # Additional decay beyond AD degradation tests due to organisms and fungi
        Compost_mass_reduction = 0.4,
        Compost_xportToField = 30,
        Compost_spread_diesellpert = 0.4,
        Compost_N_loss = 0.38,
        #Compost_NAvailabiltiy_Factor = 0.4,
        Compost_FracGasC =0.016,
        Peatdisplacementfactor = 1,
        EF_Peat_kgCO2eperton = -970,
        Compost_EF1 = 0.015, # Boldrin
        
        #Anaerobic Digestion
        AD_Cf = 0.90,
        AD_flared=0.03,
        AD_Digester_CH4Leaks = 0.025,
        AD_Digester_CH4IC = 0.005,
        #AD_Digester_N20incompleteCombustion = 0.03,
        AD_Digester_CE = 4.19318820416827,
        AD_Digester_parasiticLoad = 0.12,
        AD_reductionInVS = 0.55,
        AD_Storage_residualCH4 = 0.054,
        AD_Storage_EF3 = 0.005,
        AD_Storage_FracGasMS = 0.26,
        AD_xportTofield = 20,
        AD_LA_FracGasD =0.2,
        #AD_LandApplication_EF1 = 0.0125, in land app factors
        AD_LandApplication_OtherNFactor = 0.02,
        AD_LA_FracLeachD = 0.007,
        AD_N_Availability = 0.65,
        AD_CSfactor = 0.4,
       
        CH4content=0.60,
        
        #Landfill
        LFDieseluseLpert =5.83,
        Landfill_OX_Max = 0.35,
        Heating_value = 11700, 
        Landfill_CF = 0.85,
        k= 0.144,
        LCEMax = 0.9,
        BMP_Correctionfactor = 1,

        
        
        #Land Application
        LandApplication_FracGasM =0.2,
        LandApplication_EF1 = 0.0125,
        #LandApplication_OtherNFactor = 0.02,
        LA_Displaced_N_Production_Factor = -6.8,
        LA_Displaced_N_Direct_Indirect = -5.4,
        LA_Displaced_P_Production_Factor =-1.8, 
        LA_Displaced_K_Production_Factor = -0.96,
        LA_CSfactor = 0.7,
        LA_xportToField = 20,
        N_displacement = 1,
        K_displacement =1, 
        P_displacement =1,
        K_Availability = 1,
        P_Availability =1,
        LA_DisplacedFertilizer_Direct_Indirect = -5.4,
        
        # Animal Feed
        EFCornDisplacement = -592,
        CornTS = 0.88,
        CornTDN = 0.88,
        AF_loss = 0.1
    )
    me$AD_Digester_OneMinusparasiticLoad <- (1-me$AD_Digester_parasiticLoad)
    me$AD_OneMinusreductionInVS <- (1-me$AD_reductionInVS)
    class(me) <- append(class(me), "GlobalFactors")
    return(me)
}

########### FeedStock stuff
Feedstock <- function(type="dontKnow",TS=0,VS=0,Bo=0,TKN=0,
                      percentCarboTS=0,percentProteinTS=0,percentLipidTS=0,
                      fdeg=0,Phosphorus=0, Potassium=0, 
                      Carbodigestability = 'med')
    
{
    if(missing(percentCarboTS)) percentCarboTS=0
    if(missing(percentProteinTS)) percentProteinTS=0
    if(missing(percentLipidTS)) percentLipidTS=0
    if(missing(fdeg)) fdeg=0
    
    if (!is.numeric(TS) || !is.numeric(VS) || 
        !is.numeric(Bo) || !is.numeric(TKN) ||
        !all(is.finite(TS)) || !all(is.finite(VS)) ||
        !all(is.finite(Bo)) || !all(is.finite(TKN)))
        stop("invalid input")
    #if (length(Bo) != length(TKN))
    #    stop("lengths differ")
    # Carbon content
    carboPercentC<-0.444
    proteinPercentC<-0.531
    lipidPercentC<-0.771
    InitialC<-(percentCarboTS*carboPercentC + 
                   percentProteinTS*proteinPercentC +
                   percentLipidTS*lipidPercentC)*TS*1000
    proteinDigest <- 0.85
    lipidDigest <- 0.8
    # Carbodigestability enumeration:  'med' = 35% CF and 65% NFE
    #                           'high' = 9% CF and 91% NFE
    #                           'low' = 60% CF and 40% NFE
    
    CarboDigest <- switch(Carbodigestability,
                          'high' = 0.78,
                          'med' = 0.73,
                          'low' = 0.68)
    
    #if Carbodigestability = "high" CarboDigest =0.78
    # if Carbodigestability = "low" CarboDigest =0.68
    # if Carbodigestabiity = "me" CarboDigest =0.73
    
    TDN <- proteinDigest * percentProteinTS + 
        lipidDigest * 2.25 * percentLipidTS + 
        CarboDigest * percentCarboTS
    
    
    TVS = VS*TS
    Lo=Bo*TVS
    Nperton<-TKN/1000
    #print(paste("TS",TS,"Lo ",Lo," TVS ",TVS,"initialC ",InitialC))
    me <- list(
        type = type,
        TS=TS,VS=VS,Lo=Lo,Bo=Bo,TKN=TKN,TVS=TVS,
        percentCarboTS=percentCarboTS,percentProteinTS=percentProteinTS,
        percentLipidTS=percentLipidTS,fdeg=fdeg,InitialC=InitialC,Nperton=Nperton,
        TDN=TDN, Phosphorus=Phosphorus, Potassium=Potassium, rdeg=1-fdeg
    )
    
    
    ## Set the name for the class
    class(me) <- append(class(me),"Feedstock")
    return(me)
}
# length uses the already generic length function
length.Feedstock <- function(obj) length(obj$TS)
# create a generic to use in the Feedstock class
typeof <- function(obj) UseMethod("typeof")
typeof.Feedstock <- function(obj) obj$type







