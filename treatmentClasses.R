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
        CtoCH4=16/12,
        Energy_content_methane_BTUperm3CH4 = 35315, 
        # AR5 GWP factors,
        GWPN20 = 265, 
        GWPCH4 = 28, 
        # Emission factors
        EFGrid = -537.36,
        #EFfreight_kgCO2ePERtonKm = 0.107,
        DieselprovisionkgCO2eperL=0.45,
        # Taken from Fruergaard et al. (2009)
        DieselcombustionkgCO2eperL=2.720494342,
        #Calculated from GREET 2014 CO2, CH4 and N2O emissions w IPCC AR5 GWF
        # Land Application 
        IPCC_EF4 = 0.01,
        N_availabilityfactor = 0.4,
        DieselspreadLpertkm = 0.30,
        
        #Compost
        Compost_dieseLlpert = 3,
        #Boldrin,2009
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
        AD_MCFactor = 0.84,
        #Ebner et al., 2015, correction from Bo to Methane utilized (includes leaks, flares and MCF)
        AD_Digester_CH4Leaks = 0.03,
        # Moller
        AD_Digester_CH4incompleteCombustion = 0.005,
        #AD_Digester_N20incompleteCombustion = 0.03,
        # Ebner et al., 2015
        AD_Digester_conversion_KwHPerM3 = 4.19318820416827,
        AD_Digester_parasiticLoad = 0.12,
        AD_reductionInVS = 0.55,
        AD_Storage_EFresidualMethaneM3CH4PerKgVS = 0.054,
        #IPCC
        AD_Storage_IPCC_EF3 = 0.005,
        AD_Storage_IPCC_FracGasMS = 0.26,
        #AD_LandApplication_FracGasM =0.2,
        #AD_LandApplication_EF1 = 0.0125,
        AD_LandApplication_OtherNFactor = 0.02,
        #AD_LandApp_NAvailabiltiy_Factor = 0.4,
        #AD_DisplacedFertilizer_Production_Factor = -6.8,
        #AD_DisplacedFertilizer_Direct_Indirect = -5.4,
        AD_CSfactor = 0.4,
        AD_xportTofield = 20,
      
        
        #Landfill
        LFDieseluseLpert =5.83,
        #Based upon Warm v.13 model 0.7gal/t for landfill equipment operation
        Landfill_Oxidation_Factor = 0.10,
        #EPA, LMOP
        # could change yearly as described in Warm v.13 but asssumed an average value in this model
        #Landfill_Oxidation_Factor<-c(.10,rep(0.2,16),rep(0.25,Max_Years-17))
        Heating_value = 11700, 
        #EPA, LMOP
        Landfill_CF = 0.85,
        k= 0.144,
        #WARM v13       
        #Both feedstock and landfill parameter 
        LCEMax = 0.9,
        #Levis and Barlaz, 
        BMP_Correctionfactor = 1,
        #Cho, 2012 
        
        
        #Land Application
        LandApplication_FracGasM =0.2,
        LandApplication_EF1 = 0.0125,
        #LandApplication_OtherNFactor = 0.02,
        #LandApp_NAvailabiltiy_Factor = 0.4,
        LA_DisplacedFertilizer_Production_Factor = -6.8,
        LA_DisplacedFertilizer_Direct_Indirect = -5.4,
        LA_CSfactor = 0.7,
        LA_xportToField = 20,
        
        # Animal Feed
        EFCornDisplacement = -592,
        CornTS = 0.88,
        CornTDN = 0.88,
        AF_loss = 0.1
    )
    class(me) <- append(class(me), "GlobalFactors")
    return(me)
}

########### FeedStock stuff
Feedstock <- function(type="dontKnow",TS=0,VS=0,Bo=0,TKN=0,
                      percentCarboTS=0,percentProteinTS,percentLipidTS=0,
                      fdeg=0,TDN=0, Phosphorus=0, Potassium=0)
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







