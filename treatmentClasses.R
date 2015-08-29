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
########### GlobalFactors stuff
GlobalFactors <- function()
{
    me <- list(N20N_to_N20 = 44/28, 
               GWPN20 = 265, 
               density_CH4 = 0.67, 
               GWPCH4 = 28, 
               EFGrid = -533.66,
               #EFfreight_kgCO2ePERtonKm = 0.107,
               Energy_content_methane_BTUperm3CH4 = 35315, 
               Heating_value = 11700, 
               #AD_Digester_utilizationFactor = 0.84,
               #AD_Digester_CH4Leaks = 0.03,
               #AD_Digester_CH4incompleteCombustion = 0.005,
               #AD_Digester_N20incompleteCombustion = 0.03,
               #AD_Digester_conversion_KwHPerM3 = 4.19318820416827,
               #AD_Digester_parasiticLoad = 0.12,
               #AD_Storage_reductionInVS = 0.55,
               #AD_Storage_EFresidualMethaneM3CH4PerKgVS = 0.043, # 0.054
               #AD_Storage_IPCC_EF3 = 0.005,
               IPCC_EF4 = 0.01)
               #AD_Storage_IPCC_FracGasMS = 0.26,
               #AD_LandApplication_FracGasM =0.2,
               #AD_LandApplication_EF1 = 0.0125,
               #AD_LandApplication_OtherNFactor = 0.02,
               #AD_LandApp_NAvailabiltiy_Factor = 0.4,
               #AD_DisplacedFertilizer_Production_Factor = -6.8,
               #AD_DisplacedFertilizer_Direct_Indirect = -5.4)
    class(me) <- append(class(me), "GlobalFactors")
    return(me)
}

########### FeedStock stuff
Feedstock <- function(type="dontKnow",TS=0,VS=0,Bo=0,TKN=0,
                      percentCarboTS=0,percentProteinTS,percentLipidTS=0,fdeg=0)
{
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
    #print(paste("TS",TS,"Lo ",Lo," TVS ",TVS,"initialC ",InitialC))
    me <- list(
        type = type,
        TS=TS,VS=VS,Lo=Lo,Bo=Bo,TKN=TKN,TVS=TVS,
        percentCarboTS=percentCarboTS,percentProteinTS=percentProteinTS,
        percentLipidTS=percentLipidTS,fdeg=fdeg,InitialC=InitialC
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







