# R script defining classes and functions for waste treatment
#
# Classes
#   GlobalFactors: a 'static'ish collection of global variables
#      members: See below.  too many to list here.
#      methods: GlobalFactors()
#                   returns GlobalFactors object
#   Feedstock:
#      members: type,TS,TVS,Bo,TKN)
#      methods: Feedstock(type="string",TS=0,TVS=0,Bo=0,TKN=0)
#                   returns Feedstock object
#
# Functions
#   AnaerobicDigestionTreatmentPathway(Feedstock, GlobalFactors, XportToField = 20)
#       returns data.frame of factor vs. outputs, labeled with row and col names
#
#
########### GlobalFactors stuff
GlobalFactors <- function()
{
    me <- list(N20N_to_N20 = 44/28, 
               GWPN20 = 310, 
               density_CH4 = 0.67, 
               GWPCH4 = 21, 
               Grid_emissions_factor = 533.66,
               Energy_content_methane_BTUperm3CH4 = 35315, 
               Heating_value = 11700, 
               methane_KWhPerM3 = 35315/11700,
               Hauling_kgCO2ePERtonKm = 0.107,
               AD_Digester_utilizationFactor = 0.84,
               AD_Digester_CH4Leaks = 0.03,
               AD_Digester_incompleteCombustion = 0.005,
               AD_Digester_conversionEfficiency = 4.19318820416827,
               AD_Digester_electricityUtilized = 0.12,
               AD_Digester_avoidedElectricityEmissions = -533.66,
               AD_Storage_reductionInVS = 0.55,
               AD_Storage_residualMethane = 0.043,
               AD_Storage_IPCC_EF3 = 0.005,
               AD_Storage_IPCC_EF4timesFracGasm = 0.0026,
               AD_LandApplication_mysteryFactor1 = 0.0125,
               AD_LandApplication_mysteryFactor2 = 0.002,
               AD_DisplacedFertilizer_mineralizationFactor = 0.4,
               AD_DisplacedFertilizer_WoodCowieEmissionFactor = -6.8,
               AD_DisplacedFertilizer_mysteryFactor1 = -5.4)
    class(me) <- append(class(me), "GlobalFactors")
    return(me)
}

########### FeedStock stuff
Feedstock <- function(type="dontKnow",TS=0,TVS=0,Bo=0,TKN=0)
{
    if (!is.numeric(TS) || !is.numeric(TVS) || 
            !is.numeric(Bo) || !is.numeric(TKN) ||
            !all(is.finite(TS)) || !all(is.finite(TVS)) ||
            !all(is.finite(Bo)) || !all(is.finite(TKN)))
        stop("invalid input")
    if (length(TS) != length(TVS) || (length(TS) != length(Bo)) ||
            (length(TS) != length(TKN)) || (length(TS) != length(type)))
        stop("lengths differ")
    me <- list(
        type = type,
        TS=TS,TVS=TVS,Bo=Bo,Lo=Bo*TVS/100,TKN=TKN
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

################# Treatment Functions
debug<-F
AnaerobicDigestionTreatmentPathway <- function(Feedstock, GlobalFactors, XportToField = 20)
{
    # Step 1: calculate Digester emissions kgCO2e/MT
    meth       <- Feedstock$Lo * GlobalFactors$AD_Digester_utilizationFactor
    if(debug) print(paste("meth ",meth))
    leakTmp    <- meth * GlobalFactors$AD_Digester_CH4Leaks
    CH4Leaks   <- leakTmp * GlobalFactors$density_CH4 * GlobalFactors$GWPCH4
    incompTmp  <- meth * GlobalFactors$AD_Digester_incompleteCombustion
    incomplete <- incompTmp * GlobalFactors$density_CH4 * GlobalFactors$GWPCH4
    elecTmpTmp <- GlobalFactors$AD_Digester_conversionEfficiency * meth/1000
    if(debug) print(paste("elecTmpTmp ",elecTmpTmp))
    elecTmp    <- elecTmpTmp * (1 - 0.12)
    if(debug) print(paste("elecTmp ",elecTmp))
    avoidedElecEmissions <- elecTmp * GlobalFactors$AD_Digester_avoidedElectricityEmissions
    if(debug) print(paste("CH4Leaks ",CH4Leaks," incomplete ",incomplete,
                          " avoided ",avoidedElecEmissions))
    Digester   <- CH4Leaks + incomplete + avoidedElecEmissions
    if(debug) print(paste("Digester ",Digester))
    
    # Step2: calculate Storage emissions kgCO2e/MT
    TVSinDigestate    <- Feedstock$TVS * GlobalFactors$AD_Storage_reductionInVS
    if(debug) print(paste("TVSinDigestate ",TVSinDigestate))    
    residualMethane   <- TVSinDigestate * GlobalFactors$AD_Storage_residualMethane
    if(debug) print(paste("residualMethane ",residualMethane))    
    effluentEmissions <- residualMethane * GlobalFactors$density_CH4 * GlobalFactors$GWPCH4
    if(debug) print(paste("effluentEmissions ",effluentEmissions))
    directN20         <- Feedstock$TKN * GlobalFactors$N20N_to_N20 * 
        GlobalFactors$GWPN20 * GlobalFactors$AD_Storage_IPCC_EF3 / 1000
    if(debug) print(paste("directN20 ",directN20))
    indirectN20       <- GlobalFactors$AD_Storage_IPCC_EF4timesFracGasm *
        Feedstock$TKN * GlobalFactors$N20N_to_N20 * GlobalFactors$GWPN20 / 1000
    if(debug) print(paste("indirectN20 ",indirectN20))
    N20Emissions      <- directN20 + indirectN20
    if(debug) print(paste("N20Emissions ",N20Emissions))
    Storage           <- effluentEmissions + N20Emissions
    if(debug) print(paste("Storage ",Storage))
    
    # Step 3: Calculate Land Application  kgCO2e/MT
    xport           <- 1.5 * XportToField/20
    if(debug) print(paste("xport ",xport))
    Nremaining      <- Feedstock$TKN - GlobalFactors$AD_Storage_IPCC_EF3 * Feedstock$TKN -
        Feedstock$TKN*0.26
    direct2         <- Nremaining * GlobalFactors$AD_LandApplication_mysteryFactor1 *
        GlobalFactors$N20N_to_N20 * GlobalFactors$GWPN20 / 1000
    if(debug) print(paste("direct2 ",direct2))
    indirect2       <- Nremaining * GlobalFactors$AD_LandApplication_mysteryFactor2 *
        GlobalFactors$N20N_to_N20 * GlobalFactors$GWPN20 / 1000
    if(debug) print(paste("indirect2 ",indirect2))
    N20Emissions    <- direct2 + indirect2
    if(debug) print(paste("N20Emissions ",N20Emissions))
    LandApplication <- xport + N20Emissions
    if(debug) print(paste("LandApplication ",LandApplication))
    
    # Step 4: Displaced fertilizer kgCO2e/MT
    Nremaining      <- Nremaining - 
        Nremaining * GlobalFactors$AD_LandApplication_mysteryFactor1 -
        Nremaining * 0.2
    effectiveNapplied <- Nremaining * 
        GlobalFactors$AD_DisplacedFertilizer_mineralizationFactor
    avoidedNfert    <- GlobalFactors$AD_DisplacedFertilizer_WoodCowieEmissionFactor *
        effectiveNapplied/1000
    avoidedNfertDirectAndIndirect <- GlobalFactors$AD_DisplacedFertilizer_mysteryFactor1 *
        effectiveNapplied/1000
    displacedFertilizer <- avoidedNfert + avoidedNfertDirectAndIndirect
    if(debug) print(paste("displacedFertilizer ",displacedFertilizer))
    
    # Add together
    netEmissions <- 
        Digester +
        Storage +
        LandApplication +
        displacedFertilizer
    names(netEmissions) <- Feedstock$type
    return(netEmissions)
}




