# R script defining classes and functions for waste treatment
#
# TODO: Make these all vector or distribution driven
#
# Classes
#   Feedstock:
#      members: mass,volume,type,parameters(Ts,Vs,TKN,Bo,CN)
#      methods: Feedstock(mass=x,vol=y,type="string",Ts=0,Vs=0,TKN=0,Bo=0,CN=0)
#                  returns Feedstock object
#   Treatment:
#      members: type,feedstk,parameters(???)
#      methods: Treatment(type="AD",feedstk=aFeedstockObject,p1=0,p2=0...)
#                  returns Treatment object
#               calculate.Treatment()
#
# Functions
#
Feedstock <- function(mass=0,vol=0,type="dontKnow",Ts=0,Vs=0,TKN=0,Bo=0,CN=0)
{
    if (!is.numeric(mass) || !is.numeric(vol) ||
            !all(is.finite(mass)) ||
            !all(is.finite(vol)))
        stop("invalid input")
    if (length(Ts) != length(Vs) || (length(Ts) != length(TKN)) ||
            (length(Ts) != length(Bo)) || (length(Ts) != length(CN)))
        stop("lengths differ")
    me <- list(
        mass = mass,
        vol = vol,
        type = type,
        Ts=Ts,Vs=Vs,TKN=TKN,Bo=Bo,CN=CN
    )
    
    ## Set the name for the class
    class(me) <- append(class(me),"Feedstock")
    return(me)
}
