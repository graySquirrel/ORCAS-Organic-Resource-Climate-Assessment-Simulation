# monte carlo fracGASM
library(mc2d)
#parameters

# AEF3st=array.array('f',[.0025,.005,.01])
# AFracGASMS=array.array('f',[.15,.26,0.45])
# AEF4=array.array('f',[.002,.01,.05])
# AEF3la=array.array('f',[.005,.0125,.05])
# AFracGASM=array.array('f',[.05,.2,.5])
# AFracLEACH=array.array('f',[0,.007,.8])
# AEF5=array.array('f',[.005,.0075,.025])
# AFracLoss=array.array('f',[.005])

FracLoss=0.005
#GWP=310
GWP=265
CF=44/28
#Nin = 290289
#Nin = 322461
#Nin = 337039
Nin = 312336

EF3st   <- mcstoc(rnorm, type="V", mean=0.005, sd=0.0025)
EF3la   <- mcstoc(rnorm, type="V", mean=.0125, sd=0.01875)
FracGASMS   <- mcstoc(rnorm, type="V", mean=.26, sd=0.095)
EF4   <- mcstoc(rnorm, type="V", mean=.01, sd=.02)
FracGASM   <- mcstoc(rnorm, type="V", mean=.2, sd=.15)
EF5   <- mcstoc(rnorm, type="V", mean=.0075, sd=0.00875)
#FracLEACH   <- mcstoc(rnorm, type="V", mean=.007, sd=0.39625)
FracLEACH   <- mcstoc(rnorm, type="V", mean=0.4, sd=0.2)

NremainingFunc <- function() {
    N2O_Ndst=EF3st*Nin
    Nremaining = Nin - N2O_Ndst
    #print ("Nrem1 " + str(Nremaining))
    Emdst=N2O_Ndst*CF*GWP
    NvolST=FracGASMS*Nremaining
    #print ("NvolST " + str(NvolST))
    N2O_NidST=EF4*NvolST
    #print ("N2O_NidST " + str(N2O_NidST))
    Nremaining=Nremaining-N2O_NidST-NvolST
    EmidST=N2O_NidST*CF*GWP
    #print ("Nrem2 " + str(Nremaining))
    N2O_Ndla=Nremaining*EF3la
    #print ("N2O_Ndla " + str(N2O_Ndla))
    Nremaining=Nremaining-N2O_Ndla
    #print ("Nrem3 " + str(Nremaining))
    Emdla=N2O_Ndla*CF*GWP
    NvolLA=Nremaining*FracGASM
    #print ("NvolLA " + str(NvolLA))
    N2O_NvolLA=NvolLA*EF4
    #print ("N2O_NvolLA " + str(N2O_NvolLA))
    EMidLA=N2O_NvolLA*CF*GWP
    Nremaining=Nremaining-NvolLA
    #print ("Nrem4 " + str(Nremaining))
    Nleach=FracLEACH*Nremaining
    N2O_Nleach=EF5*Nleach
    Emidleach=N2O_Nleach*CF*GWP
    EMN2Ototal=Emdst+EmidST+Emdla+Emidleach
    Nremaining=Nremaining-Nleach
    EMN2Ototal
}
EMN2Ototal <- NremainingFunc()

MC1 <- mc(EF3st, EF3la, FracGASMS, EF4, FracGASM, EF5, FracLEACH, EMN2Ototal)
hist(MC1)
print(MC1)
