# monte carlo fracGASM

#parameters

# AEF3st=array.array('f',[.0025,.005,.01])
# AFracGASMS=array.array('f',[.15,.26,0.45])
# AEF4=array.array('f',[.002,.01,.05])
# AEF3la=array.array('f',[.005,.0125,.05])
# AFracGASM=array.array('f',[.05,.2,.5])
# AFracLEACH=array.array('f',[0,.007,.8])
# AEF5=array.array('f',[.005,.0075,.025])

#GWP=310
GWP=265
CF=44/28
#Nin = 290289
#Nin = 322461
#Nin = 337039
Nin = 312336

EF3st   <- 0.005
EF3la   <- .0125
FracGASMS   <- .26
EF4   <- .01
FracGASM   <- .2
EF5   <- .0075
FracLEACH   <- .007

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
    #Nremaining=Nremaining-NvolLA-N2O_NvolLA
    Nremaining=Nremaining-NvolLA
    #print ("Nrem4 " + str(Nremaining))
    Nleach=FracLEACH*Nremaining
    N2O_Nleach=EF5*Nleach
    Emidleach=N2O_Nleach*CF*GWP
    EMN2Ototal=Emdst+EmidST+Emdla+Emidleach
    #Nremaining=Nremaining-Nleach-N2O_Nleach
    Nremaining=Nremaining-Nleach
    EMN2Ototal
}
EMN2Ototal <- NremainingFunc()
print(EF3st)
print(EF3la)
print(FracGASMS)
print(EF4)
print(FracGASM)
print(EF5)
print(FracLEACH)
print(EMN2Ototal)
#MC1 <- mc(EF3st, EF3la, FracGASMS, EF4, FracGASM, EF5, FracLEACH, EMN2Ototal)
