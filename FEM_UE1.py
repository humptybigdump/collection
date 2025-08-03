import numpy as np
import scipy as sp
import sympy as sym
import matplotlib.pyplot as plt
from matplotlib import cm

###########
# EINGABE #
###########

# Netzgenerierung
elemente = 3
knoten = 3

# Knotenspezifikation
koordinaten = np.zeros((knoten,2))
koordinaten[0,:] = 1,0
koordinaten[1,:] = 0,0
koordinaten[2,:] = 1,1

# Koinzidenzmatrix (ordnet einem Element zwei globale Knoten zu)
koinzidenzmatrix = np.zeros((elemente,2))
koinzidenzmatrix[0,:] = 0,2
koinzidenzmatrix[1,:] = 1,0
koinzidenzmatrix[2,:] = 1,2

# E-Modul, Querschnittsfläche
EModul,A = 10.,10.

# Skalierungsfaktor für Visualisierung
mag=1

# Verschiebungsrandbedingungen [Knotennummer, Richtung, Wert]
RBU = np.array([[0,0,0],
                [0,1,0],
                [1,1,0]])

# Kraftrandbedingungen [Knotennummer, Richtung, Wert]
RBF = np.array([[2,0,1]])


##############
# Berechnung # 
##############

# Zuordnungsmatrizen
L = np.zeros([elemente,4,2*knoten])
for e in range(elemente):
    node1=int(koinzidenzmatrix[e,0])
    node2=int(koinzidenzmatrix[e,1])
    L[e,0,2*node1]=1
    L[e,1,2*node1+1]=1
    L[e,2,2*node2]=1
    L[e,3,2*node2+1]=1

LE = np.zeros([2*knoten,len(RBU)])
for i in range(2*knoten):
    for j in range(len(RBU)):
        k = int(2*RBU[j,0]+RBU[j,1])
        if (i==k):
            LE[i,j]=1

LF = np.zeros([2*knoten,2*knoten-len(RBU)])
j=0            
for i in range(2*knoten):
    if int(sum(LE[i,:]))==0:
        LF[i,j]=1
        j+=1          

# unreduzierte globale Größen
D = np.zeros(2*knoten)
F = np.zeros(2*knoten)
K = np.zeros([2*knoten,2*knoten])

# reduzierte globale Größen
DF = np.zeros([2*knoten-len(RBU)])
DE = np.zeros([len(RBU)])
FF = np.zeros([2*knoten-len(RBU)])
FE = np.zeros([len(RBU)])
KFF = np.zeros([2*knoten-len(RBU),2*knoten-len(RBU)])
KEE = np.zeros([len(RBU),len(RBU)])
KFE = np.zeros([2*knoten-len(RBU),len(RBU)])
KEF = np.zeros([len(RBU),2*knoten-len(RBU)])

# Elementgrößen
l = np.zeros(elemente)
S = np.zeros(elemente)
E = np.zeros(elemente)
Ke = np.zeros((elemente,4,4))
Re = np.zeros((elemente,4,4))

# Berechnung der Elementsteifigkeitsmatrizen
for i in range(elemente):
    node1=int(koinzidenzmatrix[i,0])
    node2=int(koinzidenzmatrix[i,1])
    node1x=koordinaten[node1,0]
    node1y=koordinaten[node1,1]
    node2x=koordinaten[node2,0]
    node2y=koordinaten[node2,1]
    phi = np.arctan2(node2y-node1y,node2x-node1x)
    l[i] = np.sqrt((node2x-node1x)**2+(node2y-node1y)**2)
    K0 = np.array([[ 1., 0,-1.,0.],
                   [ 0., 0, 0.,0.],
                   [-1., 0, 1.,0.],
                   [ 0., 0, 0.,0.]])
    
    Re[i,:,:] = np.array([[ np.cos(phi), np.sin(phi),          0.,          0.],
                         [-np.sin(phi), np.cos(phi),          0.,          0.],
                         [          0.,          0., np.cos(phi), np.sin(phi)],
                         [          0.,          0.,-np.sin(phi), np.cos(phi)]])
    
    Ke[i,:,:] = np.matmul(np.transpose(Re[i,:,:]),np.matmul(EModul*A/l[i]*K0,Re[i,:,:]))



# Assemblierung der globalen Steifigkeitsmatrix
for e in range(elemente):
    K += np.matmul(np.transpose(L[e,:,:]),np.matmul(Ke[e,:,:],L[e,:,:]))


# Assemblierung des globalen Lastvektors
for i in range(len(RBF)):
    F[int(2*RBF[i,0]+RBF[i,1])]=RBF[i,2]


# Systemreduktion
KFF = np.matmul(np.transpose(LF),np.matmul(K,LF))
KEE = np.matmul(np.transpose(LE),np.matmul(K,LE))
KFE = np.matmul(np.transpose(LF),np.matmul(K,LE))
KEF = np.matmul(np.transpose(LE),np.matmul(K,LF))
FF  = np.matmul(np.transpose(LF),F)
DE  = RBU[:,2]


# Gleichungslöser
DF = np.linalg.solve(KFF,FF-np.matmul(KFE,DE))


# Reaktionskräfte 
FE = np.matmul(KEE,DE) + np.matmul(KEF,DF)


# Zusammenstellung der globalen, unreduzierten Größen
D = np.matmul(LF,DF)+np.matmul(LE,DE)
F = np.matmul(LF,FF)+np.matmul(LE,FE)

# Spannungsberechnung
for e in range(elemente):
    ULOC = np.matmul(L[e,:,:],D)
    ULOCT = np.matmul(Re[e,:,:],ULOC)
    E[e] = (ULOCT[2]-ULOCT[0])/l[e]
    S[e] = EModul*E[e]


##############
# Ergebnisse # 
##############

# Ausgabe
print("Knotenverschiebungen:", D)
print("Knotenkräfte:", F)
print("Stabspannungen:", S)

# Visualisierung
Srange=max(S)-min(S)
# Ausgangslage
for e in range(elemente):
    node1=int(koinzidenzmatrix[e,0])
    node2=int(koinzidenzmatrix[e,1])
    # Undeformiert
    node1x=koordinaten[node1,0]
    node1y=koordinaten[node1,1]
    node2x=koordinaten[node2,0]
    node2y=koordinaten[node2,1]
    plt.plot([node1x,node2x],[node1y,node2y],color='k',lw=1)

# Endlage
for e in range(elemente):
    node1=int(koinzidenzmatrix[e,0])
    node2=int(koinzidenzmatrix[e,1])   
    # Deformiert
    node1x=koordinaten[node1,0]+ mag*D[2*node1+0]
    node1y=koordinaten[node1,1]+ mag*D[2*node1+1]
    node2x=koordinaten[node2,0]+ mag*D[2*node2+0]
    node2y=koordinaten[node2,1]+ mag*D[2*node2+1]
    plt.plot([node1x,node2x],[node1y,node2y],c=cm.bwr((S[e]-min(S))/Srange),lw=2) 

plt.xlabel("X-Position")
plt.ylabel("Y-Position")
plt.title("2D-Fachwerk")
plt.axis('equal')
plt.subplots_adjust(right=0.8) 
sub_ax = plt.axes([0.82, 0.11, 0.02, 0.77]) 
scalarcm = cm.ScalarMappable(cmap=cm.bwr)
scalarcm.set_array(S)
cbar = plt.colorbar(scalarcm, cax=sub_ax)
cbar.set_label("Stabspannung in MPa")
plt.show()