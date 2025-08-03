# -*- coding: utf-8 -*-

# -----------------------------------------------------------------------------
# Sammlung aller Funktionen, auf die in der main Methode zugegriffen wird, 
# enthält keinen ausführbaren Code
# -----------------------------------------------------------------------------

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from scipy.integrate import quad
    


def komponentenauswahl (komp_namen, datei, arbeitsblatt, spalten, zeilen):
    ''' Liest aus einem Excel Arbeitsblatt die vorgegebenen Spalten und Zeilen
    einer Tabelle (hier der jeweiligen benötigten Stoffdaten ein und wählt
    dann nur die relevanten Zeilen mit den ausgewählten Komponenten aus.
    Der Einleseprozess ist über das Python package pandas realisiert. Die 
    eingelesenen Daten werden als dataframe übergeben
    '''
    auswahl = pd.read_excel(datei, sheet_name = arbeitsblatt, 
                   usecols = spalten, skiprows = (zeilen), index_col = 0)
    auswahl = auswahl.loc[komp_namen,:]
    return auswahl


def molenstroeme_start (n, m, F, K, nue, dampfanteil, n_feed, G_j, L_j):
    """Berechnet die Gesamtmolenströme L_j und G_j auf jeder Stufe auf Basis
    des äquimolaren Stofftransports. Erste Schätzung der Ströme zu Beginn.
    Gleichungen 54 im TTV 2 Skript
    """
    # Gesamtbilanz
    L_j[n-1] = F - K
    G_j[0] = K 
    # Bilanz um Kopf
    L_j[0] = K * nue  # L_0
    G_j[1] = K + L_j[0]

    for j in range(1, n - 1):
        
        if 1 <= j <= n_feed - 2: # Verstärkungsteil
            if n_feed >= 2:
                L_j[j] = L_j[0] 
                G_j[j+1] = G_j[1]    
            else:
                print("tu nichts")
                # tu nichts
                
        elif j == n_feed - 1: # Boden feedzugabe-1 (gasförmiger Feed)
            G_j[n_feed] = G_j[1] - dampfanteil*F
            L_j[n_feed-1] = L_j[0]
        elif j >= n_feed: # Abtriebsteil
            L_j[j] = L_j[0] + (1-dampfanteil)*F
            G_j[j+1] = L_j[j] - L_j[0]

    
def berechne_dampfdruck(SD_dampfdruck, SD_allgemein, T, p0_ji):
    """Berechnet anhand der Wagner-Gleichung die Dampfdrücke auf den
    einzelnen Böden und gibt diese als Matrix zurück
    Gleichung: 42b (Kap. D1, VDI Wämeatlas, 2019, 12. Auflage)
    """
    p0_ji = np.copy(p0_ji)
    T_r = np.ones(p0_ji.shape)
    T_r = ((T_r.T * T).T/SD_allgemein.loc[:,"Tc"].to_numpy())
    
    p0_ji = ((SD_dampfdruck.loc[:,"A"].to_numpy()) * (1-T_r)
               + SD_dampfdruck.loc[:,"B"].to_numpy() * (1-T_r)**1.5
               + SD_dampfdruck.loc[:,"C"].to_numpy() * (1-T_r)**2.5
               + SD_dampfdruck.loc[:,"D"].to_numpy() * (1-T_r)**5) / T_r
    
    p0_ji = (np.exp(p0_ji.astype(float))  # cast nötig, da object
               * SD_allgemein.loc[:,"Pc"].to_numpy()).astype(float)

    return p0_ji
     
    
def loese_Gleichungssystem (A_ji, L_i, v_ji, n, m):
    """Berechnet für jede Komponente die Gleichungsmatrix A_i und deren
    Inverse, anschließend wird die Inverse mit dem Loesungsvektor L_i
    multipliziert und das Ergebnis im Vektor v_ji gespeichert.
    Gleichung 55 im TTV 2 Skript
    """    
    for i in range (1, m + 1):
        diagonale_unten = np.delete(A_ji[:, i - 1], n - 1)
        diagonale_oben = np.ones(n - 1)
        hauptdiagonale = - (A_ji[:, i-1] + 1)
        A_i = (np.zeros((n, n)) + np.diag(hauptdiagonale, 0) 
               + np.diag(diagonale_oben, k=1) + np.diag(diagonale_unten, k=-1))
        v_ji[:, i-1]= np.dot(np.linalg.inv(A_i),(L_i[:, i-1]))*(-1)
 

def fun_f(theta, d_iber, b_iber, F, z_F, K):
    """Funktion für Theta Konvergenz, die mithilfe von 'fsolve' gelöst wird.
    Gleichung 65 im TTV 2 Skript
    """
    
    f = np.sum(F * z_F / (1 + theta * (b_iber / d_iber))) - K
    
    return f

    
def fun_g(T, x_jikor, SD_dampfdruck, SD_allgemein, p0_ji, p_ges):
    """Funktion für Boiling-Point Methode, die mithilfe von 'fsolve' gelöst
    wird. Gleichung 70 im TTV 2 Skript
    """

    if T.size == 1: # für skalare Temperaturwerte
        f = (np.sum(berechne_dampfdruck(SD_dampfdruck, SD_allgemein, T, p0_ji) 
                / p_ges * x_jikor) - 1)
        
    else: # für Temperaturvektor
        f = (np.sum(berechne_dampfdruck(SD_dampfdruck, SD_allgemein, T, p0_ji) 
                / p_ges * x_jikor, axis = 1) - 1)
        
    return f


def fun_h (T, R, koeff_cpi):
    ''' Berechnet aus der Temperatur, R und den übergebenen Koeffizienten A-G
    den cp-Wert bei einer bestimmten Temperatur und gibt ihn als Skalar zurück
    Gleichung: 51a (Kap. D1, VDI Wämeatlas, 2019, 12. Auflage)
    '''   
    
    A, B, C, D, E, F, G = koeff_cpi
    
    # Annahme: Moderate Temperaturen -> H Term vernachlässligbar wegen y^4
    y = T/(A+T)
    
    cp_ideal = R*(B+(C-B)*pow(y,2)*(1+(y-1)*(D+E*y+F*pow(y,2)+G*pow(y,3))))
    
    return cp_ideal


def berechne_dhv (T, Tc, R, dhv_koeff,delta_hv):
    ''' Berechnet aus der Temperatur, der kritischen Temperatur und den 
    übergebenen Koeffizienten A-E die Verdampfungenthalpie einer Komponente bei 
    einer bestimmten Temperatur und gibt sie als Skalar zurück
    Gleichung: 46b (Kap. D1, VDI Wämeatlas, 2019, 12. Auflage)
    '''
    
    # Iteration über Spalten als Kompenenten
    for i,A in enumerate(dhv_koeff):
        # Jede Schleife wird für eine Komponente durchgeführt
        t = np.ones(len(T))- T/Tc[i]
        delta_hv[:,i] = R*Tc[i]*(
                        A[0]*pow(t,1/3)
                        +A[1]*pow(t,2/3)
                        +A[2]*t
                        +A[3]*pow(t,2)
                        +A[4]*pow(t,6))        
       


def enthalpie_reinstoff (SD_allgemein, SD_cp_ideal, SD_delta_hv, T, h_gas, 
                         h_fluessig, R):
    """Berechnet Enthalpien der Gas- und Flüssigphase pro Komponente und 
    befüllt die Matrizen h_gas und h_flüssig. Berechnungsroute nach VDI
    Wärmeatlas, 2019, 12. Auflage, Kap.D1, S.181, Route A
    """
    
    # Bildungsenthalpien 1 x 3
    delta_hb0i = SD_allgemein.loc[:, "Dh0f"].to_numpy()
    # Tci 3 x 1
    Tc = SD_allgemein.loc[:, "Tc"].to_numpy()
    # Untere Integralgrenze. Obere Grenze ist jeweils T[j]
    T_standard = 298.15 # Kelvin
    
    # Separates Array für delta hv und das integrierte cp
    delta_hv = np.zeros(h_gas.shape)
    dcpdT = np.zeros(h_gas.shape)
    
    # panda dataframe wird in numpy array umgewandelt
    # so ist eine schnellere Berechnung der arrays möglich
    koeff_cp = SD_cp_ideal[["A", "B", "C", "D", "E", "F", "G"]].to_numpy()
    koeff_dhv = SD_delta_hv[["A", "B", "C", "D", "E"]].to_numpy()
    
    
    # # Verifikationspunkte für Methanol
    # T_ver_cp = 303.15
    # print("cp Test \n",fun_h(T_ver_cp,R,koeff_cp[0]),"\n")
    # T_ver_dh = 373.15*np.ones(len(T))
    # berechne_dhv(T_ver_dh,Tc,R,koeff_dhv,delta_hv)
    # print("dh Test\n",delta_hv,"\n")
    
    # Schleife nötig, da Array mit Funktionen nicht integriert werden kann
    for j in range(np.size(h_gas, 0)): # Zeilen = Böden
        
        for i in range(np.size(h_gas, 1)): # Spalten = Komponente
                dcpdT[j,i], _ = quad(lambda T_val: fun_h(T_val, R, koeff_cp[i])
                                ,T_standard,T[j])
                
                
    
    # Bildungsenthalpien der Komponenten zu nxm 
    n = np.size(h_gas,0)
    delta_hb0ji = np.tile(delta_hb0i, (n,1))
    
    # Berechnung der Verdampfungsenthalpien für alle Stufen und Komponenten
    berechne_dhv(T, Tc, R, koeff_dhv, delta_hv)
    # print(delta_hb0ji," Test1\n")
    # print(dcpdT," Test2\n")
    # print(delta_hv," Test3\n")
    
    h_gas = delta_hb0ji + dcpdT
    h_fluessig = h_gas - delta_hv
    
    return h_gas,h_fluessig

def h(h,z):
    return np.sum(h*z)

def berechne_xy_Feed(T,z,F,p0_f,xD,p):
    
    NiV = np.copy(z)
    Ni = z*F
    
    print(np.sum(z*p0_f),"Dampfdruck")
    
    q = xD/(1-xD)
    
    for i in range(len(z)):
        NiV[i] = q*Ni[i]*p0_f[i]/(p+q*p0_f[i])
    
    print(Ni, " Ni")
    print(NiV, " Ni")
    print(np.sum(NiV), " Summe")
    yF = NiV/np.sum(NiV)
    
    xF = yF*p/p0_f
    
    return xF,yF
    

def berechne_neue_molenströme(L_j,G_j,F,xd,hl,hg,hg_F,hl_F,nf,x,y,z):
    
    # Die hier verwendeten spez Enthalpien sind alle über die Komponenten
    # gemittelt und der Übersichtlichkeit geschuldet kurz benannt.
    
    L = np.copy(L_j)
    G = np.copy(G_j)
    
    
    
    h_k = np.sum(hl[0]*x[0])
    # h_s = np.sum(hl[-1]*x[-1])
    
    # Mittlere Enthalpie des Feeds
    h_F = xd*hg_F+(1-xd)*hl_F
    h_F_g = hg_F
    h_jG_zF = np.sum(hg*z,axis=1)
    
    # h_jG_yj = np.sum(hg*y,axis=1)
    
    h_fG_xf = np.sum(x[nf-1]*hg[nf-1])
    # hgj+1 xj. Gleicher Index erzeugt genau die gewünschte Verschiebung
    h_jG_xj = np.sum(hg*x,axis =1)
    # hgj+1 immer bei xk
    h_jG_xk = np.sum(hg*x[0],axis=1)
    # hl immer bei xs
    # h_jL_xs = np.sum(hl*x[-1], axis = 1)
    # hlj immer bei xj
    h_jL_xj = np.sum(hl*x, axis = 1)
    
    K = G[0]
    # S = L[-1]
    
    # 1. h Term nur weil T0 = T1
    Qk = L[0]*(h_jG_xj[0]-h_jL_xj[0])+K*(h_jG_xk[0]-h_k)
    
    # Sumpfverdampferwärmestrom
    # Qs = K*h_k+Qk-F*(xd*hg_F+(1-xd)*hl_F)+S*h_s
    
    '''
    Ab hier beginnt die Berechnung der Kolonne über Gl 78 a,b,c.
    Hier wird immer nach L_j aufgelöst und ab der Feedstufe
    der Enthalpiestrom vom Feed in die Bilanz aufgenommen.
    Alternativ kann für n>=n_feed um den Sumpf bilanziert werden um ab dort
    nach G_j aufzulösen.
    
    Disclaimer:
    Die variable Einstellung vom Dampfanteil ist nicht korrekt implementiert.
    Der Feed im Nassdampfgebiet ist auch im thermodyn Phasengleichgewicht.
    Deshalb ist grundsätzlich davon auszugehen, dass y_F != x_F != z_F.
    Zur Berechnung der x und y Feed ist eine Flashrechnung nötig.
    y_F wird auf der Feedstufe gebraucht, wenn nur der Gasanteil des Feeds
    nach oben strömt.
    '''        
    for j in range(1,len(L)-1):
        
        if 1 <= j <= nf-2:
            L[j]=(K*(h_k-h_jG_xk[j])
                       +Qk)/(h_jG_xj[j]-h_jL_xj[j])
            
            # G_j entweder über Molbilanz oder Enthalpiebilanz
            G[j+1] =(L[j]*h_jG_xj[j]+K*h_jG_xk[j])/np.sum(hg[j+1]*y[j+1])
            # G[j+1] = L[j] + K
        
        elif j == nf-1:          
            # Seperate Bestimmung von L aus Feedboden
            L[j]=(K*(h_k-h_jG_xk[j])
                  +xd*F*(h_fG_xf-h_F_g)
                  +Qk)/(h_jG_xj[j]-h_jL_xj[j])
            
            # G_j entweder über Molbilanz oder Enthalpiebilanz
            G[j+1] =(L[j]*h_jG_xj[j]+K*h_jG_xk[j]-xd*F*h_jG_zF[j]
                     )/np.sum(hg[j+1]*y[j+1])
            # G[j+1] = L[j] + G[0]-xd*F
            
        elif j >= nf:
            
            L[j]=(K*(h_k-h_jG_xk[j])
                  +F*(h_jG_zF[j]-h_F)
                  +Qk)/(h_jG_xj[j]-h_jL_xj[j])
            
            # G_j entweder über Molbilanz oder Enthalpiebilanz
            G[j+1] =(L[j]*h_jG_xj[j]+K*h_jG_xk[j]-F*h_jG_zF[j]
                     )/np.sum(hg[j+1]*y[j+1])
            # G[j+1]=L[j]+G[0]-F
            
            # Bestimmungsgleichung alternativ über Bilanz des Sumpfes
            # (siehe vorherige Version des Skriptes)
            # G[j+1] = ((S*(h_jL_xs[j]-h_s))
            #             +Qs)/(h(hg[j+1],y[j+1])-h(hl[j],y[j+1]))
            
            # L[j] = G[j+1] + L[-1]
    
    return L,G


def plot_2d_array(data, legend_labels=None, y_axis_title=None, plot_title=None):
    """
    Plot a 2D numpy array.

    Parameters:
        data (numpy.ndarray): 2D array to be plotted.
        legend_labels (list): List of string labels for the legend.
        y_axis_title (str): Title for the Y-Axis.
        plot_title (str): Title for the entire plot.
    """
    n, m = data.shape
    x_values = np.arange(n)

    plt.figure(figsize=(10, 6))

    for i in range(m):
        label = legend_labels[i] if legend_labels is not None else f'Column {i + 1}'
        plt.plot(x_values, data[:, i], label=label)

    plt.title(plot_title if plot_title is not None else '2D Array Plot')
    plt.xlabel('Stufe')
    plt.ylabel(y_axis_title if y_axis_title is not None else 'Values')
    
    # Nur Ticks bei ganzzahligen Werten als den Stufen
    plt.xticks(x_values)

    if legend_labels is not None:
        plt.legend()

    plt.grid(True)
    plt.show()
