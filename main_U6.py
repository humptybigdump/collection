# -*- coding: utf-8 -*-

# Coding Cookie, steht in der 1. oder 2. Zeile des Codes und gibt Python an in 
# welchem Encoding das Skript geschrieben wurde. Hier Unicode UTF-8. 
# Ohne dieses Zeile kann Python im Standard nur Zeichen des Amerikanischen 
# Englisch verwenden, aber keine Umlaute

"""
Created on Thu Jan  9 16:03:26 2020

@author: Ute Koller
"""

# -----------------------------------------------------------------------------
# TTV 2 - Uebung 4
# Rektifikation eines Dreistoffgemischs
# -----------------------------------------------------------------------------

# Importieren aller benötigten Python packages
import numpy as np  # Python Bibliothek für Matrizen und numerische Berechnung
from scipy.optimize import fsolve # Bibliothek für numerische Algorithmen
# import matplotlib.pyplot as plt # Bibliothek zum Plotten von Daten

# Importieren aller Funktionen aus module.py
from modules_U6 import berechne_dampfdruck
from modules_U6 import loese_Gleichungssystem
from modules_U6 import molenstroeme_start
from modules_U6 import komponentenauswahl
from modules_U6 import fun_f
from modules_U6 import fun_g
from modules_U6 import enthalpie_reinstoff
from modules_U6 import berechne_neue_molenströme
from modules_U6 import plot_2d_array
from modules_U6 import berechne_xy_Feed


# -----------------------------------------------------------------------------
# Angabe der Kolonnenparameter:
# Absolutdruck, Feedstrom, Kopfstrom, Rücklaufverhältnis, Feedzusammensetzung,
# Dampfanteil, Anfangstemperatur
# -----------------------------------------------------------------------------
komp_namen = np.array(["Methanol", "Ethanol", "n-Propanol"])
n = 10                         # Anzahl Kolonnenstufen
m = komp_namen.size           # Anzahl Komponenten

R = 8.314                     # universelle Gaskonstante [J/(mol*K)]
T_0 = 310.93                  # Anfangstemperatur [K]
T = T_0*np.ones(n)            # Temperaturarray für alle Kolonnenstufen
p_ges = 1.013                 # Gesamtdruck [bar]

F = 100.0                        # Feedstrom [mol/h]
z_F = np.array([1/3, 1/3, 1/3])  # Feezusammensetzung
T_F = T_0                        # Feedtemperatur
n_feed = 4                       # Stufe der Feedzugabe
dampfanteil = 0.00                  # Dampfanteil Feed [0, 1]

K = 50                           # Kopfstrom [mol/h]
nue = 1                          # Rücklaufverhältnis
   
      
# Ausgabe der Anfangsparameter auf der Konsole
print("Folgende Parameter für die Kolonne wurden angegeben:")
print("Namen der Komponenten:")
print(komp_namen)
print("Anzahl Kolonnenstufen:\t", n)
print("Anzahl Komponenten:\t", m)
print("Anfangstemperatur [K]:\t", T_0)
print("Gesamtdruck Kolonne [bar]:\t", p_ges)
print("\n")
print("Feedstrom [mol/h]:\t", F)
print("Stufe für Feedzugabe:\t", n_feed)
print("Dampfanteil im Feed:\t", dampfanteil)
print("\n")
print("Kopfstrom [mol/h]:\t", K)
print("Rücklaufverhältnis:\t", nue)
print("\n")


# -----------------------------------------------------------------------------
# Stoffdaten (SD) für Dampfdruckberechnung und Enthalpieberechnung 
# aus Excel einlesen
# -----------------------------------------------------------------------------

# Exceldatei mit Stoffdaten, diese muss im gleichen Ordner wie das Pythonskript
# liegen
datei = "Eingabedatei.xlsx"

# allgemein
arbeitsblatt = "allgemein" # Export aus VDI Wärmeatlas D3.1
spalten = ["Substance German", "Formula", "Tc", "Pc", "Dh0f"]
SD_allgemein = komponentenauswahl(komp_namen, datei, arbeitsblatt, spalten, 3)
print("Allgemeine Stoffdaten:\n", SD_allgemein)
print("\n")

# Dampfdruck
arbeitsblatt = "Dampfdruck" # Export aus VDI Wärmeatlas D3.1
spalten = ["Substance German", "Formula", "A", "B", "C", "D"]
SD_dampfdruck = komponentenauswahl(komp_namen, datei, arbeitsblatt, spalten, 8)
print("Koeffizienten zur Dampfdruckberechnung (Wagner-Gleichung):\n", 
      SD_dampfdruck)
print("\n")

# Verdampfungsenthalpie
arbeitsblatt = "dhv" # Export aus VDI Wärmeatlas D3.1
spalten = ["Substance German", "Formula", "A",  "B", "C", "D", "E"]
SD_delta_hv = komponentenauswahl(komp_namen, datei, arbeitsblatt, spalten, 8)
print("Koeffizienten Berechnung der Verdampfungenthalpie (PPDS-Gleichung):\n", 
      SD_delta_hv)
print("\n")

# cp_ideal
arbeitsblatt = "cp ideal" # Export aus VDI Wärmeatlas D3.1
spalten = ["Substance German", "Formula", "A",  "B", "C", "D", "E", "F", "G"]
SD_cp_ideal = komponentenauswahl(komp_namen, datei, arbeitsblatt, spalten, 13)
SD_cp_ideal.fillna(0, inplace = True)
print("Koeffizienten Berechnung der cp-Werte (PPDS-Gleichung):\n", 
      SD_cp_ideal)
print("\n")

# -----------------------------------------------------------------------------
# Initialisierung aller im weiteren Verlauf benötigten Felder
# -----------------------------------------------------------------------------

# Variablen Übung 4
p0_ji = np.zeros((n, m))     # Dampfdrücke der Komponenten in jeder Stufe [bar]
K_ji = np.zeros((n, m))  # Gleichgewichtskonstanten [-]
A_ji = np.zeros((n, m))  # Absorptionskoeffizienten [-]
v_ji = np.zeros((n, m))  # Komponentenstrom Gasphase [mol/h]
l_ji = np.zeros((n, m))  # Komponentenstrom Fluessigphase [mol/h]
y_ji = np.zeros((n, m))  # Zusammensetzung Gasphase [-]
x_ji = np.zeros((n, m))  # Zusammensetzung Fluessigphase [-]
L_i = np.zeros((n,m))    # Lösungsvektor

# Variablen Übung 5
v_jikor = np.zeros((n, m))  # korrigierte Komponentenströme Gasphase [mol/h]
l_jikor = np.zeros((n, m))  # korrigierte Komponentenströme Fl-Phase [mol/h]
y_jikor = np.zeros((n, m))  # korrigierte Zusammensetzung Gasphase [-]
x_jikor = np.zeros((n, m))  # korrigierte Zusammensetzung Fl-Phase [-]
theta = 0.0

# Variablen Übung 6
'''...Initialisierung wichtiger Variablen Übung 6...'''
p_feed = p_ges*np.ones(n)

# Stoffstromvektoren
G_j = np.zeros(n)  # enthält später [K, G_1, ... G_(n-1)]
L_j = np.zeros(n)  # enthält später [L_0, L_1, ..., L_(n-2), S]
x_jin = np.zeros((n,m))
y_jin = np.zeros((n,m))
# Enthalpien der Einzelkomponenten auf jeder Stufe    
h_gas = np.zeros((n,m))
h_fluessig = np.zeros((n,m))
# mittlere spez Enthalpie des Feedstroms
h_g_feed = np.zeros((1,m))
h_l_feed = np.zeros((1,m))


# Berechnung der Molströme nach äquimolarem Stofftransport
# Darf nicht in while Schleife stehen!!!
molenstroeme_start (n, m, F, K, nue, dampfanteil, n_feed, G_j, L_j)

# Enthalpie im Feed
p0_feed = berechne_dampfdruck(SD_dampfdruck, SD_allgemein, T_F, p0_ji)
K_feed = p0_feed[n_feed,:]/p_ges  # GGW_Konstante Feed

# Reinstoffenthalpien des Feeds
T_F_l = T_F*np.ones(1)
h_g_feed,h_l_feed = enthalpie_reinstoff(SD_allgemein, SD_cp_ideal, SD_delta_hv,
                                           T_F_l, h_g_feed, h_l_feed, R)

# Dampfzusammensetzung vom Feed muss eigentlich separat berechnet werden
# Es wäre hier eine Flashrechnung anzusetzen
if dampfanteil > 0 and dampfanteil <1:
    xF,yF = berechne_xy_Feed(T, z_F, F, p0_feed[0], dampfanteil, p_ges)
elif dampfanteil == 1 :
    yF = z_F
    xF = np.copy(z_F)*0
else:
    xF = z_F
    yF = np.copy(z_F)*0

h_mg_F = np.sum(h_g_feed*z_F)
h_ml_F = np.sum(h_l_feed*z_F)

# -----------------------------------------------------------------------------
# WHILE-SCHLEIFE FUER THIELE-GEDDES METHODE
# läuft solange sich die Molenbrüche x_ji und y_ji um eine Differenz > epsilon
# ändern, maximal 100 Druchläufe!
# -----------------------------------------------------------------------------

# Initialisierung von Variablen
eps1 = 1
eps2 = 1
max_iterations = 100
iteration = 0
krit = 1e-9

# While-Schleife
while (eps1 > krit and eps2 > krit) and iteration < max_iterations:
    
# -----------------------------------------------------------------------------
# Start der Berechnungen für Übung 4
# -----------------------------------------------------------------------------
       
    # Berechnung der Dampfdrücke der für jeden Kolonnenboden    
    p0_ji = berechne_dampfdruck(SD_dampfdruck, SD_allgemein, T, p0_ji)
    
    # Befüllen des Lösungsvektors
    L_i[n_feed, :] = F * z_F
    
    # Berechnung der Gleichgewichtskoeffizienten
    K_ji = p0_ji / p_ges
    
    # Berechnung der Absorptionskoeffizienten, Gl. 29 und 34, TTV 2 Skript
    # Transponieren nötig, da G_j Zeilenvektor
    A_ji = np.transpose(L_j / G_j / np.transpose(K_ji)) 
    A_ji[0] = L_j[0] / G_j[0]  # Kopfkondensator (total)
    
    # Aufstellen des Gleichungssystem mit A_ji und auflösen, 
    # um v_ji zu erhalten, Gl. 48, TTV 2 Skript
    loese_Gleichungssystem(A_ji, L_i, v_ji, n, m)
    l_ji = A_ji * v_ji
    
    # # Berechnung der Zusammensetzung von Gas- und Flüssigphase
    # y_ji = np.transpose(np.transpose(v_ji) / G_j)
    # x_ji = np.transpose(np.transpose(l_ji) / L_j)
    
    # -------------------------------------------------------------------------
    # theta-Konvergenz, man erhält die korrigierten Komponentenströme
    # l_jikor, v_jikor
    # -------------------------------------------------------------------------
    
    d_iber = v_ji [0]
    b_iber = l_ji [len(l_ji) - 1]
    
    theta = fsolve(fun_f, theta, (d_iber, b_iber, F, z_F, K))
    
    d_ikor = F * z_F / (1 + theta * (b_iber / d_iber)) 
    l_jikor = d_ikor/d_iber * l_ji
    v_jikor = d_ikor/d_iber * v_ji
        
    for k in range(0, n):
       y_jikor[k] = v_jikor[k]/np.sum(v_jikor[k])
       x_jikor[k] = l_jikor[k]/np.sum(l_jikor[k])
       
    # -------------------------------------------------------------------------
    # Boiling-Point Methode 
    # -------------------------------------------------------------------------
    # Aufruf von 'fsolve' mit erweiterter Ausgabe, um Informationen über den 
    # Iterationsverlauf zu 
    
    T, infodict, ier, mesg = fsolve(fun_g, T, (x_jikor, SD_dampfdruck, 
                                               SD_allgemein, p0_ji, p_ges), 
                                               full_output = True)
    # -------------------------------------------------------------------------
    # Beginn Übung 6: Methode der konstanten Zusammensetzung
    # -------------------------------------------------------------------------

    
    h_gas,h_fluessig = enthalpie_reinstoff(SD_allgemein, SD_cp_ideal, SD_delta_hv,
                                           T, h_gas, h_fluessig, R)    
    
    # mittlere Enthalpien auf den Kolonnenböden nach Skript
    h_mg = np.sum(h_gas*y_jikor,axis=1)
    h_ml = np.sum(h_fluessig*x_jikor,axis=1)
    
    # -------------------------------------------------------------------------
    # Energiebilanzen
    # -------------------------------------------------------------------------
    
    # Berechnung der neuen Molenströme
    L_j,G_j = berechne_neue_molenströme(L_j, G_j, F, dampfanteil,
                                        h_fluessig, h_gas, h_mg_F, h_ml_F,
                                        n_feed, x_jikor, y_jikor, z_F)                
    # Vergleich der Molenbrüche neu gegen alt
    dxji = x_jikor-x_jin
    dyji = y_jikor-y_jin
    eps1 = sum(sum(abs(row)) for row in dxji)
    eps2 = sum(sum(abs(row)) for row in dyji)
        
    # Zwischenspeichern der Werte x_jikor und y_jikor als "alte" Werte zum 
    # Vergleich mit der nächsten Iteration
    x_jin = np.copy(x_jikor)
    y_jin = np.copy(y_jikor)
    
    # Erhöhe die Iterationszählung
    iteration += 1
    if iteration % 5 == 0:
        print("Iteration Nr: ",iteration)
        print("Epsilon 1: ",eps1, " Epsilon 2: ", eps2, "\n")
    
# -----------------------------------------------------------------------------
# ENDE DER WHILE SCHLEIFE
# -----------------------------------------------------------------------------

# Schleifenende
if eps1 <= krit:
    print("Nach ",iteration,"Iterationen wurde die Schleife beendet.\n")
else:
    print("Die Schleife wurde 100 Mal ohne Konvergenz durchlaufen.\n")

# -----------------------------------------------------------------------------
# # Konsolenausgabe der Ergebnisse
# -----------------------------------------------------------------------------
print("Temperaturen \n", T,"\n")
print("Gasströme \n", G_j,"\n")
print("Flüssigkeitsströme \n", L_j,"\n")
print("Flüssigkeitszusammensetzung \n", x_jin,"\n")
print("Gaszusammensetzung \n", y_jin,"\n")


# -----------------------------------------------------------------------------
# Plotten der Graphen
# -----------------------------------------------------------------------------

Tplot = np.reshape(T, (-1, 1))

plot_2d_array(x_jin,komp_namen,"Molenbrüche / -","Molenbrüche $x_{ji}$")
plot_2d_array(y_jin,komp_namen,"Molenbrüche / -","Molenbrüche $y_{ji}$")
plot_2d_array(Tplot,None,"T / K","Temperaturen")
