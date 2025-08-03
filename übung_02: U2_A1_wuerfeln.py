#!/usr/bin/env python3
# -*- coding:utf-8 -*-
#
# Importiere benötigte Pakete
# Mathematikpaket
import numpy as np
# Paket zum Erstellen von Schaubildern
import matplotlib
import matplotlib.pyplot as plt
# Paket fuer statistische Methoden und Zufallsverteilungen
from scipy import stats

# Setze Optionen fuer den Stil der matplotlib Grafiken
matplotlib.rcParams['figure.autolayout'] = True
matplotlib.rcParams['font.size'] = 24

# Anzahl der Würfelrunden
n_sets = 20

# Anzahl der Würfe pro Runde
n_wuerfe = 5

# Seiten des ''Würfels''
n_seiten = 6

# Ziehe n_sets x n_wuerfe gleichverteilte Zufallszahlen zwischen 1 und 6
seed = 12345
zufallszahlengenerator = np.random.default_rng( seed )
zufallszahlen = zufallszahlengenerator.integers( low=1, high=n_seiten+1, size=(n_sets, n_wuerfe))

# Gebe Ergebnis auf dem Terminal aus
print("{} Messreihen zu je {} Würfen:".format(n_sets, n_wuerfe))
print(zufallszahlen)
# Das Objekt zufallszahlen ist ein 2D array bestehend aus den Wuerfen pro Runde
# print(type(zufallszahlen))
# print(len(zufallszahlen))

# Erstelle 1D array aller Wuerfe
alle_wuerfe = zufallszahlen.flatten()
print("Alle {} Würfe:".format(n_sets*n_wuerfe))
print(alle_wuerfe)
# print(len(alle_wuerfe))

# Mittelwert der Augenzahlen für jede Runde
# Erstelle 1D array mit den Mittelwerten der einzelnen Runden
mittelwerte = zufallszahlen.mean(axis=1)
print("Die {} Mittelwerte:".format(n_sets))
print(mittelwerte)

# Erstellen der Schaubilder
# Erstelle eine Leinwand mit 2 Schaubildern
fig, axes = plt.subplots(2)
# fig = Figure Objekt
# Setze Größe in Zoll (Breite, Höhe)
fig.set_size_inches(8, 10)
# axes = Axen der beiden Schaubilder
axis_means = axes[0]
axis_wuerfe = axes[1]

# Histogrammiere Ergebnisse
schritt = 0.5
bingrenzen_wuerfe = np.arange(0.75, n_seiten+0.75, schritt)
bingrenzen_means = np.arange(0.75, n_seiten+0.75, schritt)
axis_wuerfe.hist(alle_wuerfe, bins=bingrenzen_wuerfe)
axis_means.hist(mittelwerte, bins=bingrenzen_means)

# Schaubild aller Wuerfe
# Grafik plotten
axis_wuerfe.plot()

# Verschönerung der Darstellung
axis_wuerfe.set_xlabel(r'Würfelwurf')
axis_wuerfe.set_ylabel(r'Häufigkeit')
axis_wuerfe.grid(True)

# Schaubild der Mittelwerte
# Anpassung einer Normalverteilung an die Verteilung der Mittelwerte
fitmittel, standardabweichung = stats.norm.fit(mittelwerte)
print("Ergebnis der Anpassung einer Normalverteilung an die Mittelwertsverteilung:\n Mittelwert {}, Standardabweichung {}".format(fitmittel, standardabweichung))

# Verschönerung der Darstellung
axis_means.set_xlabel(r'Mittlere Augenzahl')
axis_means.set_ylabel(r'Häufigkeit')
axis_means.grid(True)

# Grafiken abspeichern und zeigen
fignam = "U2_A1_wuerfeln.pdf"
print("Speichere erstellte Grafiken in {}".format(fignam))
plt.savefig(fignam)
plt.show()
exit()
