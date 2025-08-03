# -*- coding: utf-8 -*-
"""
Statistische Analyse mit Python: Grundlegende Funktionen
Hendrik Andersen, 2020
hendrik.andersen@kit.edu
"""
#%% 
"""
In Spyder lassen sich mit '#%%' separate Zellen anlegen.
Zellen lassen sich isoliert ausführen (strg + enter).
einzelne Zeilen lassen sich mit F9 ausführen.
Das Gesamte Script kann über F5 ausgeführt werden.
Alternativ gibt es in der oberen Bedienungsleiste entsprechende Buttons.
"""
#####################
# Grundlegendes:
# Module, Variablen
#####################
"""
Pakete/Module enthalten Funktionen, die nach dem Import ausgeführt werden können.
Pakete sind Sammlungen von Modulen.
Syntax: paket.modul.funktion(Argumente)
Die Funktion getcwd (getcwd -> get current working directory) gibt das 
aktuelle Arbeitsverzeichnis an.
In der Hilfe (oberes rechtes Panel) sind Pakete, Module und Funktionen 
detailliert beschrieben
"""

# Module/Pakete importieren funktioniert über import modulName
import os
os.getcwd()

# Arbeitsverzeichnis definieren (chdir -> change directory)
# Hier bitte den korrekten Pfad zur Datei einfügen
# Empfohlen: Ordner für den Kurs errichten, Datei rein, Pfad entsprechend angeben.
# Unter Windows müssen die Backslashes in Slashes umgewandelt werden.
os.chdir('/Users/hendrikandersen/edu/KIT/oekologische_klimafolgen/ws_23_24/python/') 

# Variablen definieren - Alles nach einem #-Zeichen ist ein Kommentar
x = 11 # Ganze Zahlen: integer
y=22.21  # Kommazahl: float; Leerstellen (bspw. vor/nach "=" oder ",") sind egal 
f = "hello" # Zeichen zwischen "" oder '' nennt man strings (Textinformation)

# Berechnungen ausfuehren
z = x*y
print(z)

#%% 
#####################
# Listen
#####################
"""
In Listen können Daten verschiedener Datentypen (string, integer, float) 
gespeichert werden. Listen sind immer 1-Dimensional (alle Werte stehen hintereinander)
"""
l_1 = [3,6,5,17,9,2,8]   # Listen in eckige Klammern
l_1[2] # den dritten Wert in der Liste anzeigen (Zaehlung beginnt bei 0)
l_2 = [x,y,f,z,4.5102] # Listen können unterschiedliche Datentypen abspeichern
l_2[2]
l_3 = [l_1,l_2,l_2[4]] # Listen können auch Listen enthalten
f[2] # Indizierung funktioniert auch bei strings

"""
Aufgabe:
    Schauen Sie sich im Variablenmanager die definierten Variablen an.
"""
#%%
########################
# Arrays
# Künstl. Daten erzeugen
########################
"""
Arrays können mehrdimensional sein, sind aber weniger flexibel als Listen 
Arrays enthalten immer nur einen Datentyp (bspw. float)
Das Paket numpy ist sehr beliebt für wissenschaftliche Analysen mit 
mehrdimensionalen Arrays
"""
import numpy as np # man kann Pakete mit einer Abkürzung benannt importieren um sich etwas Schreibarbeit zu sparen...
a_1 = np.array([4,5,6,7,8])
a_2 = np.random.rand(5)
print(a_2)
a_3 = np.random.rand(5,3)
print(a_3.shape)
print(a_3)

#%%
################
# Daten Plotten
################
"""
Das Modul matplotlib.pyplot enthält viele Funktionen zum Plotten von Daten.
Möglich sind Linienplots, Scatterplots, Histogramme, Barplots, 2D plots, 3D...
Lassen Sie sich in der Gallerie inspirieren: https://matplotlib.org/3.1.1/gallery/index.html
"""
import matplotlib.pyplot as plt


plt.plot(np.arange(10))
plt.show()

plt.plot(np.arange(10), marker='+')
plt.show()

plt.plot(np.sin(np.arange(10)))
plt.show()


plt.plot(l_1) 
plt.show()

plt.plot(l_2) # Diese Darstellung funktioniert zwar, ist aber wohl wenig sinnvoll...
plt.show()

x = np.arange(0, 10, 0.1) # Schau in der Hilfe nach um zu verstehen, was np.arange() macht
y = np.sin(x) # Schau in der Hilfe nach um zu verstehen, was np.sin() macht
plt.plot(x, y)
plt.show()

plt.plot(a_3) # Verschiedene Linien können direkt auf einmal dargestellt werden
plt.show()

plt.imshow(a_3) # Dieser 2D Array kann natürlich auch zweidimensional dargestellt werden.
plt.colorbar(label='values [unitless]')
plt.show()

a_4 = np.random.rand(10000) 
plt.hist(a_4) 
plt.show()

a_5 = np.random.randn(10000) 
plt.hist(a_5) 
plt.show()

ax = plt.subplot()
ax.plot(a_5, c='black', label='data')
ax.set_xlabel('index')
ax.set_ylabel('value')
plt.legend(loc='upper right')
plt.title('Titel')
plt.show()

"""
Aufgaben:
    1) Lesen Sie in der Hilfe von np.random nach: Was ist die Korrekte Funktion
    um zufällige gleichverteilte Ganzzahlen (Integer) zu generieren?
    2) Erzeugen sie einen Scatterplot mit a_4 und a_5. Schauen sie in der Hilfe 
    nach und versuchen Sie einen Weg zu finden, in dem Plot das Überlappen der 
    einzelnen Punkte zu berücksichtigen (hier gibt es verschiedene Lösungen).
"""

#%%
##############################
# Daten einlesen und speichern
# Daten Indizieren
##############################
"""
Daten können natürlich gespeichert oder eingelesen werden, dafür stehen in numpy,
aber auch in zahlreichen anderen Modulen/Paketen Funktionen zur Verfügung.
Besonders interessant ist das Paket Pandas, das im Gegensatz zu Numpy mit 
'Dataframes' arbeitet, und integrierte Funktionalität für Zeitserien beinhaltet.
"""
np.savetxt(fname = 'normal.csv', X=a_5)
a_6 = np.loadtxt(fname = 'normal.csv') # np.genfromtxt() funktioniert auch

# Daten von einer URL herunterladen
URL = 'https://data.seattle.gov/api/views/65db-xm6k/rows.csv?accessType=DOWNLOAD'
from urllib.request import urlretrieve
urlretrieve(URL, 'Fremont.csv') 

# Daten als Pandas Dataframe einlesen
import pandas as pd 
df = pd.read_csv('Fremont.csv')
df
df.index # Indexspalte ausgeben
df.columns # Spaltennamen ausgeben
df = pd.read_csv('Fremont.csv', index_col='Date', parse_dates=True)
df.head()# head() zeigt nur die ersten 5 Zeilen
df.index
# Können Sie den Unterschied der beiden Einleseroutinen erkennen?

# Befehle lassen sich auch übersichtlich auf mehere Zeilen verteilen. Beim
# Ausführen per F9 müssen dann alle Zeilen markiert sein.
# Umbennen der Spalten für einfachere Bedienung
df = df.rename(columns={"Fremont Bridge Sidewalks, south of N 34th St": "total", 
                        "Fremont Bridge Sidewalks, south of N 34th St Cyclist East Sidewalk": "east",
                        "Fremont Bridge Sidewalks, south of N 34th St Cyclist West Sidewalk": "west"}) 
df.columns # Spaltennamen ausgeben

# Verschiedene Arten des Indizierens in Pandas
df.index[4]
df['total'][df.index[4]]
df.iloc[1] # iloc: indizieren über Index (integer)
df.loc['2012-10-03 01:00:00'] # loc indizieren über Labels 
df['total']['2012-10-03'] # spezifischer Tag
df['2013-10':'2013-12'] # Oktober, November, Dezember 2012
df.between_time('01:00', '03:00') # alle Daten zwischen 1 und 3 Uhr (nachts)

#%%
######################
# Dataframe operations
######################
"""
Mit einem Pandas Dataframe können direkt Funktionalitäten angesprochen werden:
    1) Plotting aus matplotlib 
    2) Berechnungen aus numpy
    3) und mehr...
"""
df.plot() # Pandas verwendet automatisch die Labels aus dem Dataframe 
df.resample('w').mean().plot() # Beim Resamplen kann die (hier zeitl.) Auflösung verändert werden
df.resample('m').mean().rolling(12).mean().plot()

df.groupby(df.index.weekday).mean().plot()

X = df['east'].resample('m').mean()
y = df['west'].resample('m').mean()
z = df['total'].resample('m').mean()

"""
Aufgaben:
    1) Plotten sie die mittlere Anzahl an Radfahrenden im Wochenverlauf (Mo, Di, ...)
    Tipp: benutzen Sie groupby 
    2) Plotten sie ein 12-monatiges rollendes Monatsmittel 
    3) Wie viele Radfahrer (insgesamt) sind im Januar 2015 über die Brücke gefahren?
"""
#%%
######################
# Korrelationen
# Trendanalyse
######################
"""
Für statistische Analysen gibt es verschiedene Pakete: Numpy, Scipy, Sklearn...
"""
# Korrelation berechnen
from scipy import stats # Das Modul stats im Paket scipy enthält viele nützliche Funktionen
r, p = stats.pearsonr(X, y)
# np.corrcoef berechnet 
corr_matrix = np.corrcoef([X,y,z,np.random.rand(z.size)]) 
plt.imshow(corr_matrix, cmap = plt.cm.RdBu_r, vmin = -1, vmax = 1)
plt.colorbar(label = 'Pearson r'); plt.show()  # Semikolon bedeutet neue Zeile

# scatterplots können Zusammenhänge darstellen
plt.scatter(X, y)
plt.show()

# Regressionsanalyse
slope, intercept, r, p, std_err = stats.linregress(X, y)  # perform linear regression

ax = plt.subplot(111)
ax.scatter(X, y)
ax.set_xlabel('east'); ax.set_ylabel('west')
ax.plot(X, intercept + slope*X, 'r')
plt.show()

"""
Ein weiteres sehr mächtiges Paket für Statistik und maschinelles Lernen ist Sklearn (Scientific-Kit für Machine Learning).
Wir können hier die Funktion LinearRegression nutzen, die im Grunde das gleiche macht 
wie scipy.stats.linregress, aber etwas unterschiedlich anzusprechen ist. Der zentrale 
Unterschied ist, dass in sklearn alles auf multivariate Statistik/ML ausgerichtet ist,
und somit die Funktionen in sklearn erwarten, dass "X" aus mehreren Prädiktoren besteht (und somit 
die dimensionalität (shape) von n x m hat (shape von (n,m)), wobei n die einzelnen Datenpunkte
sind und m die Prädiktoren. Dies wird selbst dann erwartet, wenn nur ein Prädiktor existiert.)).
"""
from sklearn.linear_model import LinearRegression
regr = LinearRegression()

# Hier erzeugen wir eine zusätzliche Dimension damit die Funktion LinearRegression() funktioniert
X_ = np.expand_dims(X, axis = 1)
print(X.shape, X_.shape)

# die Regression fitten
regr.fit(X_,y)
# Mit dem gefitteten Modell können wir vorhersagen machen
y_pred = regr.predict(X_)

fig, ax = plt.subplots()
ax.scatter(X, y, label='original data')
# Die Regressionlinie können wir zeichnen indem wir X vs. die vorhergesagten Werte als Linie plotten
ax.plot(X, y_pred, color='r', label='fitted line')
ax.set_ylabel('y')
ax.set_xlabel('X')
plt.show()


# Wie ihr festellen könnt, sind die Modellkoeffizienten (Bspw. die Steigung der Regressionsgerade)
# nicht identisch (nur fast), da sie mit unterschiedlicher Genauigkeit abgespeichert werden.
# Die Funktion isclose() aus dem Paket math bestätigt, dass sie quasi identisch sind.
print(slope)
print(regr.coef_)
print(slope==regr.coef_)

import math
print(math.isclose(slope,regr.coef_))

"""
Der Vorteil der Funktionen von Sklearn ist, dass alle Funktionen die gleiche Formatierung
von X und y erwarten und die gleiche Syntax (.fit(), .predict(), .score(), ...) hat.
So können sehr einfach andere Methoden auf die Datensätze angewendet werden.

Bei Regressionsanalysen wollen wir natürlich wissen, wie "gut das Modell funktioniert",
also die Vorhersagekraft quantifizieren. Das können wir zum Beispiel, indem wir 
Metriken wie R² (erklärte Varianz), RSS (residual sum of squares) and RSE 
(residual standard error), oder root mean square error (RMSE) oder ähnliches berechnen.
"""

fig, ax = plt.subplots()
ax.scatter(X, y, label='original data')
ax.plot(X, y_pred, color='r', label='fitted line')
ax.set_ylabel('y')
ax.set_xlabel('X')
ax.vlines(X_,y_pred,y_pred+(y-y_pred), linestyle=':', label='residuals')
ax.legend()
ax.set_xticks(())
ax.set_yticks(())
plt.show()



# Datengruppen darstellen und Unterschiede auf Signifikanz prüfen
plt.boxplot([X,y],labels=['east','west'])
plt.show()
# Die Anzahl der Radfahrenden in westliche und östliche Richtung ist unterschiedlich -
# aber ist der Unterschied signifikant?

t,p = stats.ttest_ind(X, y) # t ist die T-Statistik, p ist der beidseitige Signifikanzwert
p < 0.01 # Dies ist eine logische Abfrage. Die Antwort darauf kann True oder False sein. (boolean)

"""
Aufgaben:
    1) Berechnen sie Korrelationen und die Regressionsanalyse in unterschiedlichen
    zeitlichen Auflösungen (täglich, wöchentlich, monatlich).
    2) Welche Vorhersage für 'west' trifft das Regressionsmodell wenn 'east'=45
    (tägliche Auflösung)
    3) Schau in der Hilfe von ttest_ind nach: Was besagt der Test genau, und 
    sind die Unterschiede zwischen X und y auf dem 0.05 Level signifikant?
"""
#%%
#################
# räumliche Daten
#################
"""
Wie für alles im Leben führen hier viele Wege zum Ziel. 
Das Paket xarray ist eine neue Entwicklung, in der die Pandas Funktionalität 
für mehrdimensionale (räumliche) Daten erweitert wurde, was sehr effizientes 
Arbeiten mit räumlichen Daten ermöglicht.
"""
import xarray as xr

# Beispieldatensatz laden
airtemps = xr.tutorial.open_dataset('air_temperature')
# Die Daten anschauen
airtemps
"""
Kurze Erkläuterung:
    In xarray werden Daten typischerweise in Datensätzen (xarray.Dataset) gespeichert.
    Diese Datensätze können einen oder mehrere Variablen enthalten, die in xarray.DataArrays
    gespeichert sind. In diesem Beispiel enthält airtemps eine Variable (air):
        airtemps.air
        <xarray.DataArray 'air' (time: 2920, lat: 25, lon: 53)>
        Dieser DataArray enthält also 2920 Zeitschritte, entlang 25 Breitengeraden usw. 
    Mit diesen DataArrays kann wie in Pandas gearbeitet werden (Berechnungen und Plots)
    So erzeugt der Befehl
    airtemps.air.mean(dim='time').plot()    
    Eine räumliche Darstellung der mittleren Temperatur (Die Berechnung des 
    Mittelwert passiert nur entlang der definierten Dimension).
    Eigentlich steckt hinter einem Dataarray ein numpy.array, der über den Befehl 
    'values' bearbeitet werden kann.
    airtemps.air.values
    So kann man, wenn man möchte, auch mit der Standard Numpy Syntax arbeiten.
"""

# Daten können ebenfalls mit xarray abgespeichert werden, bevorzugter Datentyp
# ist netcdf (.nc), welches in den Erdsystemwissenschaften sehr populär ist.
airtemps.to_netcdf('airtemps.nc') # abspeichern
airtemps = xr.open_dataset('airtemps.nc') # erneut öffnen

# Im Gegensatz zu Pandas wird in xarray über sel/isel indiziert (loc/iloc in Pandas)
airtemps.air.isel(time=0).plot(); plt.show() # 0ten Zeitschritt selektieren
airtemps.air.sel(time='2013-01-01T00').plot(); plt.show() # spezifischen Zeitschritt über String
airtemps.air.sel(time='2013-01-01T00').plot.hist(); plt.show()

airtemps.air.sel(lon=260,lat=50).plot(); plt.show() # Selektion eines spezifischen Pixels
airtemps.air.sel(lon=260,lat=50).resample(time='1m').max().plot(); plt.show() # jeden Monat Maximaltemperatur
airtemps.air.sel(lon=360-122.335167,lat=47.608013,method='nearest').mean() # nächsten Pixel an Seattle wählen


avg_t_seattle = airtemps.air.sel(lon=360-122.335167,lat=47.608013,method='nearest').resample(time='d').mean()
bikers = df['total'].resample('d').sum()["2013-01-01":"2014-12-31"] # Selektion des Zeitraums der Reanalysedaten
r,p = stats.pearsonr(avg_t_seattle, bikers)

airtemps.air.mean(dim=['lon','lat']).plot(); plt.show() # mitteln entlang zweier Achsen (hier der gesamte geogr. Raum)
airtemps.air.mean(dim='time').plot(); plt.show() # Mittel über die gesamte Zeit

# Was passiert hier?
airtemps.air.groupby('time.hour').mean(dim='time').plot.imshow(col='hour', col_wrap = 2)
plt.show()

# Besteht ein Zusammenhang zwischen der Anzahl von Radfahrenden und der Temperatur?
dailyMeanT_seattle = airtemps.air.sel(lon=-122.335167,lat=47.608013,method='nearest'
                                      ).resample(time='d').mean(dim='time')
dailySumB_seattle = df['total'].resample('d').sum()
dailySumB_seattle_select = dailySumB_seattle[dailyMeanT_seattle.time[0]:dailyMeanT_seattle.time[-1]]

plt.scatter(dailyMeanT_seattle,dailySumB_seattle_select)

"""
Aufgaben:
    1) Plotten Sie den Jahresverlauf Temperatur (Monatsmittel) für New York
    2) Versuchen Sie, die mittlere Temperatur für jede Uhrzeit mit einem Befehl
    als separate Karten darzustellen
    3) Wie ist die Korrelation zwischen Tagesmitteltemperatur in Seattle
    und der Summe der Radfahrenden auf Fremont Bridge?
"""

#%%
#######################
# Kartendarstellungen #
#######################
"""
In python stehen verschiedene Pakete zum Plotten von Daten auf Karten zur 
Verfügung (Bspw. cartopy oder basemap). Hierbei müssen neben den Daten die
Projektion sowie Koordinaten definiert, oder als Argumente übergeben werden.
Cartopy kann hierbei intelligent die Koordinaten aus einem xarray Datensatz 
interpretieren.
Weitere Beispiele wie dies mit Basemap funktioniert: 
https://matplotlib.org/basemap/users/examples.html 
"""
# In eine Karte plotten
import cartopy.crs as ccrs
ax = plt.axes(projection=ccrs.Orthographic())
airtemps.air.isel(time=0).plot.contourf(ax=ax, transform=ccrs.PlateCarree());
ax.set_global(); ax.coastlines(); plt.show()

# Karte auf den richtigen Ausschnitt zentrieren
ax = plt.axes(projection=ccrs.Orthographic(-80, 35)) # Zentrum definieren 
airtemps.air.isel(time=0).plot.contourf(ax=ax, transform=ccrs.PlateCarree());
ax.set_global(); ax.coastlines(); plt.show()

# Ein anderes plotting Tool (pcolormesh vs. contourf)
ax = plt.axes(projection=ccrs.Orthographic(-80, 35))
airtemps.air.isel(time=0).plot.pcolormesh(ax=ax, transform=ccrs.PlateCarree());
ax.set_global(); ax.coastlines(); plt.show()

# Eine andere Projektion wählen
ax = plt.axes(projection=ccrs.LambertConformal(-80, 35))
(airtemps.air.isel(time=0)-airtemps.air.isel(time=1)).plot.pcolormesh(ax=ax, transform=ccrs.PlateCarree());
ax.coastlines(); plt.show()

#%%
##############################
# eigene Funktionen 
# Schleifen
##############################
"""
In Python können Sie eigene Funktionen entwickeln, die Sie anschließend genau
wie Funktionen über funktionsName(Argumente) ansprechen können.

In Schleifen lassen sich Funktionen oder Operationen wiederholt ausführen. Es
gibt verschiedene Arten von Schleifen (hier gezeigt: for-Schleife)
"""

def einsDazu(val):
    """
    Addiere 1 zu val
    """
    val += 1 # ist das gleiche wie val = val+1
    return val # return übergibt am Ende der Funktion das Ergebnis

einsDazu(2) # wir übergeben der Funktion den Wert 2

# Diese for Schleife iteriert über die definierten Schritte und führt die 
# Funktion über den pro Schritt definierten Wert (i, hier: 0,1,2...9) aus.
for i in range(10):
    print(einsDazu(i))

# Schleifen lassen sich verschachteln
# Versuchen sie, die entstehende Zahlenreihenfolge aufzuschreiben, bevor sie 
# die Funktion ausführen. Denken Sie daran, dass python bei 0 anfängt.
for i in range(5):
    for j in range(2):
        print(einsDazu(i)*j)

# Beispiel: in einer Schleife über Längen und Breitengrad die Korrelation eines
# Pixels mit den umliegenden Pixeln zu berechnen
for i in range(5):
    for j in range(5):
        print(stats.pearsonr(airtemps.air.sel(lon=260,lat=50),airtemps.air.sel(lon=260+i,lat=50+j,method='nearest')))


# Beispiel: in einer Schleife über Längen und Breitengrad die Korrelation eines
# Pixels mit den umliegenden Pixeln zu berechnen
# Zunächst erzeugen wir einen Array mit dem richtigen shape, in dass die 
# Ergebnisse abgespeichert werden. 

air_resampled = airtemps.air.resample(time='1m').mean(dim='time')
   
cor = np.zeros([airtemps.lat.size,airtemps.lon.size])
for i in range(airtemps.lat.size):
    for j in range(airtemps.lon.size):
        print(i,j)
        cor[i,j] = stats.pearsonr(air_resampled.sel(lon=260,lat=50),air_resampled.isel(lon=j,lat=i))[0]

plt.imshow(cor)
plt.colorbar(label='Pearson Korrelation')
plt.title('Korrelation jedes Pixels mit lon=260 und lat=50')
plt.show()

"""
Aufgaben:
    1) Wie hoch ist die Korrelation im Mittel? 
    2) Resamplen sie die Daten in eine andere zeitliche Auflösung
    3) Wie verändert sich die mittlere Korrelation beim Resampling?
    Zum Nachdenken: müssen alle Rechnungen innerhalb der Schleife stattfinden?     
"""
