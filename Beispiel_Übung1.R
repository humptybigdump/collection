#############Einfuehrung in data.table#############

# Uebung Nr. 1, 21.12.2020
# Autor: Nadine Kostorz, IFV


## Clear memory
rm(list = ls())

# einmalig
install.packages("data.table")

# jedes Mal -> Funktionsbibliothek laden
library(data.table)


#Daten als data.table einlesen (Diesen Pfad muessen sie durch den Pfad fuer die Datei auf ihrem Lokalen Rechner ersetzten und dabei beachten "\" mit "/" zu ersetzen
Daten = fread("//..../Uebung1/Matrial_zum_Ausgeben/Daten_Befragung_Studierende.csv")

#
Daten[,c("angestrebter_abschluss_sonstiges"):=NULL ]

# Daten Bau_Geo
Studis_BGU = Daten[fakultaet_kit ==7]

#Ueberblick Anzahl Fachsemester
summary(Studis_BGU$anzahl_fachsemester)

# Haeufigkeitstabelle anzahl_fachsemester im Datensatz BGU
table(Studis_BGU$anzahl_fachsemester)

# Haeufigkeitstabelle anzahl_fachsemester im Datensatz mit allen Studenten zum Vergleich
table(Daten$anzahl_fachsemester)

# Korrektur der Werte -99 und -66 zu NA (not available) 
Daten[,anzahl_fachsemester:= ifelse(anzahl_fachsemester==-99 , NA, anzahl_fachsemester)]
Daten[,anzahl_fachsemester:= ifelse(anzahl_fachsemester==-66 , NA, anzahl_fachsemester)]

# Ueberblick Anzahl Fachsemeester im Datensatz Daten mit allen 
summary(Daten$anzahl_fachsemester)

#neue Variable anlegen im Datensatz BGU mit Wert 1
Studis_BGU[,test := 1]


