# Loesung Uebungsblatt Nr. 2
# Autor: Lucas Schuhmacher
# Datum: 19.12.2022

## Aufgabe 1.1

# Library installieren
library(data.table)

# Datenimport
personen <- fread("//ifv-fs.ifv.kit.edu/Lehre/Lehrveranstaltungen/Empirische Daten im Verkehrswesen/WS22-23/Übung/MOP_Daten/P.csv", header=T)
wege <- fread("//ifv-fs.ifv.kit.edu/Lehre/Lehrveranstaltungen/Empirische Daten im Verkehrswesen/WS22-23/Übung/MOP_Daten/Wege.csv", header=T )

## Aufgabe 1.3

round(prop.table(table(wege$VMDIW)),2)   # VMDIW = hauptsaechlich benutztes Verkehrsmittel

# Lsg: die meisten Wege werden mit VM Nr. 4 = Pkw als Fahrer zurueckgelegt

## Aufgabe 1.4

# ungewichtet
mean(wege$KM)

# gewichtet
weighted.mean(wege$KM, wege$KMGEW)

# Differenz
diff_gewicht = mean(wege$KM) - weighted.mean(wege$KM, wege$KMGEW)

# Lsg: Differenz = 0,173 km, ungewichteter Wert etwas groesser

## Aufgabe 1.5

# Wege zur Arbeit
summary(wege[ZWECK==1]$KM)

# dienstliche Wege
summary(wege[ZWECK==2]$KM)

# Ausbildung
summary(wege[ZWECK==3]$KM)

# Einkaufswege
summary(wege[ZWECK==4]$KM)

# Freizeitwege
summary(wege[ZWECK==5]$KM)

# Servicewege
summary(wege[ZWECK==6]$KM)

# Heimwege
summary(wege[ZWECK==7]$KM)

# Lsg: Median immer kleiner als Mittelwert, sehr lange Wege eingentlich Ausreisser (siehe Differenz zwischen Q3 und Max)
