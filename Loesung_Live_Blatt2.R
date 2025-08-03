# Uebung Nr. 2
# Auswertungen mit MOP-Daten

### Clear memory
rm(list = ls())

install.packages("data.table")
#packages / Funktionsbibliothek laden
library(data.table)

#1.1
#Daten als data.table einlesen
### ACHTUNG SIE MUESSEN DEN PFAD DURCH DEN SPEICHERORT AUF IHREM PC ERSETZEN ###

personen <- fread("//ifv-fs.ifv.kit.edu/Lehre/Lehrveranstaltungen/Empirische Daten im Verkehrswesen/WS20-21/Übung/Uebung2/Daten/P.csv")

wege<- fread("//ifv-fs.ifv.kit.edu/Lehre/Lehrveranstaltungen/Empirische Daten im Verkehrswesen/WS20-21/Übung/Uebung2/Daten/Wege.csv")

#1.3
#Modal Split

modalsplit = table(wege$VMDIW)

#Die meisten Wege werden mit VM 4 = MIV-Fahrer zurückgelegt.

#1.4
#Durchschnittliche Wegelaenge

#ungewichtet
mean(wege$KM)

#gewichtet
weighted.mean(wege$KM, wege$KMGEW)

#1.5
#Wegelaenge nach Zweck, MW, oberes und unteres Quartil
# Arbeitswege

summary(wege[ZWECK ==1]$KM)
