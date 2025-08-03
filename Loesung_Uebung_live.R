# Uebung Nr 3 - Emp. Daten im Verkehrswesen
#
# 08.02.2021

### Clear memory
rm(list = ls())


# einmalig
# install.packages("ggplot2")

library(data.table)
library(ggplot2)

## Einfuehrung GGPLOT2 ##


personen <- fread("//ifv-fs.ifv.kit.edu/Lehre/Lehrveranstaltungen/Empirische Daten im Verkehrswesen/WS20-21/Übung/Uebung2/Daten/P.csv")

wege<- fread("//ifv-fs.ifv.kit.edu/Lehre/Lehrveranstaltungen/Empirische Daten im Verkehrswesen/WS20-21/Übung/Uebung2/Daten/Wege.csv")





#### Beispiel durch. Geschwindigkeit nach Verkehrsmittel ###

#Berechnung der durchn. Geschwindigkeit pro Verkehrsmittel 
mean_speed_by_VMDIW = aggregate(SPEED~VMDIW, wege, mean)

# Plot
ggplot(mean_speed_by_VMDIW, aes(VMDIW, SPEED)) + geom_point()

# Benennung der Verkehrsmittel (Reihenfolge entspricht der Nummerierung im Codeplan!)
VMDIW_NAME<- c("Fuss", "Rad", " Mofa etc.", "PKW Fahrer", "PKW Mitfahrer", "Bus", "StraBa", "U/SBahn", "Zug","sonstige", "Flugzeug")

#cbind =  Hinzufügen einer Spalte -> siehe Cheat Sheet 
cbind(mean_speed_by_VMDIW, VMDIW_NAME)

# Erneuter Plot, diesmal mit Beschriftung der Verkehrsmittelnahmen 
ggplot(mean_speed_by_VMDIW, aes(VMDIW_NAME, SPEED)) + geom_point(size = 5, colour="blue")




########## UEB 3 #############

# Aufgabe 2.2
# Modal Split nach Geschlecht 

# 1 = m 2 = w

InfoGeschlecht = personen[, c("id", "PersNr", "Sex")]

# Merge

WegeinklGeschlecht = merge(wege, InfoGeschlecht, by.x= c("ID","PERSNR"), by.y= c("id","PersNr"), all.x=TRUE)
#Mofawege
round(prop.table(table(WegeinklGeschlecht[VMDIW==3]$Sex)), 2)

#Fusswege 
round(prop.table(table(WegeinklGeschlecht[Sex==2]$VMDIW)),2)


# Aufgabe 2.3
# Geschwindigkeit auf Fusswegen

install.packages("ggplot2")
library(ggplot2)

ggplot(wege[VMDIW==1], aes(KM, SPEED)) + geom_point(colour="red", size=2)

# Zusatzaufgabe 1
# Aufgabe entspricht der Einfuehrung, siehe oben fuer die Loesung

# Zusatzaufgabe 2
# Ein Histogram visualisiert die Haeufigkeitsverteilung, standardmaessig bezieht sich dies nur auf eine Variable 
# Die Funktion fuer Histogramme ist geom_histogram(). binwidht gibt den Bereich pro Klasse an, hier verwenden wir 1, da jede Zahl ein anderes VM ist.
# 

ggplot(wege, aes(VMDIW))+geom_histogram(binwidth = 1)


# Wir sehen sehr eindeutig, dass die meisten Wege mit dem Pkw zurueckgelegt werden