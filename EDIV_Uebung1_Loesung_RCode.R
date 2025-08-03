#einmalig (Installation Packages)
install.packages("data.table")
install.packages("xlsx")

rm(list = ls())

#immer, wenn R neu gestartet wird
library(data.table)
library(xlsx)


#### Aufgabe 1 ####

#1.1
#Daten einlesen (hier den entprechenden Speicherort anpassen)
personen <- fread("//ifv-fs.ifv.kit.edu/Lehre/Lehrveranstaltungen/Empirische Daten im Verkehrswesen/WS24-25/Übung/MOP/MOP-Daten/P.csv")

#1.2
#neue Variable in neuer Spalte
personen[, alter := JAHR-GEBJAHR]

#1.3
#Daten löschen mit Subset & Spalten-Name
ohne_info <-subset(personen, select = -c(KRANKmo:ANORM2so))

#Daten entfernen mittels Spalten-Nummer
ohne_info <- personen[, -c(27:61)]

#1.4
#Speichern des neuen Datensatzes (hier den entprechenden Speicherort anpassen)
save(ohne_info, file = "//ifv-fs.ifv.kit.edu/Lehre/Lehrveranstaltungen/Empirische Daten im Verkehrswesen/WS24-25/Übung/data_erg.RData")
fwrite(ohne_info, "//ifv-fs.ifv.kit.edu/Lehre/Lehrveranstaltungen/Empirische Daten im Verkehrswesen/WS24-25/Übung/ohne_info.csv")

#### Aufgabe 2 ####

#2.1
#beide Kennzahlen auf einmal mittels summary()
summary(personen$alter)

#beide Kennzahlen separat
Diff = mean(personen$alter) - median(personen$alter)

#2.2
#absolute Haeufigkeit 
table(personen$SEX)

#Verteilung des Geschlechts
prop.table(table(personen$SEX))

#2.3
#Geschlecht x Fahrrad absolut
table(personen$SEX, personen$FAHRRAD)


#relative Anteile Geschlecht x Fahrrad 
prop.table(table(personen$SEX, personen$FAHRRAD))




