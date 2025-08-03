#einmalig (Installation Packages)
install.packages("data.table")
install.packages("xlsx")

rm(list = ls())

#immer, wenn R neu gestartet wird
library(data.table)
library(xlsx)


#### Aufgabe 1 ####

#1.1
#Daten einlesen
personen <- fread("//ifv-fs.ifv.kit.edu/Lehre/Lehrveranstaltungen/Empirische Daten im Verkehrswesen/WS21-22/Übung/MOP/P.csv")

#1.2
#neue Variable in neuer Spalte
personen[, alter := 2021-GebJahr]

#1.3
#Daten löschen mit Subset & Spalten-Name
ohne_info <-subset(personen, select = -c(UrlaubMo:anorm2So))

# Daten entfernen mittels Spalten-Nummer
ohne_info <- personen[, -c(42:64)]

#1.4
#Speichern des neuen Datensatzes
save(ohne_info, file = "//ifv-fs.ifv.kit.edu/Lehre/Lehrveranstaltungen/Empirische Daten im Verkehrswesen/WS21-22/Übung/Übung 1/data_erg_Ue2.RData")

#### Aufgabe 2 ####

#2.1
# beide Kennzahlen auf einmal mittels summary()
summary(personen$alter)

#beide Kennzahlen separat
Diff = mean(personen$alter) - median(personen$alter)

#2.2
#absolute Haeufigkeit 
table(personen$Sex)

# Verteilung des Geschlechts
prop.table(table(personen$Sex))

#2.3
#  Geschlecht x Fahrrad absolut
table(personen$Sex, personen$Fahrrad)


# relative Anteile Geschlecht x Fahrrad 
prop.table(table(personen$Sex, personen$Fahrrad))




