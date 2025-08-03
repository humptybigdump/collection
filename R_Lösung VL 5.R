#Lösung zu Aufgaben der 5. VL

library(gmodels)
#Daten einlesen
library(haven)
Roh_GueW <- read_sav("C:/Users/user/Desktop/Roh_GueW.sav")
View(Roh_GueW)
#Datensatz umbenennen
data <- Roh_GueW

#Variablen separieren HwG_1 und NfC_1
hwg1 <- c(data$HwG_1)
nfc1 <- c(data$NfC_1)

#Kreuztabellen anschauen
CrossTable(nfc1, hwg1, chisq = TRUE)

#Median für nfc1 und hwg1 bestimmen.
median(hwg1, na.rm = TRUE)
median(nfc1)

#Mediansplit durchführen, so dass Werte unterhalb und oberhalb des Medians identifiziert werden.
nfc1_high <- nfc1 > 3
hwg1_high <- hwg1 > 3

#Neue Kreuztabelle erstellen
CrossTable(nfc1_high, hwg1_high, chisq = TRUE)

