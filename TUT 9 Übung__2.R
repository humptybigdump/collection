#Multiple lineare Regression - Tutorium

#a) Datensatz laden
library(haven)
setwd("C:/Users/langm/StatistikMat")
Roh_GueW <- read_sav("Roh_GueW.sav")

#b) multiple lineare Regression durchführen
mlReg <- lm(Interesse ~ Alter + HwG_1, data = Roh_GueW)
summary(mlReg)

#c) prüfen der Voraussetzungen
#Linearität der Variablen
#partielles Regressionsdiagramm für jede unabhängige Variable
#nur vollständige Datensätze auswählen, sonst entstehen Probleme bei den Regressionen
library(dplyr)
GueW1 <- Roh_GueW %>% filter(is.na(Alter)|is.na(HwG_1)|is.na(Interesse))

#Für Alter:
A1 <- lm(Alter ~ HwG_1, data = GueW1)
A2 <- lm(Interesse ~ HwG_1, data = GueW1)

plot(A1$residuals ~ A2$residuals)
abline(lm(A1$residuals ~ A2$residuals))
summary(lm(A1$residuals ~ A2$residuals))
#sieht nicht unbedingt nach Linearität aus, auch p-Wert nicht signifikant

#Für HwG_1:
B1 <- lm(HwG_1 ~ Alter, data = GueW1)
B2 <- lm(Interesse ~ Alter, data = GueW1)

plot(B1$residuals ~ B2$residuals)
abline(lm(B1$residuals ~ B2$residuals))
summary(lm(B1$residuals ~ B2$residuals))
#sieht linear aus, p-Wert signifikant --> Linearität vorhanden

#keine Ausreißer
summary(Roh_GueW$Alter)
summary(as.factor(Roh_GueW$HwG_1))
summary(Roh_GueW$Interesse)
#keine Ausreißer im Datensatz vorhanden

#keine hohe Multikollinearität z.B. mit VIF
library(car)
vif(mlReg)
#Wert muss (deutlich) kleiner sein als 10

#Varianzhomogenität der Residuen mittels Streudiagramme aus Vorhersagewerten und Residuen
plot(mlReg$residuals ~ mlReg$fitted.values)
#eine Art Trichter ist erkennbar --> niedrige Werte haben hohe Varianz, hohe Werte niedrige Varianz
#--> keine Varianzhomogenität

#Normalverteilung der Residuen
shapiro.test(mlReg$residuals)
#Ergebnis soll nicht signifikant sein

#Unkorreliertheit der Residuen mit Durbin-Watson Test
dwt(mlReg)
#Ziel: Wert von 2 oder sehr nahe 2

#Zusammengefasst: keine Ausreißer, keine hohe Multikollinearität, normalverteilte Residuen
#unkorrelierte Residuen ABER Einfluss von Alter nicht klar linear und Varianzhomogenität fehlt

#d) erklärte Varianz: adjusted r-squared auslesen
summary(mlReg)
#Wert von 0.3049 bedeutet ca. 30% erklärte Varianz -> das ist kein schlechter Wert

#e) Regressionsgleichung aufstellen
# y-Dach = 37.92 - 0.15 * Alter + 12.09 * (Wert von HwG_1)

#f) Vorhersage mit Alter = 35 und HwG_1=4
37.92 - 0.15 * 35 + 12.09 * 4



#Apfeilbeispiel
#Grundwert 1000 Äpfel
#Prozentwert 800 Äpfel  
#Prozentsatz 80%
# 900 Äpfel  90% -> UNterschied von 10%-Punkte
