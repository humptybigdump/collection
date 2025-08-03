#TUT 4 Übungen und Co.

install.packages("sjmisc")
install.packages(("DescTools"))
install.packages("haven")

#benötigte packages laden
library("sjmisc")
library("dplyr")
library("DescTools")
library("haven")

#Datensatz laden
load("C:/Users/langm/StatistikMat/GueW_Auswahl.RData")

#Aufgabe 2: CramersV
CramerV(x = GueW$NfC_4, y = GueW$NfC_1)








#alternative Koeffizienten für ordinale Skalen
GoodmanKruskalGamma(x = GueW$NfC_4, y = GueW$NfC_1)
KendallTauA(x = GueW$NfC_4, y = GueW$NfC_1)
KendallTauB(x = GueW$NfC_4, y = GueW$NfC_1)
SpearmanRho(x = GueW$NfC_4, y = GueW$NfC_1)
SomersDelta(x = GueW$NfC_4, y = GueW$NfC_1)

#Roh-Version der GueW-Befragung laden
Roh_GueW <- read_sav("C:/Users/langm/StatistikMat/Roh_GueW.sav")


#Aufgabe 3: Plot, Korrelation und Regression
plot(Roh_GueW$Alter, Roh_GueW$Interesse)

cor(x = Roh_GueW$Alter, y = Roh_GueW$Interesse, use = "complete")

lm(Roh_GueW$Interesse ~ Roh_GueW$Alter, data = Roh_GueW)



