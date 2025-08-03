#Tutorium zu Mittelwertvergleichen und t.tests
library("dplyr")
library("haven")
library("car")
setwd("C:/Users/langm/StatistikMat")
Analyse <- read_sav("Analyse_.sav")

remove(Analyse)


summary(Roh_GueW$Alter)


#a) Annahmen prüfen
#Normalverteilung
#Aufteilen in zwei Gruppen
Weiblich <- Analyse %>% filter (SD05 ==1)
Männlich <- Analyse %>% filter (SD05 ==2)

#Shapiro-Tests
shapiro.test(Weiblich$Prokrast_Index)
shapiro.test(Männlich$Prokrast_Index)

#Varianzhomogenität
leveneTest(Analyse$Prokrast_Index, Analyse$SD05)

#metrische Variablen: Datentabelle öffnen --> Nachschauen (optimal wäre ein Codebuch)

#b) Nullhypothese aufstellen
#HO: 

#c) Durchführung t-Test
t.test(Analyse$Prokrast_Index, Analyse$Trait_Index)
