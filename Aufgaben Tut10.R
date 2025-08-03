library(dplyr)
library(ggplot2)
library(readxl)
library(DescTools)

# a) Import Datensatz
Triathlon <- read_excel("C:/Users/tablu/Documents/Studium/Tutorium/Vorbereitung/09_Bivariate Analyse II/R/Triathlon 2022.xlsx")

# b) Lineare Regression Alter und Trainingsstunden / Preisgeld
  RegressionAlterTrainingsstunden <- lm(Trainingsstunden ~ Alter, data = Triathlon)
  RegressionAlterTrainingsstunden  
  # -> b0 = 27.73, d.h. 0-Jährige trainieren 27,73h pro Wochen.
  # -> b1 = -0.44, d.h. Steigt das Alter um ein Jahr, so verringert sich die Anzahl der Trainingsstunden pro Woche um 0,44h.
  Triathlon %>%
    ggplot(aes(x = Alter, y = Trainingsstunden)) +
    geom_point() +
    geom_smooth(method = lm)
  
  RegressionAlterEinkommen <- lm(Einkommen ~ Alter, data = Triathlon)
  RegressionAlterEinkommen
  # -> b0 = 4374.07, d.h. 0-Jährige haben ein Einkommen von 4374,07 Euro durch Preisgelder.
  # -> b1 = 26.03, d.h. Steigt das Alter um ein Jahr, so erhöht sich das Einkommen durch Preisgelder um 26,03 Euro.
  Triathlon %>%
    ggplot(aes(x = Alter, y = Einkommen)) +
    geom_point() +
    geom_smooth(method = lm)
  
# C) Regressionsgleichung
  # Alter und Trainingsstungen: yhat = 27.73 - 0.44 * x
    27.73-0.44 * 39 # -> yhat(x = 39) = 10.57 h
  # Alter und Einkommen: yhat = 4374.07 + 26.03 * x
    4374.07 + 26.03 * 39 # -> yhat(x = 39) = 5389.24 Euro
    
# D) Modellgüte
  summary(RegressionAlterTrainingsstunden)
  # -> r2 = 0.045, d.h. die Variable Alter erklärt 4,5 % der Varianz der Trainingsstunden pro Woche.
  summary(RegressionAlterEinkommen)
  # -> r2 = 0.002, d.h. die Variable Alter erklärt 0,2 % der Varianz des Einkommens durch Preisgelder.
  
# Bonus: Lineare Regression Geschlecht und Preisgeld
  # Dummy-Variable Geschlecht
  library(car)
  Triathlon$GeschlechtD <- recode(Triathlon$Geschlecht, "'m'=0; 'w'=1")
  RegressionGeschlechtEinkommen <- lm(Einkommen ~ GeschlechtD, data = Triathlon)
  summary(RegressionGeschlechtEinkommen)
  # b0 = 5116.8, d.h. Männer haben ein Einkommen von 5116,80 Euro durch Preisgelder.
  # b1 = -369.9, d.h. Frauen haben ein Einkommen, das um 369,90 Euro geringer ist als das von Männern.
  # r2 = 0.003, d.h. die Variable Geschlecht erklärt 0,3 % der Varianz des Einkommens durch Preisgelder.
  
  Triathlon %>%
    ggplot(aes(x = GeschlechtD, y = Einkommen)) +
    geom_point() +
    geom_smooth(method = lm) +
    xlab("Geschlecht")
  