library(dplyr)
library(sjmisc)
library(DescTools)
library(ggplot2)
library(car)
library(readxl)

# a) Import Datensatz
Triathlon <- read_excel("C:/Users/tablu/Documents/Studium/Tutorium/Vorbereitung/09_Bivariate Analyse II/R/Triathlon 2022.xlsx")

# b) Multiple Regression
  Regression_Einkommen_2Vars <- lm(Einkommen ~ Alter + Trainingsstunden, data = Triathlon)
  Regression_Einkommen_2Vars
  # Intercept: Ein*e Sportler*in, die 0 Jahre alt ist und 0 Stunden/Woche trainiert, hat ein Einkommen von 6.333,18 Euro.
  # Alter: Erhöht sich das Alter um 1 Jahr, so verringert sich das Einkommen um 5,37 Euro.
  # Trainingsstunden: Erhöhen sich die Trainingsstunden um 1 Stunde/Woche, verringert sich das Einkommen um 70,66 Euro.

# c) Standardisierte Regressionskoeffizienten
  library(QuantPsyc)
  lm.beta(Regression_Einkommen_2Vars)
  # oder
  Triathlon <- Triathlon %>%
    mutate(Einkommen_standardisiert = scale(Einkommen), Alter_standardisiert = scale(Alter), Trainingsstunden_standardisiert = scale(Trainingsstunden))
  Regression_Einkommen_2Vars_standardisiert <- lm(Einkommen_standardisiert ~ Alter_standardisiert + Trainingsstunden_standardisiert, data = Triathlon)
  Regression_Einkommen_2Vars_standardisiert
  # Alter = -0,008; Trainingsstunden = -0,226 -> Trainingsstunden haben einen größeren Einfluss.

# d) Detailoutput
  summary(Regression_Einkommen_2Vars)
  # F-Test: p-value > 0,05 -> keine Erklärungsleistung für Grundgesamtheit
  # t-Tests: p-value Trainingsstunden < 0,05 -> Trainingsstunden haben einen signifikanten Einfluss auf das Einkommen.
  # r2 = 0,05 -> Das Modell erklärt 5 % der Varianz im Einkommen
  # Adj. r2 = 0,03 -> keine allzugroße Differenz
  
# Bonus I: Voraussetzungen prüfen
  # Ausreißer
  scale(residuals(Regression_Einkommen_2Vars))
    # keine Abweichungen > 3
  # einflussreiche Fälle
  hatvalues(Regression_Einkommen_2Vars)
    # keine Werte deutlich über dem Durchschnitt
  # Multikollinearität
  vif(Regression_Einkommen_2Vars)
    # VIF < 10 -> unproblematisch
  # Homoskedastizität
  Triathlon %>%
    ggplot(aes(x = Einkommen, y = scale(residuals(Regression_Einkommen_2Vars)))) +
    geom_point()
    # Voraussetzung nicht erfüllt
  # Normalverteilung Residuen
  Triathlon %>%
    ggplot(aes(x = scale(residuals(Regression_Einkommen_2Vars)))) +
    geom_histogram()
  # oder:
  shapiro.test(residuals(Regression_Einkommen_2Vars))
    # p-value < 0,05 -> keine Normalverteilung -> nicht erfüllt
  # Autokorrelation
  durbinWatsonTest(Regression_Einkommen_2Vars)
    # D-W Statistic nahe 2 -> erfüllt
  
# Bonus II: Vergleich zweier Modelle
  Regression_Einkommen_3Vars <- lm(Einkommen ~ Alter + Trainingsstunden + Traubenzucker, data = Triathlon)
  summary(Regression_Einkommen_3Vars)
  summary(Regression_Einkommen_2Vars)
  anova(Regression_Einkommen_2Vars, Regression_Einkommen_3Vars)
    # p-value > 0,05 -> keine verbesserte Erklärungsleistung