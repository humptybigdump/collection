if(rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

# Packages laden
library(tidyverse)
library(DescTools)

# Datensatz einlesen
em_data <- read_csv("em_fernsehminuten_datensatz.csv")

# Fernsehzeit Befragte vs. durchschnittliche deutsche Fernsehzeit -> t-Test mit einem theoretischen Wert (eine Stichprobe)
  # H0: Fernsehzeit Befragte = 5460 Minuten
  # H1: Fernsehzeit Befragte != 5460 Minuten
  # Signifikanzniveau: alpha = 0.05
## Vorannahmen
  # metrische Variable? Ja
  # Normalverteilung?
shapiro.test(em_data$fernseh_min_monat)
  # p-value = 0.3886 > 0.05 -> Normalverteilung liegt vor
## t-Test für eine Stichprobe
t.test(em_data$fernseh_min_monat, mu = 5460, alternative = "two.sided")
  # p-value < 2.2e-16 < 0.05 -> H0 wird abgelehnt, der Mittelwert der Finalbesucher:innen entspricht nicht dem deutschen Durchschnitt der Fernsehnutzung.

# EM-Minuten der Fußballfans vs. EM-Minuten der Nicht-Fans -> t-Test für unabhängige Stichproben
  # H0: EM-Minuten Fußballfans <= EM-Minuten Nicht-Fans
  # H1: EM-Minuten Fußballfans > EM-Minuten Nicht-Fans -> eigentlich rechtsseitiger Hypothesentest, aber durch Gruppencodierung als 0 = Nicht-Fan und 1 = Fan umdrehen
  # Signifikanzniveau: alpha = 0.05
## Vorannahmen
  # metrische Variable? Ja
  # Normalverteilung?
shapiro.test(em_data$em_min)
  # p-value = 0.2873 > 0.05 -> Normalverteilung liegt vor
  # Varianzhomogenität?
LeveneTest(em_data$em_min, em_data$fan)
  # p-value = 0.5271 > 0.05 -> Varianzhomogenität liegt vor
## t-Test für unabhängige Stichproben
t.test(em_data$em_min ~ em_data$fan, alternative = "less")
  # p-value = 0.04287 < 0.05 -> H0 wird abgelehnt, Fußballfans schauen mehr EM-Minuten als Nicht-Fans.

# Fernsehnutzung x EM-Minuten -> t-Test für abhängige Stichproben
  # H0: Fersehminuten <= EM-Minuten
  # H1: Fersehminuten > EM-Minuten
  # Signifikanzniveau: alpha = 0.05
## Vorannahmen
  # metrische Variablen? Ja
  # Normalverteilung?
shapiro.test(em_data$fernseh_min_monat)
  # p-value = 0.3886 > 0.05 -> Normalverteilung liegt vor
shapiro.test(em_data$em_min)
  # p-value = 0.2873 > 0.05 -> Normalverteilung liegt vor
  # Varianzhomogenität?
LeveneTest(em_data$fernseh_min_monat, em_data$em_min)
  # p-value = NaN -> unklar... (eigentlich keine Durchführung)
## t-Test für abhängige Stichproben
t.test(em_data$fernseh_min_monat, em_data$em_min, alternative = "greater", paired = TRUE)
  # p-value < 2.2e-16 < 0.05 -> H0 wird abgelehnt. Höhere monatliche Fernsehnutzung mit mehr geschauten EM-Spielen einher.

# Bonus: Unterschied Fans/Nicht-Fans
em_data_fans <- em_data %>% 
  filter(fan == 1)
em_data_nichtfans <- em_data %>% 
  filter(fan == 0)
## Fans
  # H0 & H1 wie in der Aufgabe zuvor
  # Signifikanzniveau: alpha = 0.05
  # Vorannahmen
    # metrisch? Ja
    # Normalverteilung?
shapiro.test(em_data_fans$fernseh_min_monat)
      # p-value = 0.2519 -> Normalverteilung liegt vor
shapiro.test(em_data_fans$em_min)
      # p-value = 0.1258 -> Normalverteilung liegt vor
    # Varianzhomogenität?
LeveneTest(em_data_fans$fernseh_min_monat, em_data_fans$em_min)
      # p-value nicht berechenbar
  # t-Test
t.test(em_data_fans$fernseh_min_monat, em_data_fans$em_min, alternative = "greater", paired = TRUE)
    # p-value < 2.2e-16 < 0.05 -> H0 wird abgelehnt
## Nicht-Fans
  # H0 & H1 wie in der Aufgabe zuvor
  # Signifikanzniveau: alpha = 0.05
  # Vorannahmen
    # metrisch? Ja
    # Normalverteilung?
shapiro.test(em_data_nichtfans$fernseh_min_monat)
      # p-value = 0.5515 -> Normalverteilung liegt vor
shapiro.test(em_data_nichtfans$em_min)
      # p-value = 0.1195 -> Normalverteilung liegt vor
    # Varianzhomogenität?
LeveneTest(em_data_nichtfans$fernseh_min_monat, em_data_nichtfans$em_min)
      # p-value nicht berechenbar
  # t-Test
t.test(em_data_nichtfans$fernseh_min_monat, em_data_nichtfans$em_min, alternative = "greater", paired = TRUE)
    # p-value < 2.2e-16 -> H0 wird abgelehnt