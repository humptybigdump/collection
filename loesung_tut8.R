if(rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

# Packages laden
library(tidyverse)
library(DescTools)

# Datensatz einlesen
em_data <- read_csv("em_fernsehminuten_datensatz.csv")

# Tabellenanalyse
  # geeignete Variablen = nominalskaliert: Fan & Lieblingsnation
## Kreuztabelle
kreuztabelle_fan_lieblingsnation <- table(em_data$lieblingsnation, em_data$fan)
kreuztabelle_fan_lieblingsnation
  # Beispielinterpretation: 15 Befragte sind keine Fußballfans und haben die Lieblingsnation 1.
## Spaltenprozente
prop.table(kreuztabelle_fan_lieblingsnation, 2)
  # Beispielinterpretation: 4 % der Fußballfans haben die Lieblingsnation 1.
## Zeilenprozente
prop.table(kreuztabelle_fan_lieblingsnation, 1)
  # Beispielinterpretation: 58 % der Fans von Lieblingsnation 1 sind keine Fußballfans.
## Differenz der Spaltenprozente (Beispiel)
7.09 - 3.25
  # = 3,84 < 5 %: Fans haben nicht häufiger die Lieblingsnation 2 als Nicht-Fans.
## Differenz der Zeilenprozente (Beispiel)
57.69 - 30.77
  # = 26,92 > 10 %: Fans der Nation 1 sind häufiger keine Fußballfans als Fans der Nation 2.

# Chi2-Test
  # H0: Es gibt keinen Zusammenhang zwischen der Lieblingsnation und Fußballfans/Nicht-Fans.
  # H1: Es gibt einen Zusammenhang zwischen der Lieblingsnation und Fußballfans/Nicht-Fans.
  # Signifikanzniveau: alpha = 0,05
  # Vorannahmen
    # unabhängige Beobachtungen? Ja
    # alle erwarteten Werte >= 5?
    chi2_fan_lieblingsnation$expected
      # alle Werte > 5
## Chi2
chi2_fan_lieblingsnation <- chisq.test(em_data$fan, em_data$lieblingsnation, correct = TRUE)
chi2_fan_lieblingsnation
  # p-value = 0.7065 > 0.05 -> H0 wird beibehalten: Es gibt keinen signifikanten Zusammenhang

# Cramérs V (eigentlich nicht berechnet, da kein Zusammenhang festgestellt werden konnte)
CramerV(x = em_data$lieblingsnation, y = em_data$fan)
  # 0,19 -> schwacher Zusammenhang zwischen Lieblingsnation und Fan (in Stichprobe)

# Für Schnelle:
set.seed(1)
fanartikel <- sample(c(1:5), size = 500, replace = TRUE)
em_data <- data.frame(em_data, fanartikel)
## Chi2-Test mit Fan-Variable
  # H0: Es gibt keinen Zusammenhang zwischen dem Fanstatus und mitgebrachten Fanartikeln.
  # H1: Es gibt einen Zusammenhang zwischen dem Fanstatus und mitgebrachten Fanartikeln.
  # Signifikanzniveau: alpha = 0,05
  # Voraussetzungen:
    # unabhängige Beobachtungen? Ja.
    # alle Indifferenzwerte >= 5?
chi2_fan_fanartikel$expected
    # alle Werte > 5
  # Chi2-Test
chi2_fan_fanartikel <- chisq.test(em_data$fan, em_data$fanartikel)
chi2_fan_fanartikel
  # p-value = 0,4258 -> H0 beibehalten: Es gibt keinen Zusammenhang.
## Chi2-Test mit Lieblingsnation-Variable
  # H0: Es gibt keinen Zusammenhang zwischen der Lieblinsnation und mitgebrachten Fanartikeln.
  # H1: Es gibt einen Zusammenhang zwischen der Lieblingsnation und mitgebrachten Fanartikeln.
  # Signifikanzniveau: alpha = 0,05
  # Voraussetzungen:
    # unabhängige Beobachtungen? Ja.
    # alle Indifferenzwerte >= 5?
chi2_lieblingsnation_fanartikel$expected
    # Werte < 5 -> im Anschluss Fishers Test durchführen!
# Chi2-Test
chi2_lieblingsnation_fanartikel <- chisq.test(em_data$lieblingsnation, em_data$fanartikel)
chi2_lieblingsnation_fanartikel
  # p-value = 0,4477 > 0,05 -> H0 beibehalten: Es gibt keinen Zusammenhang.
# Fishers Test
fisher.test(em_data$lieblingsnation, em_data$fanartikel)