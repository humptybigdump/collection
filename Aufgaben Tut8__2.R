library(tidyverse)
library(sjmisc)
library(readr)
library(DescTools)

# Datensatz importieren
chartsger <- read_csv2("Dateipfad/chartsger.CSV", trim_ws = TRUE, na = "-77")
Analyse_chartsger <- chartsger

# Top10-Datensatz
id <- c(1:10)
gender <- c(1, 1, 1, 1, 1, 1, 1, 2, 1, 1)
haircol <- c(1, 5, 3, 3, 3, 2, 3, 1, 2, 1)
top10 <- data.frame(id, gender, haircol)

# Aufgabe 1: Kreuztabelle
  kreuztabelle <- table(top10$haircol, top10$gender)
  kreuztabelle
  # Spaltenprozente
  prop.table(kreuztabelle, 2)
    # Vergleich:
      # blond: 78% Differenz -> Frauen sind häufiger blond.
      # braun: 22% Differenz -> Männer sind häufiger braunhaarig.
      # schwarz: 44% Differenz -> Männer sind häufiger braunhaarig.
      # grau: 11% Differenz -> Männer sind häufiger grauhaarig.
  # Zeilenprozente
  prop.table(kreuztabelle, 1)
  # Chi2
  chisq.test(top10$haircol, top10$gender)
    # Chi2 = 2.59
  
# Aufgabe 2
  # A) Chi2-Test Geschlecht und Haarfarbe
    # H0: Es gibt keinen Zusammenhang zwischen Geschlecht und Haarfarbe.
    # H1: Es gibt einen Zusammenhang zwischen Geschlecht und Haarfarbe.
    # alpha = 0.05
    # Vorannahmen:
      # unabhängige Beobachtungen -> teilweise
      # erwartete Werte > 5 -> viele Werte unter 5
      chartsger_kreuztabelle <- table(Analyse_chartsger$haircol, Analyse_chartsger$gender)
      chartsger_kreuztabelle
      margin.table(chartsger_kreuztabelle, 2)
    chisq.test(Analyse_chartsger$gender, Analyse_chartsger$haircol)
      # p = 0.1245 > 0.05 -> H0 beibehalten und von keinem Zusammenhang ausgehen.
    # Kontrolle: Fisher's Test, da viele Werte < 5
    fisher.test(Analyse_chartsger$gender, Analyse_chartsger$haircol)
      # p = 0.01432 < 0.05 -> H0 ablehnen und von einem Zusammenhang ausgehen.
  # B) Cramers V Top100 vs. Top10
    # Cramers V Top100
    CramerV(x = Analyse_chartsger$gender, y = Analyse_chartsger$haircol)
      # 0.30 -> mittelstarker Zusammenhang
    # Cramers V Top10
    CramerV(x = top10$gender, y = top10$haircol) 
      # 0.51 -> (mittel)starker Zusammenhang 