library(dplyr)
library(ggplot2)
library(readxl)
library(DescTools)

# Aufgabe 1: siehe PDF

# Aufgabe 2: Import Datensatz
Triathlon <- read_excel("C:/Users/tablu/Documents/Studium/Tutorium/Vorbereitung/09_Bivariate Analyse II/R/Triathlon 2022.xlsx")

# Aufgabe 3:
  # A) Gibt es einen Zusammenhang zwischen dem Geschlecht und der Lieblingsdisziplin?
    # -> zwei nominalskalierte Variablen: Cramers V
    CramerV(x = Triathlon$Geschlecht, y = Triathlon$Disziplin)
    # Cramers V = 0,17 -> Es gibt einen schwachen Zusammenhang zwischen dem Geschlecht und der Lieblingsdisziplin.
    
  # B) Gibt es einen Zusammenhang zwischen dem Alter und der Anzahl an Trainingsstunden pro Woche?
    # -> zwei metrisch skalierte Variablen: Pearsons r
    cor(Triathlon$Alter, Triathlon$Trainingsstunden, method = "pearson")
    # Pearsons r = -0,21 -> Es gibt einen mittelstarken negativen Zusammenhang zwischen dem Alter und der Anzahl an Trainingsstunden pro Woche.
    
  # C) Gibt es einen Zusammenhang zwischen der Gesamtplatzierung und dem Traubenzuckerkonsum vor dem Wettbewerb?
    # -> zwei ordinalskalierte Variablen: Kendall's tau / Spearman's rho
    cor(Triathlon$Traubenzucker, Triathlon$Platzierung_gesamt, method = "kendall")
    # Kendall's tau = -0,04 -> Es gibt keinen Zusammenhang zwischen dem Konsum von Traubenzucker und der Gesamtplatzierung.
    cor(Triathlon$Traubenzucker, Triathlon$Platzierung_gesamt, method = "spearman")
    # Spearman's rho = -0,05 -> Es gibt keinen Zusammenhang zwischen dem Konsum von Traubenzucker und der Gesamtplatzierung.
    
  # D) Gibt es einen Zusammenhang zwischen der Gesamtplatzierung und dem Einkommen durch Preisgelder?
    # -> eine ordinalskalierte Variable und eine metrische Variable -> metrische Variable downgraden: Kendall's tau oder Spearman's rho
    cor(Triathlon$Platzierung_gesamt, Triathlon$Einkommen, method = "kendall")
    # Kendall's tau = 0,04 -> Es gibt keinen Zusammenhang zwischen Gesamtplatzierung und Einkommen durch Preisgelder
    cor(Triathlon$Platzierung_gesamt, Triathlon$Einkommen, method = "spearman")
    # Spearman's rho = 0,06 -> Es gibt einen schwachen Zusammenhang zwischen Gesamtplatzierung und Einkommen durch Preisgelder.
    
  # E) Sind die gefundenen Zusammenhänge auch über die Stichprobe hinaus anzunehmen?
      # A)
      chisq.test(Triathlon$Geschlecht, Triathlon$Disziplin, correct = FALSE)
      # -> p-value > 0,05 -> kein signifikanter Zusammenhang
      # B)
      cor.test(Triathlon$Alter, Triathlon$Trainingsstunden, method = "pearson")
      # -> p-value < 0,05 -> es gibt einen signifikanten Zusammenhang
      
  # F) Visualisiere den festgestellten signigikanten Zusammenhang
      Triathlon %>%
        ggplot(aes(x = Alter, y = Trainingsstunden)) +
        geom_point() +
        geom_smooth(method = lm, se = FALSE) +
        ggtitle("Zusammenhang zwischen Alter und Trainingsstunden pro Woche")
      