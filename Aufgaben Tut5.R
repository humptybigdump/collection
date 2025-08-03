library(ggplot2)
library(dplyr)

Analyse_starwars

# Aufgabe 1: Visualisiere die folgenden Angaben aus dem Analyse_starwars-Datensatz.
  # A) Verteilung der Geschlechter
    Analyse_starwars %>%
      ggplot(aes(x = sex)) +
      geom_bar()

  # B) Verteilung der Masse
    Analyse_starwars %>%
      ggplot(aes(x = mass)) +
      geom_histogram()
  
  # C) Verteilung der Größe (Boxplot)
    Analyse_starwars %>%
      ggplot(aes(y = height)) +
      geom_boxplot() +
      stat_boxplot(geom = "errorbar")

  # D) Den Zusammenhang zwischen Geburtsjahr und Masse
    Analyse_starwars %>%
      ggplot(aes(x = birth_year, y = mass)) +
      geom_point()

# Aufgabe 2: Verschönere deine Grafiken aus der ersten Aufgabe nach folgenden Angaben...
  # A) Hellblaue Balken
    Analyse_starwars %>%
      ggplot(aes(x = sex)) +
      geom_bar(fill = "lightblue")

  # B) Wertebereich = 10
    Analyse_starwars %>%
      ggplot(aes(x = mass)) +
      geom_histogram(binwidth = 10)

  # C) rot-umrandete Box und kleiner Fehlerbalken
    Analyse_starwars %>%
      ggplot(aes(y = height)) +
      geom_boxplot(color = "red") +
      stat_boxplot(geom = "errorbar", width = 0.1)
  
  # D) Beschriftungen
    Analyse_starwars %>%
      ggplot(aes(x = birth_year, y = mass)) +
      geom_point() +
      xlab("Geburtsjahr") +
      ylab("Masse") +
      ggtitle("Zusammenhang zwischen Geburtsjahr und Masse")
    