library(ggplot2)
library(dplyr)
library(tidyr)

# Aufgabe 1: Visualisiere die folgenden Angaben aus dem Analyse_chartsger-Datensatz.
  # A) Verteilung der Geschlechter (Balkendiagramm)
    Analyse_chartsger %>%
      ggplot(aes(x = gender)) +
      geom_bar()

  # B) Verteilung der Spotify-Streams
    Analyse_chartsger %>%
      ggplot(aes(x = sfy_streams)) +
      geom_histogram()
    
    # oder
    Analyse_chartsger %>%
      ggplot(aes(x = sfy_streams)) +
      geom_density()
  
  # C) Verteilung der Spotify-Streams (Boxplot)
    Analyse_chartsger %>%
      drop_na() %>%
      ggplot(aes(y = sfy_streams)) +
      geom_boxplot() +
      stat_boxplot(geom = "errorbar")

  # D) Den Zusammenhang zwischen YouTube-Likes und Spotify-Streams (Scatterplot)
    Analyse_chartsger %>%
      ggplot(aes(x = yt_likes, y = sfy_streams)) +
      geom_point()

# Aufgabe 2: Verschönere deine Grafiken aus der ersten Aufgabe nach folgenden Angaben...
  # A) Hellblaue Balken
    Analyse_chartsger %>%
      ggplot(aes(x = gender)) +
      geom_bar(fill = "lightblue")

  # B) Wertebereich = 100
    Analyse_chartsger %>%
      ggplot(aes(x = sfy_streams)) +
      geom_histogram(binwidth = 100)

  # C) rot-umrandete Box und kleiner Fehlerbalken
    Analyse_chartsger %>%
      drop_na() %>%
      ggplot(aes(y = sfy_streams)) +
      geom_boxplot(color = "red") +
      stat_boxplot(geom = "errorbar", width = 0.1)
  
  # D) Beschriftungen
    Analyse_chartsger %>%
      ggplot(aes(x = yt_likes, y = sfy_streams)) +
      geom_point() +
      xlab("YouTube-Likes in Tausend") +
      ylab("Spotify-Streams in Mio.") +
      ggtitle("Zusammenhang zwischen YouTube-Likes und Spotify-Streams")
    