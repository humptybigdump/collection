library(dplyr)
library(ggplot2)
library(DescTools)
library(readr)

# Einfache Regression
chartsger <- read_csv2("Dateipfad/chartsger.CSV", trim_ws = TRUE, na = "-77")
Analyse_chartsger <- chartsger  
  # A: Regressionen berechnen
    # YT-Likes und Spotify-Streams
      yt_likes_to_sfy_streams_reg <- lm(sfy_streams ~ yt_likes, data = Analyse_chartsger)
      yt_likes_to_sfy_streams_reg
        # Intercept: 110,84 -> Songs mit 0 YouTube-Likes haben 110,84 Mio. Spotify-Streams.
        # Koeffizient: 0,21 -> Erhöht sich die Anzahl der YouTube-Likes um eintausend (1000), erhöht sich die Anzahl der Spotify-Streams um 0,21 Mio. Streams.
    # Körpergröße und Spotify-Streams
      height_to_sfy_streams_reg <- lm(sfy_streams ~ height, data = Analyse_chartsger)
      height_to_sfy_streams_reg
        # Intercept: -389,55 -> Songs von Interpret*innen mit einer Körpergröße von 0 cm haben -389,55 Mio. Spotify-Streams.
        # Koeffizient: 5,03 -> Erhöht sich die Körpergröße der Interpret*innen um 1 cm, erhöht sich die Anzahl der Spotify-Streams um 5,03 Mio. Streams.
  
  # B: Regressionsgleichungen und Vorhersagen
    # YT-Likes und Spotify-Streams
      # sfy_streams = 110,84 + 0,21 * yt_likes
        110.84 + 0.21 * 14
        # Song mit 14 Tausend YT-Likes hat nach unserem Modell 113,78 Mio. Spotify-Streams.
    # Körpergröße und Spotify-Streams
      # sfy_streams = -389,55 + 5,03 * height
        -389.55 + 5.03 * 200
        # Song von einem*einer Interpret*in mit einer Körpergröße von 200 cm hat nach unserem Modell 616,45 Mio. Spotify-Streams.
  
  # C: Modellgüte und Signifikanz
    # YT-Likes und Spotify-Streams
      summary(yt_likes_to_sfy_streams_reg)
      # r-squared = 0,72 -> 72% der Varianz in den Spotify-Streams werden durch die Variable YouTube-Likes erklärt.
      # p-value < 0,05 -> Es handelt sich um einen signifikanten Einfluss.
    # Körpergröße und Spotify-Streams
      summary(height_to_sfy_streams_reg)
      # r-squared = 0,004 -> 0,4% der Varianz in den Spotify-Streams werden durch die Körpergröße erklärt.
      # p-value > 0,05 -> Es handelt sich nicht um einen signifkanten Einfluss.
      
  # Für Schnelle: blonde Haare und Körpergröße
    # Dummy-Variable blond:
      Analyse_chartsger <- Analyse_chartsger %>%
        mutate(blond_dummy = ifelse(haircol == 1, 1, 0))
    # Regression:
      blond_to_height_reg <- lm(height ~ blond_dummy, data = Analyse_chartsger)
      blond_to_height_reg
        # Intercept: Interpret*innen ohne blonde Haare haben eine Körpergröße von 176,77 cm.
        # Koeffizient: Interpret*innen mit blonden Haaren haben eine um 2,68 cm verringerte Körpergröße.
      summary(blond_to_height_reg)
        # r-squared = 0,014 -> 1,4% der Varianz in der Körpergröße werden durch die Variable blond/nicht-blond erklärt.
        # p-value > 0,05 -> Es handelt sich nicht um einen signifikanten Einfluss.
  