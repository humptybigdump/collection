library(tidyverse)
library(readr)
library(car)
library(QuantPsyc)
library(patchwork)

# Datensatz
chartsger <- readr("Dateipfad/chartsger.CSV", trim_ws = TRUE, na = "-77")
Analyse_chartsger <- chartsger

# A) Multiple Regression: YT-Likes + Körpergröße -> Spotify-Streams
model_streams_against_likes_height <- lm(sfy_streams ~ yt_likes + height, data = Analyse_chartsger)
model_streams_against_likes_height
  # b0 = -473.99 -> Songs mit 0 YT-Likes, die von Interpret*innen mit einer Körpergröße von 0 cm stammen, haben -473,99 Mio. Spotify-Streams.
  # b1 = 0.20 -> Erhöht sich die Anzahl der YT-Likes um 1 Tausend, so erhöht sich die Anzahl der Spotify-Streams um 0,2 Mio.
  # b2 = 3.58 -> Erhöht sich die Körpergröße des*der Interpret*in um 1 cm, erhöht sich die Anzahl der Spotify-Streams um 3,58 Mio.

# B) standardisierte Koeffezienten
  # Variante 1:
  Analyse_chartsger <- Analyse_chartsger %>%
    mutate(sfy_streams_s = scale(sfy_streams), yt_likes_s = scale(yt_likes), height_s = scale(height))
  lm(sfy_streams_s ~ yt_likes_s + height_s, data = Analyse_chartsger)
  # Variante 2:
  lm.beta(model_streams_against_likes_height)
    # beta1 = 0.83
    # beta2 = 0.05
      # -> YT-Likes haben einen größeren Einfluss auf die Spotify-Streams als die Körpergröße.

# C) Modellgüte und Signifikanz
  summary(model_streams_against_likes_height)
    # r2 = 0.692, Adjusted r2 = 0.7012
      # -> unterscheiden sich kaum, d.h. das Modell lässt sich gut über die Stichprobe hinaus verallgemeinern.
    # p-value (F-Test) < 0.05 -> H0 ablehnen und davon ausgehen, dass das Modell eine Erklärungsleistung für die Grundgesamtheit hat. 
    # p-value yt_likes < 0.05 -> signifikanter Koeffizient
    # p-value height > 0.05 -> nicht signifikant
  
# Für Schnelle I: YT-Likes + Körpergröße + Chartwochen -> Spotify-Streams
  model_streams_against_likes_height_cweeks <- lm(sfy_streams ~ yt_likes + height + cweek, data = Analyse_chartsger)
  anova(model_streams_against_likes_height, model_streams_against_likes_height_cweeks)
    # Pr(>F) = 0.0004 < 0.05
      # Das neue Modell hat eine verbesserte Erklärungsleistung.

# Für Schnelle II: Vorannahmen prüfen
  # partielle Regressionsdiagramme
    chartsger_plots <- Analyse_chartsger
    # Regression Streams und Größe
    model_height <- lm(sfy_streams ~ height, data = chartsger_plots)
    chartsger_plots$height_residuals <- residuals(model_height)
    # Regression Streams und Likes
    model_likes <- lm(sfy_streams ~ yt_likes, data = chartsger_plots)
    chartsger_plots$likes_residuals <- residuals(model_likes)
    # Regression Größe und Likes
    model_height_against_likes <- lm(height ~ yt_likes, data = chartsger_plots)
    chartsger_plots$height_against_likes_residuals <- residuals(model_height_against_likes)
    # Regression Likes und Größe
    model_likes_against_height <- lm(yt_likes ~ height, data = chartsger_plots)
    chartsger_plots$likes_against_height_residuals <- residuals(model_likes_against_height)
    # Konstanthalten Größe
    plot1 <- chartsger_plots %>%
      ggplot(aes(x = likes_against_height_residuals, y = height_residuals)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "lightblue") +
      ylab("sfy_streams") +
      xlab("yt_likes") +
      ggtitle("YouTube-Likes")
    # Konstanthalten Likes
    plot2 <- chartsger_plots %>%
      ggplot(aes(x = height_against_likes_residuals, y = likes_residuals)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "tomato") +
      ylab("sfy_streams") +
      xlab("height") +
      ggtitle("Größe")
    # Plots zusammenfassen
    plot1 +
      plot2 +
      plot_layout(ncol = 2) +
      plot_annotation(title = "Partielle Regression", theme = theme(plot.title = element_text(hjust = 0.5)))
      # tendenziell gegeben
    # -> Vorannahme erfüllt
  # wenige Ausreißer/einflussreiche Fälle
    table(scale(residuals(model_streams_against_likes_height)) > 3)
      # nur 1 Ausreißer
    table(hatvalues(model_streams_against_likes_height) > 1)
      # keine einflussreichen Fälle
    # -> Vorannahme erfüllt
  # Multikollinearität
    Analyse_chartsger %>%
      filter(!is.na(yt_likes), !is.na(height)) %>%
      cor()
      # r = 0.023 -> keine Korrelation
    vif(model_streams_against_likes_height)
      # VIF = 1.0005 -> nicht problematisch
    # Vorannahme erfüllt
  # Homoskedastizität
    Analyse_chartsger %>%
      filter(!is.na(yt_likes), !is.na(height)) %>%
      ggplot(aes(x = sfy_streams, y = scale(residuals(model_streams_against_likes_height)))) +
        geom_point()
      # -> keine Tendenz
    # -> Vorannahme erfüllt
  # Normalverteilung Residuen
    Analyse_chartsger <- Analyse_chartsger %>%
      filter(!is.na(yt_likes), !is.na(height))
    Analyse_chartsger$residuals <- residuals(model_streams_against_likes_height)
    Analyse_chartsger %>%
      ggplot(aes(x = scale(residuals))) +
      geom_histogram()
      # keine Normalverteilung
    shapiro.test(residuals(model_streams_against_likes_height))
      # p-value < 0.05 -> keine Normalverteilung
    # -> Vorannahme nicht erfüllt
  # Unkorreliertheit der Residuen (Autokorrelation)
    durbinWatsonTest(model_streams_against_likes_height)
      # D-W = 2.04 -> nahe 2 -> keine Autokorrelation
    # -> Vorannahme erfüllt