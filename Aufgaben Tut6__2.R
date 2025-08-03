library(dplyr)
library(plotrix)
library(ggplot2)

# Aufgabe 1: Punktschätzung in R
  # Schätzer für YouTube-Clicks + Standardfehler
    # händisch
    mean(Analyse_chartsger$yt_clicks) # 124.14
    sd(Analyse_chartsger$yt_clicks) / sqrt(100) # 27.43
    # Interpretation: Durchschnittlich weichen die in den Stichproben beobachteten Mittelwerte für die YouTube-Clicks um 27.43 Mio. Clicks vom tatsächlichen Mittelwert ab. 

    # dplyr
    Analyse_chartsger %>%
      summarize(mean = mean(yt_clicks), sd = sd(yt_clicks), se = sd(yt_clicks) / sqrt(100)) # mean = 124, sd = 274, se = 27.4

    # plotrix
    mean(Analyse_chartsger$yt_clicks) # 124.14
    std.error(Analyse_chartsger$yt_clicks) # -> 27.43
    
# Aufgabe 2: Intervallschätzung in R
  # Konfidenzintervall für YouTube-Abos
    # händisch
    mean(Analyse_chartsger$yt_sub, na.rm = TRUE) + 1.96 * std.error(Analyse_chartsger$yt_sub, na.rm = TRUE) # OG = 9557.94
    mean(Analyse_chartsger$yt_sub, na.rm = TRUE) - 1.96 * std.error(Analyse_chartsger$yt_sub, na.rm = TRUE) # UG = 4052.23
  # Interpretation: Mit einer Sicherheit von 95% liegt die durchschnittliche Anzahl der YouTube-Abos im Intervall zwischen 4.052.234 und 9.557.940 Abonnent*innen.

  # dplyr
  Analyse_chartsger %>%
    summarize(mean = mean(yt_sub, na.rm = TRUE), se = std.error(yt_sub, na.rm = TRUE), UG = mean(yt_sub, na.rm = TRUE) - 1.96 * std.error(yt_sub, na.rm = TRUE), OG = mean(yt_sub, na.rm = TRUE) + 1.96 * std.error(yt_sub, na.rm = TRUE))
    # verkürzt:
    Analyse_chartsger %>%
      summarize(mean = mean(yt_sub, na.rm = TRUE), se = std.error(yt_sub, na.rm = TRUE), UG = mean - 1.96 * se, OG = mean + 1.96 * se)

# Aufgabe 3: z-Test des Mittelwerts in R
  # H0: Mittelwert(GG) = 367
  # H1: Mittelwert(GG) != 367
    mean_sfy_streams <- mean(Analyse_chartsger$sfy_streams)
    mean_sfy_streams
    se_sfy_streams <- std.error(Analyse_chartsger$sfy_streams)
  
  # Konfidenzintervall
    mean_sfy_streams - 1.96 * se_sfy_streams
    mean_sfy_streams + 1.96 * se_sfy_streams    
    # Nur in 5 % der Stichproben würden Mittelwerte vorkommen, die kleiner als 255.58 und größer als 479 Mio. Streams sind.
      # Also: 367 Mio. Streams liegen im angegebenen Intervall -> H0 wird beibehalten

# Für Schnelle: Vergleich von Konfidenzintervallen
    female <- Analyse_chartsger %>%
      filter(gender == 2)
    male <- Analyse_chartsger %>%
      filter(gender == 1)
    # Konfidenzintervall Frauen
    mean(female$sfy_streams) - 1.96 * std.error(female$sfy_streams)
    mean(female$sfy_streams) + 1.96 * std.error(female$sfy_streams)        
      # 96.71 Mio. - 321.35 Mio.
    # Konfidenzintervall Männer
    mean(male$sfy_streams) - 1.96 * std.error(male$sfy_streams)
    mean(male$sfy_streams) + 1.96 * std.error(male$sfy_streams)
      # 262.23 Mio. - 532.97 Mio.
    #Vergleich: Die Intervalle überschneiden sich -> eher kein Unterschied im Hinblick auf die Spotify-Streams.
    sfy_gender = data.frame(gender =c("male","female"), lower = c(262.23, 96.71), upper = c(532.97, 321.35), mean = c(397.60, 209.03))
    sfy_gender %>%
      ggplot(aes(x = gender, y = mean, ymin = lower, ymax = upper, color = gender), width = 0.2, size = 1) + 
      geom_pointrange()