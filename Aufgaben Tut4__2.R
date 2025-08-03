# chartsger-Datensatz laden
install.packages("readr")
library(readr)
chartsger <- read_csv2("C:/Users/ueneu/Documents/Tutorium/Vorbereitung/Datensätze/chartsger.CSV", trim_ws = TRUE, na = "-77")

# Datensatz anzeigen
View(chartsger)

# Mit Datensaetzen arbeiten: Vorbereitung
  #A) Erstelle einen Analyse-Datensatz. Installiere das Tidyverse und lade dplyr.
  Analyse_chartsger <- chartsger
  install.packages("tidyverse")
  library(tidyverse)
  #B) Erstelle eine Spalte mit Spotify-Streams in absoluten Zahlen (*1.000.000).
  Analyse_chartsger %>%
    mutate(sfy_streams_abs = sfy_streams * 1000000) %>%
    select(id, sfy_streams, sfy_streams_abs)
  #C) Erstelle einen Datensatz (Analyse_chartsger_short) mit den Variablen id, gender, byear, haircol und height.
  Analyse_chartsger_short <- Analyse_chartsger %>%
    select(id, gender, byear, haircol, height)
  #D) Überprüfe, ob der Datensatz Missings enthält.
  anyNA(Analyse_chartsger_short) # L: TRUE
  table(is.na(Analyse_chartsger_short)) # L: 48 NAs
  Analyse_chartsger_short <- na.omit(Analyse_chartsger_short)

# Mit Datensaetzen arbeiten: Exploration
  # A) Wie viele Musiker*innen sind nonbinär (divers)?
  Analyse_chartsger %>%
    filter(gender == 3)
    # L: Nur ein*e Musiker*in in den Top100-Charts ist nonbinär.
  # B) Auf welcher Chartplatzierung befand sich der*die kleinste Musiker*in?
  Analyse_chartsger %>%
    arrange(height)
    # L: Der*die kleinste Musiker*in befand sich auf Platz 43 und 50 (beide 160 cm).
  # C) Wann ist die juengste Musikerin geboren?
  Analyse_chartsger %>%
    filter(gender == 2) %>%
    arrange(desc(byear))
  # L: Die juengste Musikerin ist 2002 geboren.
  
# Mit Datensaetzen arbeiten: Deskriptive Analyse
# Aufgabe 1:
  # A) Installiere das "sjmisc"-Package und lade es.
  install.packages("sjmisc")
  library(sjmisc)
  # B) Wie groß ist der prozentuale Anteil an Musikern (gender = 1) im Datensatz?
  Analyse_chartsger %>%
    frq(gender)
    # L: Ca. 80 Prozent der Musiker*innen im Datensatz sind maennlich.
  # C) Wie viele Musiker*innen haben eine Glatze (haircol = 7)?
  Analyse_chartsger %>%
    frq(haircol)
  # oder:
  Analyse_chartsger %>%
    filter(haircol == 7)
    # L: Zwei Musiker*innen haben eine Glatze.
  # D) Was ist der Modus in der Variable "byear"?
  Analyse_chartsger %>%
    frq(byear, sort.frq = "desc")
    # L: Der Modus in der Variable "byear" ist 1998.

# Aufgabe 2:
  # A) Bei welchem Wert wird die Verteilung der Koerpergroessen im Datensatz genau halbiert?
  Analyse_chartsger %>%
    drop_na(height) %>%
    summarise("median_height" = median(height))
    # L: Mit dem Wert 178cm wird die Verteilung der Koerpergroessen im Datensatz halbiert (Median).
  # B) Wie viele YouTube-Clicks haben die Chartsongs im Durchschnitt?
  Analyse_chartsger %>%
    summarise("mean_yt_clicks" = mean(yt_clicks))
  mean(Analyse_chartsger$yt_clicks)
    # L: Die Chartsongs haben im Durchschnitt 124,14 Mio. Clicks.
  # C) Sind die weiblichen oder die maennlichen Musiker*innen im Schnitt groesser?
  chartsger_fem <- Analyse_chartsger %>%
    filter(gender == 2) %>%
    drop_na(height)
  chartsger_male <- Analyse_chartsger %>%
    filter(gender == 1) %>%
    drop_na(height)
  mean(chartsger_fem$height) > mean(chartsger_male$height)
  # oder:
  mean(chartsger_fem$height) < mean(chartsger_male$height)
  # oder
  mean(chartsger_fem$height)
  mean(chartsger_male$height)
  # Die Maenner sind im Durchschnitt groesser als die Frauen (179cm > 165.36cm)
  
# Aufgabe 3:
  # A) Wie gross ist die Spannweite bei den Streams auf Spotify?
  range(Analyse_chartsger$sfy_streams)
  range(Analyse_chartsger$sfy_streams) %>% diff()
    # L: Die Spannweite beträgt 2.699,1 Mio. Streams (von 1,9 Mio. bis 2.701 Mio.). 
  # B) Berechne und interpretiere die Standardabweichung der Spotify-Streams. Handelt es sich eher um eine homogene oder eine heterogene Gruppe?
  sd(Analyse_chartsger$sfy_streams)
    # L: Die Standardabweichung beträgt 569,95 Mio. Streams. Es handelt sich eher um eine heterogene Gruppe.