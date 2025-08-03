# GueW-Datensatz laden
install.packages("haven")
library(haven)
Roh_GueW <- read_sav("C:/Users/tablu/Documents/Studium/Tutorium/Vorbereitung/04_Deskriptive Analyse/Roh_GueW.sav")

# Datensatz anzeigen
View(Roh_GueW)

# Analysedatensatz anlegen
Analyse_GueW <- Roh_GueW
Analyse_GueW[Analyse_GueW == 9] <- NA
anyNA(Analyse_GueW)
table(is.na(Analyse_GueW))
Analyse_GueW <- na.omit(Analyse_GueW)

# Mit Datensätzen arbeiten: Vorbereitung
# Aufgabe 1:
  
  # A) Lade das DPLYR-Package (oder Tidyverse).
  library(dplyr)
  
  # B) Speichere den Starwars-Datensatz im Environment ab und schaue ihn dir kurz an.
  data("starwars")
  View(starwars)
  
# Aufgabe 2:
  # A) Erstelle eine Spalte, die die Masse in Tonnen angibt.
  starwars <- starwars %>%
    mutate(mass_t = mass / 1000)
  
  # B) Erstelle einen neuen Datensatz, der nur die Variablen "name", "height", "mass", "hair_color", "eye_color", "birth_year" und "sex" enthält. Gib ihm den Namen "Analyse_starwars".
  Analyse_starwars <- starwars %>%
    select(name, height, mass, hair_color, eye_color, birth_year, sex)
  
  # C) Überprüfe, ob dein neuer Datensatz Missings enthält und entferne ggfs. die Fälle.
  anyNA(Analyse_starwars)
  Analyse_starwars <- na.omit(Analyse_starwars)
  
# Mit Datensätzen arbeiten: Exploration
# Aufgabe 1:
  
  # A) Wie viele Charaktere haben blaue Augen?
  Analyse_starwars %>%
    filter(eye_color == "blue")
    # 9 Charaktere haben blaue Augen
  
  # B) Wer ist der kleinste Charakter?
  Analyse_starwars %>%
    arrange(height)
    # Yoda ist der kleinste
  
  # C) Welche Frau ist am ältesten?
  Analyse_starwars %>%
    filter(sex == "female") %>%
    arrange(desc(birth_year))
  # Lumina Unduli ist die älteste Frau.
  
# Mit Datensätzen arbeiten: Deskriptive Analyse
# Aufgabe 1:
  
  # A) Installiere das "sjmisc"-Package und lade es.
  install.packages("sjmisc")
  library(sjmisc)
  
  # B) Wie groß ist der prozentuale Anteil an männlichen Charakteren im Datensatz?
  Analyse_starwars %>%
    frq(sex)
    # ca. 78% der Charaktere sind männlich
  
  # C) Wie viele Charaktere haben rote Augen?
  Analyse_starwars %>%
    frq(eye_color)
  # oder:
  Analyse_starwars %>%
    filter(eye_color == "red")
    # 2 Charaktere haben rote Augen.
  
  # D) Was ist der Modus in der Variable "eye_color"?
  Analyse_starwars %>%
    frq(eye_color, sort.frq = "desc")
    # Modus = brown

# Aufgabe 2:
  
  # A) Bei welchem Wert wird die Verteilung der Körpergrößen im Datensatz genau halbiert?
  median(Analyse_starwars$height)
    # 180 cm
  
  # B) Wie groß sind die Star Wars-Charaktere im Durchschnitt?
  mean(Analyse_starwars$height)
    # 175,63 cm
  
  # C) Sind die weiblichen oder die männlichen Charaktere im Schnitt größer?
  starwars_w <- Analyse_starwars %>%
    filter(sex == "female")
  starwars_m <- Analyse_starwars %>%
    filter(sex == "male")
  mean(starwars_w$height) > mean(starwars_m$height)
  # oder:
  mean(starwars_w$height) < mean(starwars_m$height)
  # oder
  mean(starwars_m$height)
  mean(starwars_w$height)
    # Die Männer sind im Durchschnitt größer als die Frauen (177.04 > 165.67)
  
# Aufgabe 3:
  
  # A) Wie groß ist die Spannweite der Variable Gewicht?
  range(Analyse_starwars$mass)
  range(Analyse_starwars$mass) %>% diff()
    # 123 (von 17 bis 140)
  
  # B) Berechne und interpretiere die Standardabweichung des Geburtsjahres. Handelt es sich eher um eine homogene oder eine heterogene Gruppe?
  sd(Analyse_starwars$birth_year)
    # 153,50, ABER: Ausreißer Yoda (896.0)