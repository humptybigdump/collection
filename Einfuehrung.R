setwd("C:/Users/lkoppers/bwSyncAndShare/WMK/projects/2020_ditkomm/2020-11-14") # Arbeitsverzeichnis waehlen
setwd("/home/lars/Schreibtisch/bwSyncAndShare/WMK/projects/2020_ditkomm/2020-11-14")

# Pakete laden
library(tidyverse) # Sammlung an Paketen aus dem tidyerse
library(RColorBrewer) # Sinnvolle Farbpaletten
library(knitr) # Schoene Tabellen
library(lubridate) # Umgang mit Datumsangaben


## Daten einlesen

# encodingprobleme Die Datei ist latin1 kodiert
Beeren <- read_csv2("41232-0002.csv", skip = 7, n_max = 28) 
view(Beeren)

# Die fehlenden Werte (NA) werden noch nicht korrekt angezeigt
Beeren <- read_csv2("41232-0002.csv", skip = 8, n_max = 28, col_names = FALSE, locale = locale(encoding = "latin1")) view(Beeren)

# korrekte Variante
Beeren <- read_csv2("41232-0002.csv", 
                    skip = 8, # die ersten 8 Zeilen werden uebersprungen
                    n_max = 28, # es werden nur 28 Zeilen eingelesen
                    col_names = FALSE, # Der ausgewaehlte Bereich enthaelt keine Ueberschriften
                    locale = locale(encoding = "latin1"), # Auswahl der latin1 Codierung
                    na = c("-", "x")) # Bestimmte Zeichen werden als fehlende Werte festgelegt
view(Beeren)


## select (rename)
## Auswahl von Variablen
Beeren <- Beeren %>% select(Anbauform = X1, Art = X2, 
                        Betriebe_2018 = X3, Flaeche_2018 = X4, Ernte_2018 = X5, 
                        Betriebe_2019 = X6, Flaeche_2019 = X7, Ernte_2019 = X8)

select(Beeren, "Art", "Flaeche_2018")
Beeren %>% select("Art", "Flaeche_2018")


## mutate
## Neue Variablen und Variablen verändern

Beeren <- Beeren %>% mutate(
  Anbauform = if_else(Anbauform == "Freiland", 
                      "Freiland", 
                      "Schutzabdeckung"))


## pivot_wider pivot_longer

Beeren <- Beeren %>% pivot_wider(names_from = Anbauform,
                                 values_from = c("Betriebe_2018", 
                                                 "Flaeche_2018", 
                                                 "Ernte_2018", 
                                                 "Betriebe_2019", 
                                                 "Flaeche_2019", 
                                                 "Ernte_2019"), 
                                 names_repair = "universal") 

Beeren2 <- Beeren %>%
  pivot_longer(c(-Art), names_to = "key", values_to = "value") %>%
  extract(key, c("tmp", "Jahr", "Anbauform"), "([^_]*)_([^_]*)_([^_]*)") %>%
  pivot_wider(names_from = tmp, values_from = value)


## zurück zu mutate

Beeren3 <- Beeren %>% mutate(GesamtFreiland = Ernte_2018_Freiland + Ernte_2019_Freiland)

Beeren %>% mutate(Gesamternte_2018 = Ernte_2018_Freiland + Ernte_2018_Schutzabdeckung,
                  Gesamternte_2019 = Ernte_2019_Freiland + Ernte_2019_Schutzabdeckung,
                  Durchschnittsernte = (Gesamternte_2018 + Gesamternte_2019) / 2) %>% 
  select(Art, Gesamternte_2018, Gesamternte_2019, Durchschnittsernte)



## summarize
## group_by

Beeren2 %>% summarise(Gesamternte = sum(Ernte, na.rm = TRUE)) # Berechnung der Summe unter Missachtung der fehlenden Werte (na.rm = TRUE)
Beeren2 %>% group_by(Art, Jahr) %>% summarise(Summe = sum(Ernte, na.rm = TRUE))

## filter

Beeren2 %>% filter(Jahr == "2018")

Beeren2 %>% filter(Jahr == "2018" & Anbauform == "Freiland")

## Tabellen mit kable()
Beeren2 %>% filter(Jahr == "2018" & Anbauform == "Freiland") %>% kable()

