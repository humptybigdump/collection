if(rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

# Packages laden
library(tidyverse)

# Aufgabe 1: Datentypen
## Character-Variable
character_one_value <- "Tintenfische"
character_multiple_values <- c("Sepien", "Kalmare", "Kraken")

## Numeric-Variable
numeric_one_value <- 8
numeric_multiple_values <- c(10, 10, 8)

# Aufgabe 2: Lotto
## Gewinnvektoren
woche_1 <- c(5, -1, -4, 12)
woche_2 <- c(-1, 5, 0, 1)
## Berechnung Gesamtgewinn Woche 1
sum(woche_1)
  # 12 € Gewinn
## Berechnung Gesamtgewinn Woche 2
sum(woche_2)
  # 5 € Gewinn
## Vergleich Woche 1 & 2
sum(woche_1) > sum(woche_2)
  # TRUE: Woche 1 war erfolgreicher als Woche 2
## durschnittlicher Gewinn 1. Woche
mean(woche_1)
  # 3 €

# Aufgabe 3: Bahnfahrten
start <- c("Berlin", "Berlin", "Berlin", "Berlin")
ziel <- c("Karlsruhe", "Stuttgart", "München", "Hamburg")
geplante_fahrzeit <- c(426, 370, 370, 393)
verzoegerung <- c(24, 0, -2, 5)
bahndaten <- data.frame(start, ziel, geplante_fahrzeit, verzoegerung)
## a) Fahrten von oder nach Karlsruhe
bahndaten %>% 
  filter(start == "Karlsruhe" | ziel == "Karlsruhe")
## b) Spalte mit tatsächlicher Fahrzeit und Löschen der Spalten geplante Fahrzeit und Verzögerung
### Option 1:
bahndaten <- bahndaten %>% 
  mutate(tatsaechliche_fahrzeit = geplante_fahrzeit + verzoegerung) %>% 
  select(start, ziel, tatsaechliche_fahrzeit)
### Option 2:
bahndaten <- bahndaten %>% 
  mutate(tatsaechliche_fahrzeit = geplante_fahrzeit + verzoegerung) %>% 
  select(-c(geplante_fahrzeit, verzoegerung))

# Aufgabe 4: Starwars
data("starwars")
## 1) Exploration
### blonde Haare
#### Option 1: filter
starwars %>% 
  filter(hair_color == "blond")
  # 3 Charaktere
#### Option 2: count
starwars %>% 
  count(hair_color)
  # 3 Charaktere
#### Option 3: count + filter
starwars %>% 
  count(hair_color) %>% 
  filter(hair_color == "blond")
  # 3 Charaktere
### häufigste Augenfarbe
starwars %>% 
  count(eye_color, sort = TRUE)
  # brown
### Ausprägungen Geschlecht
starwars %>% 
  count(sex)
  # biolog.: weiblich, hermaphroditisch, männlich, kein Geschlecht
starwars %>% 
  count(gender)
  # sozial: weiblich, männlich
## 2) Spalten films, vehicles, starships entfernen
starwars_neu <- starwars %>% 
  select(-c(films, vehicles, starships))
## 3) Umbennen birth_year in age
starwars_neu <- starwars_neu %>% 
  rename(age = birth_year)
## 4) Spalte mit Geburtsjahr
### umbenannte Version
starwars_neu <- starwars_neu %>% 
  mutate(birth_year = 2024 - age)
### alte Version
#### Option 1: Überschreiben
starwars <- starwars %>% 
  mutate(birth_year = 2024 - birth_year)
#### Option 2: neue Variable
starwars <- starwars %>% 
  mutate(birth_year_new = 2024 - birth_year)
