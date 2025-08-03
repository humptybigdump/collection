setwd("C:/Users/lkoppers/bwSyncAndShare/WMK/projects/2020_ditkomm/2020-11-14")
setwd("/home/lars/Schreibtisch/bwSyncAndShare/WMK/projects/2020_ditkomm/2020-11-14")

library(tidyverse) # Sammlung an Paketen aus dem tidyerse
library(RColorBrewer) # Sinnvolle Farbpaletten
library(knitr)
library(lubridate) # Umgang mit Datumsangaben


## Daten einlesen

RKI <- read_csv("RKI_COVID19.csv")
view(RKI)


## Beispielanalyse

# Im folgenden soll pro Landkreis die Zahl der gemeldeten Faelle ueber alle Altersklassen und Tage gebildet werden.
# Ausserdem soll der hoechste an einem Tag gemeldete Wert pro Landkreis ausgegeben werden.

# Summe der Fallzahlen
Fallzahlen_LK <- RKI %>% 
  filter(!(NeuerFall == -1)) %>% # Entfernen der Datenkorrekturen laut Dokumentation des Datensatzes
  group_by(Landkreis) %>% # Der Datensatz wird nach Landkreis gruppiert
  summarise(Faelle = sum(AnzahlFall)) # Die Summe der Faelle wird pro Landkreis gebildet

# Hoechster Wert pro Tag
# Da es pro Tag und Landkreis mehrere Eintraege gibt, muessen diese erst zusammengefasst werden
maxFallzahlen_LK <- RKI %>% 
  filter(!(NeuerFall == -1)) %>% # Entfernen der Datenkorrekturen laut Dokumentation des Datensatzes
  group_by(Landkreis, Meldedatum) %>% # Der Datensatz wird nach Landkreis gruppiert
  summarise(Faelle = sum(AnzahlFall)) # Die Summe der Faelle wird pro Landkreis un Tag gebildet

view(maxFallzahlen_LK)

# jetzt muss noch das Maximum bestimmt werden, dazu muss der Datensatz anders gruppiert werden.
maxFallzahlen_LK <- maxFallzahlen_LK %>% 
  group_by(Landkreis) %>% summarise(maxFaelle = max(Faelle))


# Die beiden Datensaetze koennen jetzt mit full_join wieder zusammengefuegt werden
Landkreis_Metriken <- full_join(Fallzahlen_LK, maxFallzahlen_LK, by = "Landkreis")

# Fuer manche weitere Analysen kann es besser sein, dass die Zahlen in einer Spalte stehen 
# und in einer weiteren die Information, ob es die Summe oder das Maximum ist

Landkreis_Metriken %>% pivot_longer(-Landkreis, names_to = "Metrik")
