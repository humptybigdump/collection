# Pfad zum Workspace setzen
# Passe den Pfad so an, dass du den Datensatz auf deinem Rechner laden kannst.
setwd("C:/Users/Clarissa/Downloads/t-Test")

# Pakete einbinden
library(tidyverse)
library(readr)
library(psych)

# Datensatz laden
schoko_zufrieden <- read_csv('schokolade_lebenszufriedenheit_abhaengig.csv')

# Explorative Analyse
view(schoko_zufrieden)
describe(schoko_zufrieden)

schoko.graph <- ggplot(schoko_zufrieden, aes(x=Schokoladenkonsum, y=Lebenszufriedenheit, group=Schokoladenkonsum))+
  geom_boxplot() 
schoko.graph

####################################################################################################################
# t-Test

stats.test.Lebenszufriedenheit <- t.test(Lebenszufriedenheit ~ Schokoladenkonsum, data =  schoko_zufrieden, paired = TRUE)
stats.test.Lebenszufriedenheit 

####################################################################################################################
# Annahmen prüfen

# Normalverteilung
niedriger_Schokokonsum <- schoko_zufrieden %>% filter(Schokoladenkonsum == 0)
hoher_Schokokonsum <- schoko_zufrieden %>% filter(Schokoladenkonsum == 1)


