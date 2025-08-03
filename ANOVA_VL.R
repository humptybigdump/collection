# Pfad zum Workspace setzen
# Passe den Pfad so an, dass du den Datensatz auf deinem Rechner laden kannst.
setwd("C:/Users/Clarissa/Downloads/t-Test")

# Pakete einbinden
library(tidyverse)
library(readr)
library(psych)

# Datensatz laden
schoko_zufrieden <- read_csv('schokolade_lebenszufriedenheit_3gruppen.csv')

# Explorative Analyse
view(schoko_zufrieden)
describe(schoko_zufrieden)

schoko.graph <- ggplot(schoko_zufrieden, aes(x=Schokoladenkonsum, y=Lebenszufriedenheit, group=Schokoladenkonsum))+
  geom_boxplot() 
schoko.graph

####################################################################################################################
# t-Test

stats.test.Lebenszufriedenheit <- aov(Lebenszufriedenheit ~ Schokoladenkonsum,
                                      data = schoko_zufrieden)
summary(stats.test.Lebenszufriedenheit)

####################################################################################################################
# Annahmen prüfen

# Normalverteilung
kein_Schokokonsum <- schoko_zufrieden %>% filter(Schokoladenkonsum == 0)
niedriger_Schokokonsum <- schoko_zufrieden %>% filter(Schokoladenkonsum == 1)
hoher_Schokokonsum <- schoko_zufrieden %>% filter(Schokoladenkonsum == 2)

shapiro.test(kein_Schokokonsum$Lebenszufriedenheit)
shapiro.test(niedriger_Schokokonsum$Lebenszufriedenheit)
shapiro.test(hoher_Schokokonsum$Lebenszufriedenheit)

schoko_zufrieden$Schokoladenkonsum <- factor(schoko_zufrieden$Schokoladenkonsum)
leveneTest(Lebenszufriedenheit ~ Schokoladenkonsum, data = schoko_zufrieden)


####################################################################################################################

# Post-hoc Tests

pairwise.t.test(schoko_zufrieden$Lebenszufriedenheit, schoko_zufrieden$Schokoladenkonsum, p.adjust.method = "bonferroni")