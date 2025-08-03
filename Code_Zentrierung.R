########## WORKSPACE LEEREN ##########

rm(list = ls())

########## NOTWENDIGE PACKAGES LADEN ##########

# notwendige packages auswählen
packages <- c("haven", "dplyr")
# noch nicht installierte packages installieren
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
# packages laden
invisible(lapply(packages, library, character.only = TRUE))

########## DATEIEN EINLESEN ##########

# Arbeitsverzeichnis zu Pfad mit Datei ändern
# Session > Set Working Directory > Choose Directory...
setwd("/Users/rp5950/Documents/Lehre/Forschungsmethoden_2/Julia") # <- ANPASSEN!!!

# SPSS-Datei einlesen und in Variable "df" speichern
df <- read_sav("Data_article_SJSM_2019.sav")

# Variable Participant kategorisch machen
df$Participant <- as.factor(df$Participant)

# Dataframe anzeigen
View(df)


# mutate()-Funktion in Paket dplyr, Paket ggf. vorher laden
library(dplyr)

# Variable mit Personenmittelwerten erstellen (Between-Effekt)
df <- df %>%
  mutate(STime_15_between = mean(STime_15, na.rm = TRUE))


# Zentrierung der Variable "STime_15" am Personen-Mittelwert (Within-Effekt)
df <- df %>% group_by(Participant) %>%
mutate(STime_15_within = STime_15 - mean(STime_15, na.rm = TRUE))

# Zentrierung der Variable "STime_15" am Gesamt-Mittelwert (grand mean)
df$STime_15_grand_centered <- df$STime_15 - mean(df$STime_15, na.rm = TRUE)
