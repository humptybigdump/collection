########## WORKSPACE LEEREN ##########

rm(list = ls())

########## NOTWENDIGE PACKAGES LADEN ##########

# notwendige packages auswählen
packages <- c("psych", "lme4", "lmerTest", "performance", "haven", "Matrix")
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

########## HAUPTMERKMALE ZUSAMMENFASSEN ##########


# Informationen über alle Variablen des Dataframes "df" anzeigen
summary(df)

# Alternative (mit Paket "psych", das bereits oben geladen wurde)
describe(df)


########## NULLMODELL UND ICC ##########

# Funktion "lmer" aus Paketen "lme4" und "lmerTest", die bereits oben geladen wurde
null_modell <- lmer(Valence ~ 1 + (1 | Participant), data = df)

# Nullmodell ausgeben lassen
summary(null_modell)

# Varianz-Kovarianzmatrix ausgeben
VarCorr(null_modell)

# ICC berechnen mit Paket "performance", das bereits oben geladen wurde
icc(null_modell)

# Informationskriterien ausgeben lassen
AIC(null_modell)
BIC(null_modell)
logLik(null_modell)

# fixed effects ausgeben lassen
fixef(null_modell)

# random effects ausgeben lassen
ranef(null_modell)


########## RANDOM INTERCEPT MODEL MIT EINEM PRÄDIKTOR ##########

# Valence prädiziert durch STime_15
lme_1 <- lmer(Valence ~ STime_15 + (1 | Participant), data = df)

# Modell ausgeben lassen
summary(lme_1)

# Varianz-Kovarianzmatrix ausgeben
VarCorr(lme_1)

# Informationskriterien ausgeben lassen
AIC(lme_1)
BIC(lme_1)
logLik(lme_1)

# fixed effects ausgeben lassen
fixef(lme_1)

# random effects ausgeben lassen
ranef(lme_1)

########## RANDOM INTERCEPT MODEL AND RANDOM SLOPES MODEL ##########

# Valence prädiziert durch STime_15 (sowohl Intercept als auch Slopes können variieren)
lme_2 <- lmer(Valence ~ STime_15 + (1 + STime_15 | Participant), data = df)

# Modell ausgeben lassen
summary(lme_2)

# Varianz-Kovarianzmatrix ausgeben
VarCorr(lme_2)

# Informationskriterien: Modellvergleich
anova(lme_1,lme_2)

# fixed effects ausgeben lassen
fixef(lme_2)

# random effects ausgeben lassen
ranef(lme_2)