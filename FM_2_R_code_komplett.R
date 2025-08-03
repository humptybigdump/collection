########## WORKSPACE LEEREN ##########

rm(list = ls())

########## NOTWENDIGE PACKAGES LADEN ##########

# notwendige packages auswählen
packages <- c("haven", "dplyr", "lme4", "lmerTest", "performance", "Matrix", "ggplot2", "ggthemes", "jtools")
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



### Erstellung neuer Variablen zur Zentrierung

# mutate()-Funktion in Paket dplyr, Paket ggf. vorher laden
library(dplyr)

# Variable mit Personenmittelwerten erstellen (Between-Effekt)
df <- df %>%
  group_by(Participant) %>%
  mutate(STime_15_between = mean(STime_15, na.rm = TRUE)) %>%
  ungroup()

# Zentrierung der Variable "STime_15" am Personen-Mittelwert (Within-Effekt)
df <- df %>% group_by(Participant) %>%
mutate(STime_15_within = STime_15 - mean(STime_15, na.rm = TRUE))

# Zentrierung der Variable "STime_15" am Gesamt-Mittelwert (grand mean)
df$STime_15_grand_centered <- df$STime_15 - mean(df$STime_15, na.rm = TRUE)

# Dataframe anzeigen
View(df)



### Spaghetti-Plots
# Multilevel-Modell anpassen (lme4 und lmerTest)
model <- lmer(Valence ~ STime_15 * Gender + (STime_15 | Participant), data = df)
summary(model)


# Daten vorbereiten
df_clean <- df %>%
  filter(!is.na(STime_15), !is.na(Valence), !is.na(Gender))


# Vorhersagen aus dem Modell extrahieren
df_clean$predicted_valence <- predict(model)

# Plot erstellen
p <- ggplot(df_clean, aes(x = STime_15, y = predicted_valence, group = Participant, color = factor(Gender))) +
  geom_line(size = 0.5) +  # Individuelle Regressionslinien basierend auf dem Multilevel-Modell
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 1.5, aes(group = 1)) +  # Gesamtregressionslinie
  scale_color_manual(values = c("0" = "blue", "1" = "red"), 
                     labels = c("0" = "Male", "1" = "Female"),
                     name = "Gender") +
  labs(x = "Sedentary Time (15-min intervals)", 
       y = "Valence", 
       title = "Spaghetti Plot Random Slopes Model") +
  theme_apa() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  ) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.2)) +
  scale_y_continuous(breaks = seq(0, 100, by = 20))

# Plot anzeigen
print(p)
ggsave("valence_sittingtime_multilevel_plot.pdf", plot = p, width = 6, height = 10, units = "in", dpi = 300)




