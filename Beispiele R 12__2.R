library(dplyr)
library(ggplot2)
library(car)
library(readxl)

# Datensatz
Triathlon <- read_excel("Dateipfad/Triathlon 2022.xlsx")
  # Datensatz liegt im Ilias-Ordner zum Download

# Aufgabe Grafische Darstellungen
Triathlon %>%
  ggplot(aes(y = Trainingsstunden)) +
  stat_boxplot(geom = "errorbar", width = 0.1) +
  geom_boxplot(fill = "lightblue") +
  scale_y_continuous(breaks = seq(0, 35, 2)) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

# Aufgabe Mittelwertvergleiche
t.test(Triathlon$Einkommen ~ Triathlon$Geschlecht)

# Aufgabe Bivariate Analyse
chisq.test(Triathlon$Geschlecht, Triathlon$Disziplin)

# Aufgabe Regression
Triathlon <- Triathlon %>%
  mutate(Disziplin_dummy = ifelse(Triathlon$Disziplin == "Laufen", 1, 0))
Regression_Aufgabe <- lm(Einkommen ~ Disziplin_dummy + Trainingsstunden, data = Triathlon)
summary(Regression_Aufgabe)
