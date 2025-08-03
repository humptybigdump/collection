library(dplyr)
library(ggplot2)
library(car)

# Aufgabe Grafische Darstellungen
Triathlon %>%
  ggplot(aes(y = Trainingsstunden)) +
  stat_boxplot(geom = "errorbar", width = 0.1) +
  geom_boxplot(fill = "lightblue")
  
# Aufgabe Mittelwertvergleiche
t.test(Triathlon$Einkommen ~ Triathlon$Geschlecht)

# Aufgabe Bivariate Analyse
chisq.test(Triathlon$Geschlecht, Triathlon$Disziplin)

# Aufgabe Regression
Triathlon$Disziplin_D <- recode(Triathlon$Disziplin, "'Radfahren'=0; 'Schwimmen'=0; 'Laufen'=1")
Regression_Aufgabe <- lm(Einkommen ~ Disziplin_D + Trainingsstunden, data = Triathlon)
summary(Regression_Aufgabe)
