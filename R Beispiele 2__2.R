library(ggplot2)
library(dplyr)
library(gapminder)

data("gapminder")

#Kennwerte (Folie 6)
summary(gapminder)

#Grafische Aufbereitung (Folie 6)
Deutschland <- gapminder %>%
  filter(country == "Germany")

Frankreich <- gapminder %>%
  filter(country == "France")

Großbritannien <- gapminder %>%
  filter(country == "United Kingdom")

Auswahl <- rbind(Deutschland, Frankreich, Großbritannien)

ggplot(Auswahl, aes(x = year, y = lifeExp, color = country)) +
  geom_line() + geom_point() +
  ggtitle("Lebenserwartung Deutschland, Frankreich und UK") +
  ylab("Lebenserwartung in Jahren") +
  xlab("") +
  labs(color = "Land")

# Rechnen mit R (Folie 15)
  # Addition
  5 + 3
  # Subtraktion
  5 - 3
  # Multiplikation
  5 * 3
  # Division
  5 / 5
  # Quadrieren
  5^3
  # Modulo
  5 %% 3
  
# Variablen zuweisen (Folie 15 & 16)
  num_var <- 4
  num_var
  cha_var <- "Baumhaus"
  cha_var
  log_var <- TRUE
  log_var
  
# Datentypen ausgeben lassen (Folie 16)
  class(cha_var)
  
# Vektoren (Folie 18)
  vektor <- c(1, 2, 3, 4, 5, 6, 7)
  vektor

# Vektor umbenennen
  names(vektor) <- c("Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag", "Samstag", "Sonntag")
  vektor  

# Auswahl aus Vektor (Folie 18)
  vektor[c(1, 7)]
  vektor[c(1:3)]

# Rechnen in Vektoren (Folie 19)
  sum(vektor)
  mean(vektor)
  
# Bedingungen (Folie 19)
  vektor_auswahl <- vektor > 2
  vektor_auswahl