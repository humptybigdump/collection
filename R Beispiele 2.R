library(ggplot2)
library(dplyr)
library(gapminder)

data("gapminder")

#Kennwerte
summary(gapminder)

#Grafische Aufbereitung
Deutschland <- gapminder %>%
  filter(country == "Germany")

Frankreich <- gapminder %>%
  filter(country == "France")

Groﬂbritannien <- gapminder %>%
  filter(country == "United Kingdom")

Auswahl <- rbind(Deutschland, Frankreich, Groﬂbritannien)
ggplot(Auswahl, aes(x = year, y = lifeExp, color = country)) +
  geom_line() + geom_point() +
  ggtitle("Lebenserwartung Deutschland, Frankreich und UK") +
  ylab("Lebenserwartung in Jahren") +
  xlab("") +
  labs(color = "Land")

# Rechnen mit R
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
  
# Variablen zuweisen
  variable1 <- 4
  variable1
  variable2 <- "Baumhaus"
  variable2
  variable3 <- TRUE
  variable3
  
# Datentypen ausgeben lassen
  class(variable2)
  
# Vektoren
  vektor1 <- c(1, 2, 3, 4)
  vektor1

# Vektor umbenennen
  names(vektor1) <- c("Montag", "Dienstag", "Mittwoch", "Donnerstag")
  vektor1  

# Auswahl aus Vektor
  vektor1[c(1, 4)]
  vektor1[c(1:3)]

# Bedingungen
  vektor_auswahl <- vektor1 > 2
  vektor_auswahl
  
# Rechnen in Vektoren
  sum(vektor1)
  mean(vektor1)  
  