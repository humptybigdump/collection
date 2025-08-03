if(rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

# Hilfe median-Befehl (Folie 25)
?median

# Rechenoperationen in R (Folie 26)
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

# Vektor (Folie 27)
vektor <- c(1, 2, 3, 4, 5, 6, 7)
vektor
names(vektor) <- c("Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag", "Samstag", "Sonntag")

# Auswahl aus Vektor (Folie 27)
vektor[c(1, 7)]
vektor[c(1:3)]

# Rechnen in Vektoren (Folie 27)
sum(vektor)
mean(vektor)

# Bedingungen (Folie 27)
vektor_auswahl <- vektor > 2
vektor_auswahl