# Faktoren
  vektor <- c("Superman", "Batman", "Wonder Woman", "Hulk", "Catwoman", "Captain America", "Spiderman")
  faktor <- factor(vektor, ordered = FALSE)
  faktor
  
# geordneter Faktor
  vektor2 <- c("heiß", "heiß", "kalt", "kalt", "mittel", "mittel", "heiß", "heiß", "kalt", "heiß")
  faktor2 <- factor(vektor2, ordered = TRUE, levels = c("kalt", "mittel", "heiß"))
  faktor2

# Zusammenfassung
  summary(faktor)

# Auswahl
  faktor[c(1, 2, 3)]
  faktor[1:4]
  
# Datensatz
  gapminder
  
# Erste Beobachtung
  head(gapminder)

# Struktur
  str(gapminder)
  
# Auswahl aus Datensatz
  gapminder[1506, "country"]
  gapminder$lifeExp

# Subset
  subset(gapminder, subset = year == 2007)

# Sortieren
  positions <- order(gapminder$year)
  gapminder[positions,]
  