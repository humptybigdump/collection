# Faktoren (Folie 18)
  vektor <- c("Superman", "Batman", "Wonder Woman", "Hulk", "Catwoman", "Captain America", "Spiderman")
  faktor <- factor(vektor, ordered = FALSE)
  faktor
  
# geordneter Faktor (Folie 18)
  vektor2 <- c("heiß", "heiß", "kalt", "kalt", "mittel", "mittel", "heiß", "heiß", "kalt", "heiß")
  faktor2 <- factor(vektor2, ordered = TRUE, levels = c("kalt", "mittel", "heiß"))
  faktor2

# Zusammenfassung (Folie 18)
  summary(faktor)

# Auswahl (Folie 18)
  faktor[c(1, 2, 3)]
  faktor[1:4]
  
# Datensatz (Folie 13)
  library(readr)
  chartsger <- read_csv2("C:/Users/ueneu/Documents/Tutorium/Vorbereitung/Datensätze/chartsger.CSV", trim_ws = TRUE, na = "-77")
  chartsger
  
# Erste Beobachtung (Folie 14)
  head(chartsger)

# Struktur (Folie 14)
  str(chartsger)
  
# Auswahl aus Datensatz (Folie 15)
  chartsger[57, "gender"]
  chartsger$haircol

# Sortieren (Folie 16)
  positions <- order(chartsger$byear)
  chartsger[positions,]
  