library(dplyr)

data("starwars")
data("storms")

# Datensatz betrachten
starwars

# Datensatz filtern
starwars %>%
  filter(hair_color == "blond")

# Datensatz sortieren
  # aufsteigend
  starwars %>%
    arrange(height)
  # absteigend
  starwars %>%
    arrange(desc(height))

# Kombination
starwars %>%
  filter(hair_color == "blond") %>%
  arrange(desc(height))

# Spalte verändern
starwars %>%
  mutate(height = height / 100)

# Spalte hinzufügen
starwars %>%
  mutate(height_meters = height / 100)

# einzelne Spalten
starwars %>%
  select(eye_color, hair_color)

# Missings löschen
starwars_na <- na.omit(starwars)

# Häufigkeitstabelle mit dplyr
starwars_na %>%
  count(hair_color) %>%
  mutate(percent = 100*n/sum(n), cumulated_percent = cumsum(percent))

# sjmisc
library(sjmisc)

# Häufigkeitstabelle mit sjmisc:
starwars_na %>%
  frq(hair_color)

# Median
median(starwars_na$mass)

# Mittelwert händisch
vektor <- c(1, 2, 3, 4, 5)
sum(vektor) / length(vektor)

# Mittelwert
mean(starwars_na$mass)

# Spannweite
range(starwars_na$birth_year)
range(starwars_na$birth_year) %>% diff()

# Quartile
quantile(starwars_na$height)

# Interquartilsabstand
IQR(starwars_na$height)

# Varianz
var(starwars_na$height)

# Standardabweichung
sd(starwars_na$height)

# z-Standardisierung
scale(starwars_na$height)
