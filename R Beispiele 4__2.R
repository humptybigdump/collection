library(dplyr)
library(readr)

data("starwars")
chartsger <- read_csv2("C:/Users/ueneu/Documents/Tutorium/Vorbereitung/Datensätze/chartsger.CSV", trim_ws = TRUE, na = "-77")

# Datensatz betrachten (Folie 7)
starwars

# Überblick erhalten (Folie 8)
glimpse(chartsger)

# Datensatz filtern (5 = graue Haare) (Folie 11)
chartsger %>%
  filter(haircol == 5)

# Datensatz sortieren (Folie 11)
  # aufsteigend (Geburtsjahr)
  chartsger %>%
    arrange(byear) %>%
    select(id, gender, byear, haircol, height)

# Kombination (rote Haare, absteigendes Geburtsjahr) (Folie 11)
chartsger %>%
  filter(haircol == 4) %>%
  arrange(desc(byear)) %>%
  select(id, gender, byear, haircol, height)

# Spalte veraendern (Folie 9)
chartsger
chartsger %>%
  mutate(height = height / 100)

# einzelne Spalten (Folie 9)
chartsger %>%
  select(gender, haircol)

# Haeufigkeitstabelle mit dplyr (Folie 13)
chartsger %>%
  count(haircol, sort = TRUE) %>%
  mutate(percent = 100*n/sum(n), cumulated_percent = cumsum(percent))

# Haeufigkeitstabelle mit sjmisc (Folie 14)
install.packages("sjmisc")
library(sjmisc)
chartsger %>%
  frq(haircol, sort.frq = "desc")

# Median (Folie 16)
median(chartsger$crank)
chartsger %>%
  summarise("median_crank" = median(crank))

# Mittelwert haendisch (Folie 17)
vektor <- c(1, 2, 3, 4, 5)
sum(vektor) / length(vektor)

# Mittelwert (Folie 17)
mean(chartsger$sfy_streams)
chartsger %>%
  summarise("mean_sfy_streams" = mean(sfy_streams))

# Spannweite (Folie 19)
Analyse_chartsger %>%
  drop_na(byear) %>%
  summarise("range_byear" = range(byear))
Analyse_chartsger %>%
  drop_na(byear) %>%
  summarise("range_byear" = range(byear) %>% diff())

# Quartile (Folie 20)
quantile(Analyse_chartsger$crank)

# Interquartilsabstand (Folie 20)
IQR(Analyse_chartsger$crank)

# Varianz (Folie 21)
var(Analyse_chartsger$yt_clicks)

# Standardabweichung (Folie 21)
sd(Analyse_chartsger$yt_clicks)

# z-Standardisierung (Folie 22)
scale(Analyse_chartsger$yt_clicks)

# Werte in einer neuen Spalte zusammenfassen (Folie 25)
chartsger %>%
  mutate(yt_sub_grouped = cut(yt_sub,
                              breaks = c(0, 1, 100, 1000, 10000, 100000),
                              labels = c("0", "1-99", "100-999", "1000-9999", "10000 und höher"),
                              right = FALSE, ordered_result = TRUE)) %>%
  select(id, yt_sub, yt_sub_grouped)

# Einheit einer Spalte verändern (Folie 26)
chartsger %>%
  mutate(height_in_m = height / 100) %>%
  select(id, height, height_in_m)
