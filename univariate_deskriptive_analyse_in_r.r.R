## Beispiele für die Sitzung Datenbereinigung/Exploration

## Benötigte Pakete laden

library(tidyverse)

## Datensatz einlesen - liegt als CSV-Datei im Ordner data vor

wissenschaftliche_akteure <- read_csv("data/codierung_wissenschaftliche_akteure.csv")

head(wissenschaftliche_akteure) %>% view()

glimpse(wissenschaftliche_akteure)

## Haeufigkeiten ausgeben lassen

wissenschaftliche_akteure %>% count(gender, sort = TRUE) %>%
  mutate(percent = 100*n/sum(n), cumulated_percent = cumsum(percent))

wissenschaftliche_akteure %>% count(nameinst, sort = TRUE) %>% 
  mutate(percent = 100*n/sum(n), cumulated_percent = cumsum(percent))

## Unuebersichtliche Haeufigkeitstabelle

wissenschaftliche_akteure %>% count(publications) %>% 
  mutate(percent = 100*n/sum(n), cumulated_percent = cumsum(percent))

## Werte zu Gruppen zusammenfassen mit cut

wissenschaftliche_akteure <- wissenschaftliche_akteure %>%
  mutate(publications_grouped = cut(publications,
                                    breaks = c(0, 1, 50, 100, 100000),
                                    labels = c("0", "1-49", "50 - 99", "100 und höher"),
                                    right = FALSE, ordered_result = TRUE))

## Uebersichtliche Haeufigkeitstabelle, Alternativfunktion verwendet

library(summarytools)

wissenschaftliche_akteure %>% freq(publications_grouped)

## Median

median(c(1, 3, 5, 8, 10, 10, 12))

median(c(1, 3, 5, 8, 10, 10, 12, 13))

## Modus

wissenschaftliche_akteure %>% count(publications, sort = TRUE) %>% 
  mutate(percent = 100*n/sum(n), cumulated_percent = cumsum(percent)) %>% 
  head()

## Mittelwert quasi-haendisch

zahlenfolge <- c(1,5,12,6,9,3)

sum(zahlenfolge)/length(zahlenfolge)

## Vergleich Mittelwert und Median

wissenschaftliche_akteure %>%
  summarise("Mittelwert Publikationszahl" = mean(publications, na.rm = TRUE),
            "Median Publikationszahl" = median(publications, na.rm = TRUE))

## Visualisierung Vergleich Mittelwert und Median

wissenschaftliche_akteure %>% filter(!is.na(publications)) %>%
  ggplot(aes(publications)) + geom_histogram(bins = 50, color = "white") +
    geom_vline(aes(xintercept = median(publications)), color = "blue") +
    geom_vline(aes(xintercept = mean(publications)), color = "red") +
    geom_text(aes(x = median(publications) +20, y = 300, label = "Median")) +
    geom_text(aes(x = mean(publications) +25, y = 250, label = "Mittelwert")) +
    scale_x_continuous(limits = c(-15,1000), expand = expansion(mult = c(0.02, 0.05))) +
    labs(x = "Zahl der Publikationen", y = "Häufigkeit",
         title = "Verteilung der Publikationszahl je Forscher:in")

## Minimum, Maximum und Spannweite

min(zahlenfolge)
max(zahlenfolge)
range(zahlenfolge)

range(wissenschaftliche_akteure$publications, na.rm = TRUE)

range(wissenschaftliche_akteure$publications, na.rm = TRUE) %>% diff()

## Quartile und Interquartilsabstand

quantile(wissenschaftliche_akteure$publications, na.rm = TRUE,
        probs = c(0,0.25,0.5,0.75,1))

IQR(wissenschaftliche_akteure$publications, na.rm = TRUE)

## Varianz haendisch und mit Funktion

zahlenfolge

mean(zahlenfolge)

zahlenfolge - mean(zahlenfolge)

sum(zahlenfolge - mean(zahlenfolge))

(zahlenfolge - mean(zahlenfolge))^2

sum((zahlenfolge - mean(zahlenfolge))^2)

sum((zahlenfolge - mean(zahlenfolge))^2)/length(zahlenfolge)

var(zahlenfolge)

var(wissenschaftliche_akteure$publications, na.rm = TRUE)

## Standardabweichung

sqrt(var(zahlenfolge))

sd(zahlenfolge)

sd(wissenschaftliche_akteure$publications, na.rm = TRUE)

## Vergleich verschiedener Variablen

wissenschaftliche_akteure %>%
  pivot_longer(c(publications, anzahl_erwaehnungen),
               names_to = "Variable", values_to = "Value") %>% 
  group_by(Variable) %>% 
  summarise(Mittelwert = mean(Value, na.rm = TRUE), Median = median(Value, na.rm = TRUE),
            Varianz = var(Value, na.rm = TRUE), Standardabweichung = sd(Value, na.rm = TRUE),
            Minimum = min(Value, na.rm = TRUE), Maximum = max(Value, na.rm = TRUE),
            Interquartilsabstand = IQR(Value, na.rm = TRUE))

## z-Standardisierung

zahlenfolge

zahlenfolge_standardisiert <- (zahlenfolge - mean(zahlenfolge))/sd(zahlenfolge)

zahlenfolge_standardisiert

c(mean(zahlenfolge_standardisiert), sd(zahlenfolge_standardisiert))

wissenschaftliche_akteure %>%
  mutate(across(c(publications, anzahl_erwaehnungen), ~ as.vector(scale(.x)))) %>%  
  pivot_longer(c(publications, anzahl_erwaehnungen),
               names_to = "Variable", values_to = "Value") %>% 
  group_by(Variable) %>% 
  summarise(Mittelwert = mean(Value, na.rm = TRUE), Median = median(Value, na.rm = TRUE),
            Varianz = var(Value, na.rm = TRUE), Standardabweichung = sd(Value, na.rm = TRUE),
            Minimum = min(Value, na.rm = TRUE), Maximum = max(Value, na.rm = TRUE),
            Interquartilsabstand = IQR(Value, na.rm = TRUE)) %>%
  mutate(across(-Variable, round, digits = 3))

wissenschaftliche_akteure %>%
  mutate(z_wert_publications = as.vector(scale(publications))) %>% 
  select(name, publications, z_wert_publications) %>% head()
