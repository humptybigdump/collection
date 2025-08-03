if(rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

# Packages laden
library(tidyverse)
library(plotrix)

# Starwars-Datensatz
data("starwars")
starwars_sample <- starwars %>% 
  select(name, mass, height) %>% 
  drop_na()

## Punktschätzungen
### Gewicht
starwars_sample %>% 
  summarise(mean = mean(mass),
            se = std.error(mass))
  # Mittelwert = 97,3
  # Standardfehler = 22,1 -> relativ große Streuung, vermutlich kein effizienter Schätzer
### Größe
starwars_sample %>% 
  summarise(mean = mean(height),
            se = std.error(height))
  # Mittelwert = 174
  # Standardfehler = 4,63 -> relativ geringe Streuung, möglicherweise effizienter Schätzer

## Intervallschätzungen
### Gewicht
starwars_sample %>% 
  summarise(conf_lower = mean(mass) - 1.96 * std.error(mass),
            conf_upper = mean(mass) + 1.96 * std.error(mass))
  # Intervall: 54,1 - 141 -> Mit einer Sicherheit von 95% liegt die Masse aller Starwars-Charaktere im Intervall zwischen 54,1 und 141 kg.
### Größe
starwars_sample %>% 
  summarise(conf_lower = mean(height) - 1.96 * std.error(height),
            conf_upper = mean(height) + 1.96 * std.error(height))
# Intervall: 165 - 183 -> Mit einer Sicherheit von 95% liegt die Körpergröße aller Starwars-Charaktere im Intervall zwischen 165 und 183 cm.

## t-Tests
### Gewicht
  # Voraussetzung: Normalverteilung?
shapiro.test(starwars_sample$mass)
  # p-value < 0,05 -> eigentlich kein t-Test erlaubt...
  # H0: mass >= 97,3
  # H1: mass < 97,3
  # Signifikanzniveau = 0,05
t.test(starwars_sample$mass, mu = 97.3, alternative = "less")
  # p-value = 0,5002 > 0,05 -> H0 wird beibehalten.

## Größe
  # Voraussetzung: Normalverteilung?
shapiro.test(starwars_sample$height)
  # p-value < 0,05 -> eigentlich kein t-Test erlaubt...
  # H0: height <= 174
  # H1: height > 174
  # Signifikanzniveau = 0,05
t.test(starwars_sample$height, mu = 174, alternative = "greater")
  # p-value = 0,4985 > 0,05 -> H0 wird beibehalten.


# Gapminder-Datensatz
library(gapminder)
set.seed(1)
gapminder_sample <- gapminder %>% 
  filter(year == 2007) %>% 
  sample_n(size = 70)

## Punktschätzungen
### Lebenserwartung
gapminder_sample %>% 
  summarise(mean = mean(lifeExp),
            se = std.error(lifeExp))
  # Mittelwert = 67,6
  # Standardfehler = 1,47 -> relativ geringe Streuung, möglicherweise effizienter Schätzer
### Population
gapminder_sample %>% 
  summarise(mean = mean(pop),
            se = std.error(pop))
  # Mittelwert = 45.342.515
  # Standardfehler = 19.135.961 -> relativ große Streuung, vermutlich kein effizienter Schätzer
### GDP
gapminder_sample %>% 
  summarise(mean = mean(gdpPercap),
            se = std.error(gdpPercap))
  # Mittelwert = 12.113
  # Standardfehler = 1.548 -> relativ große Streuung, vermutlich kein effizienter Schätzer

## Intervallschätzungen
### Lebenserwartung
gapminder_sample %>% 
  summarise(conf_lower = mean(lifeExp) - 1.96 * std.error(lifeExp),
            conf_upper = mean(lifeExp) + 1.96 * std.error(lifeExp))
  # Intervall: 64,8 - 70,5 -> Mit einer Sicherheit von 95% liegt die weltweite durchschnittliche Lebenserwartung zwischen 64,8 und 70,5 Jahren.

### Population
gapminder_sample %>% 
  summarise(conf_lower = mean(pop) - 1.96 * std.error(pop),
            conf_upper = mean(pop) + 1.96 * std.error(pop))
# Intervall: 7.836.033 - 82.848.998 -> Mit einer Sicherheit von 95% liegt die weltweite durchschnittliche Landesbevölkerung zwischen 7.836.033 und 82.848.998 Einwohner:innen.

### GDP
gapminder_sample %>% 
  summarise(conf_lower = mean(gdpPercap) - 1.96 * std.error(gdpPercap),
            conf_upper = mean(gdpPercap) + 1.96 * std.error(gdpPercap))
# Intervall: 64,8 - 70,5 -> Mit einer Sicherheit von 95% liegt das weltweite GDP per capita zwischen 9.079 und 15.148 Dollar.

## t-Tests
### Lebenserwartung
  # Voraussetzung: Normalverteilung?
shapiro.test(gapminder_sample$lifeExp)
  # p-value < 0,05 -> eigentlich kein t-Test erlaubt...
  # H0: lifeExp <= 67,6
  # H1: lifeExp > 67,6
  # Signifikanzniveau = 0,05
t.test(gapminder_sample$lifeExp, mu = 67.6, alternative = "greater")
  # p-value = 0,4906 > 0,05 -> H0 wird beibehalten.

### Population
  # Voraussetzung: Normalverteilung?
shapiro.test(gapminder_sample$pop)
  # p-value < 0,05 -> eigentlich kein t-Test erlaubt...
  # H0: pop <= 45.342.515
  # H1: pop > 45.342.515
  # Signifikanzniveau = 0,05
t.test(gapminder_sample$pop, mu = 45342515, alternative = "greater")
  # p-value = 0,5 > 0,05 -> H0 wird beibehalten.

### GDP
  # Voraussetzung: Normalverteilung?
shapiro.test(gapminder_sample$gdpPercap)
  # p-value < 0,05 -> eigentlich kein t-Test erlaubt...
  # H0: gdpPercap >= 12.113
  # H1: gdpPercap < 12.113
# Signifikanzniveau = 0,05
t.test(gapminder_sample$gdpPercap, mu = 12113, alternative = "less")
  # p-value = 0,5001 > 0,05 -> H0 wird beibehalten.
