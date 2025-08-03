install.packages("gapminder")
install.packages("dplyr")
install.packages("ggplot")

# Lade die benötigten packages für den Datensatz sowie die Visualisierungen


library(gapminder)
library(dplyr)
library(ggplot2)

#Lade den Datensatz
data("gapminder")

#Filtere aus dem Datensatz nur die Daten für das Jahr 2007
gap_2007 <- gapminder %>% filter (year == 2007)

#Stelle die Grafik zusammen
ggplot(gap_2007, aes(x = lifeExp, y = gdpPercap, color = continent, size = pop)) + 
  geom_point() + 
  scale_y_log10() +
  ggtitle("Bevölkerungsdaten aus dem Jahr 2007")

#Gib eine Übersicht über die Verteilung der Lebenserwartung
summary(gap_2007$lifeExp)