#Packages installieren
install.packages("gapminder")
install.packages("dplyr")
install.packages("ggplot2")

library(gapminder)
library(dplyr)
library(ggplot2)

#Lade den Datensatz
data("gapminder")

#Filtere nur Daten aus Jahr 2007
lifeExp_2007 <- gapminder %>% filter(year == 2007)

#Übersicht Lebenserwartung 2007
summary(lifeExp_2007$lifeExp)

#Grafik 
ggplot(lifeExp_2007, aes(x = lifeExp, y = gdpPercap, color = continent, size = pop)) + 
  geom_point() +
  scale_y_log10() +
  ggtitle("Bevölkerungsdaten 2007")

#Hilfe zu Befehlen
?median

#Variablen zuordnen
var1 <- 4
