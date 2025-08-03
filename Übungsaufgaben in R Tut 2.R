#Laden von gapminder und dplyr
library("gapminder")
library("dplyr")
data(gapminder)

#Nach continent und year filtern
gapminder %>%
  filter(continent == "Africa", year == "1997") 

#Nach Einwohnerzahl sortieren
gapminder %>%
  filter(continent == "Africa", year == "1997") %>%
  arrange(pop)
#beziehungsweise
gapminder %>%
  filter(continent == "Africa", year == "1997") %>%
  arrange(desc(pop))

#Einwohnerzahl für ganzen Datensatz in Tausende umrechnen
gapminder %>%
  mutate(pop_in_1k = pop / 1000)

gapminder %>%
  filter(year == 1957) %>%
  summarize(medianLifeExp = median(lifeExp))









#Summary über lifeExp
summary(gapminder$lifeExp)

#Minimale Einwohnerzahl
min(gapminder$pop)
