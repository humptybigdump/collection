library(dplyr)
library(ggplot2)
library(tidyr)
library(sjmisc)

# Datensatz vorbereiten
data("starwars")
Analyse_starwars <- starwars %>%
  select(name, height, mass, hair_color, eye_color, birth_year, sex)
Analyse_starwars <- na.omit(Analyse_starwars)

# Balkendiagramm (Häufigkeiten visualisieren)
starwars %>%
  frq(hair_color, sort.frq = "desc")

starwars$hair_color = factor(starwars$hair_color, levels = c('none', 'brown', 'black', 'white', 'blond', 'auburn', 'auburn, grey', 'auburn, white', 'blonde', 'brown, grey', 'grey', 'unknown'))

starwars %>%
  select(hair_color) %>%
  drop_na() %>%
  ggplot(aes(x = hair_color)) +
  geom_bar(fill = "tomato", color = "black") +
  xlab("Haarfarbe") +
  ylab("Häufigkeit") +
  labs(caption = "N = 82") +
  ggtitle("Haarfarben der Starwars-Charaktere") +
  theme(plot.title = element_text(size = 18, hjust = 0.5), axis.line = element_line(color = "black")) +
  scale_x_discrete(labels = c("keine", "braun", "schwarz", "weiß", "blond", "rot", "rot-grau", "rot-weiß", "blonde", "braun-grau", "grau", "unbekannt"))

# Histogramm
starwars %>%
  select(height) %>%
  drop_na() %>%
  ggplot(aes(x = height)) +
  geom_histogram(binwidth = 10, fill = "tomato", color = "black") +
  xlab("Größe") +
  ylab("Häufigkeit") +
  labs(caption = "N = 32") +
  ggtitle("Größen der Starwars-Charaktere") +
  theme(plot.title = element_text(size = 18, hjust = 0.5), axis.line = element_line(color = "black"))

# Kreisdiagramm
Geschlecht <- as.data.frame(table(starwars$sex))
Geschlecht$Prozent <- paste(round(Geschlecht$Freq/sum(Geschlecht$Freq)*100), "%")
Geschlecht <- Geschlecht %>%
  arrange(desc(Freq))
Geschlecht$Var1 = factor(Geschlecht$Var1, levels = c('hermaphroditic', 'none', 'female', 'male'))
Geschlecht %>%
  ggplot(aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", color = "black") +
  coord_polar("y") +
  theme_void() +
  scale_fill_manual(breaks = c("male", "female", "none", "hermaphroditic"), labels = c("männlich", "weiblich", "kein Geschlecht", "zwittrig"), values = c("tomato", "palegreen", "lightblue", "white")) +
  geom_label(aes(label = Prozent),
             position = position_stack(vjust = 0.5),
             label.size = 0,
             size = 5,
             show.legend = FALSE) +
  labs(fill = "Geschlecht") +
  theme(legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        plot.title = element_text(size = 18, hjust = 0.5)) +
  ggtitle("Geschlechter der Starwars-Charaktere") +
  labs(caption = "N = 83")

# Liniendiagramm
data("gapminder")
gapminder %>%
  filter(country == c("Germany", "United Kingdom", "France", "Sweden")) %>%
  ggplot(aes(x = year, y = lifeExp, color = country)) +
  geom_point(size = 3) + geom_line() +
  scale_color_manual(values = c("tomato", "palegreen", "lightblue", "grey"), labels = c("Frankreich", "Deutschland", "Schweden", "Großbritannien")) +
  ylim(0, 85) +
  ylab("Lebenserwartung in Jahren") +
  xlab("Jahr") +
  labs(color = "Land") +
  ggtitle("Entwicklung der Lebenserwartung") +
  theme(plot.title = element_text(size = 18, hjust = 0.5), axis.line = element_line(color = "black"))

# Boxplot
starwars %>%
  ggplot(aes(y = birth_year)) +
  stat_boxplot(geom = "errorbar", width = 0.1) +
  geom_boxplot(fill = "tomato") +
  scale_x_discrete() +
  labs(y = "Alter in Jahren", caption = "N = 43") +
  ggtitle("Alter Starwars-Charaktere") +
  theme(plot.title = element_text(size = 18, hjust = 0.5), axis.line = element_line(color = "black"))

# Scatterplot
starwars %>%
  filter(mass < 1000) %>%
  ggplot(aes(x = height, y = mass, color = sex)) +
  geom_point() +
  scale_color_manual(values = c("tomato", "palegreen", "lightblue", "violet", "grey"), labels = c("weiblich", "zwittrig", "männlich", "kein Geschlecht", "keine Angabe")) +
  ggtitle("Zusammenhang zwischen Masse und Größe nach Geschlecht") +
  ylab("Masse") +
  xlab("Größe") +
  labs(color = "Geschlecht") +
  theme(plot.title = element_text(hjust = 0.5))

starwars %>%
  filter(mass < 1000) %>%
  ggplot(aes(x = height, y = mass)) +
  geom_point() +
  facet_wrap(~ sex) +
  ggtitle("Zusammenhang zwischen Masse und Größe nach Geschlecht") +
  ylab("Masse") +
  xlab("Größe") +
  labs(caption = "N = 58") +
  theme(plot.title = element_text(hjust = 0.5))
