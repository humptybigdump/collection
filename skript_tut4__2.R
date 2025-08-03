if(rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

# Packages laden
library(tidyverse)
library(plotrix)

# Datensatz einlesen
chartsger <- read_csv2("chartsger.CSV", na = "-77")

# Häufigkeitstabelle (Folie 5)
chartsger %>%
  count(haircol, sort = TRUE) %>%
  mutate(percent = 100*n/sum(n), cumulated_percent = cumsum(percent))

# Spannweite (Folie 7)
chartsger %>%
  drop_na(byear) %>%
  summarise("range_byear" = range(byear))
chartsger %>%
  drop_na(byear) %>%
  summarise("range_byear" = range(byear) %>% diff())

# Quartile (Folie 8)
quantile(chartsger$crank)

# Interquartilsabstand (Folie 8)
IQR(chartsger$crank)

# Varianz (Folie 9)
var(chartsger$yt_clicks)

# Standardabweichung (Folie 9)
sd(chartsger$yt_clicks)

# Datensatz öffnen
data("starwars")

# Variable birth_year in age umbenennen und gender in Zahlenwerte umcodieren
starwars_work <- starwars %>% 
  rename(age = birth_year) %>% 
  mutate(gender = case_match(gender, 
                             "feminine" ~ 1,
                             "masculine" ~ 0))

# Deskription Alter und Geschlecht
## Deskription Geschlecht
summary_gender <- starwars_work %>% 
  drop_na(gender) %>% 
  summarise(`N (gültige Einträge)` = nrow(starwars_work %>% drop_na(gender)),
            Minimum = min(gender),
            Maximum = max(gender),
            Spannweite = range(gender) %>% diff(),
            Mittelwert = round(mean(gender), digits = 3),
            Standardfehler = round(std.error(gender), digits = 3),
            Standardabweichung = round(sd(gender), digits = 3),
            Varianz = round(var(gender), digits = 3))
## Deskription Alter
summary_age <- starwars_work %>% 
  drop_na(age) %>% 
  summarise(`N (gültige Einträge)` = nrow(starwars_work %>% drop_na(age)),
            Minimum = min(age),
            Maximum = max(age),
            Spannweite = range(age) %>% diff(),
            Mittelwert = round(mean(age), digits = 3),
            Standardfehler = round(std.error(age), digits = 3),
            Standardabweichung = round(sd(age), digits = 3),
            Varianz = round(var(age), digits = 3))
## Zusammenfassung der Datensätze
summary <- summary_age %>% 
  bind_rows(summary_gender)
## Teildatensatz 1 (Folie  11)
summary_1 <- summary %>% 
  mutate(Variable = c("Alter", "Geschlecht")) %>% 
  select(Variable, `N (gültige Einträge)`, Minimum, Maximum, Spannweite)
summary_1
## Teildatensatz 2 (Folie 11)
summary_2 <- summary %>% 
  mutate(Variable = c("Alter", "Geschlecht")) %>% 
  select(Variable, Mittelwert, Standardfehler, Standardabweichung, Varianz)
summary_2

# Balkendiagramm Schritt für Schritt (Folie 13)
chartsger %>%
  drop_na(haircol) %>% 
  ggplot(aes(x = factor(haircol, levels = c("3", "2", "1", "4", "6", "7", "5"), labels = c("schwarz", "braun", "blond", "rot", "sonstige", "keine", "grau")))) +
  geom_bar(fill = "tomato", color = "black") +
  xlab("Haarfarbe") +
  ylab("Häufigkeit") +
  labs(caption = "N = 98") +
  ggtitle("Haarfarbenverteilung in den Top100-Charts") +
  theme(plot.title = element_text(size = 14),
        axis.line = element_line(color = "black")) +
  scale_y_continuous(breaks = seq(0, 45, 5))

# Kreisdiagramm (Folie 15)
gender_frq <- as.data.frame(table(chartsger$gender))
gender_frq$percent <- paste(round(gender_frq$Freq/sum(gender_frq$Freq)*100), "%")
gender_frq <- gender_frq %>%
  arrange(desc(Freq))
gender_frq$Var1 = factor(gender_frq$Var1, levels = c("3", "2", "1"))

gender_frq %>%
  ggplot(aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", color = "black") +
  coord_polar("y") +
  theme_void() +
  scale_fill_manual(breaks = c("1", "2", "3"), labels = c("männlich", "weiblich", "divers"), values = c("lightblue", "tomato", "palegreen")) +
  geom_label(aes(label = percent),
             position = position_stack(vjust = 0.4),
             label.size = 0,
             size = 4,
             show.legend = FALSE) +
  labs(fill = "Geschlecht") +
  theme(legend.text = element_text(size = 11),
        legend.title = element_text(size = 12),
        plot.title = element_text(size = 14)) +
  ggtitle("Geschlechterverteilung in den Top100-Charts") +
  labs(caption = "N = 100")

# Balkendiagramm (Folie 16)
chartsger %>%
  drop_na(haircol) %>% 
  ggplot(aes(x = factor(haircol, levels = c("3", "2", "1", "4", "6", "7", "5"), labels = c("schwarz", "braun", "blond", "rot", "sonstige", "keine", "grau")))) +
  geom_bar(fill = "tomato", color = "black") +
  xlab("Haarfarbe") +
  ylab("Häufigkeit") +
  labs(caption = "N = 98") +
  ggtitle("Haarfarbenverteilung in den Top100-Charts") +
  theme(plot.title = element_text(size = 14),
        axis.line = element_line(color = "black")) +
  scale_y_continuous(breaks = seq(0, 45, 5))

# Histogramm (Folie 17)
chartsger %>%
  select(height) %>%
  drop_na() %>%
  ggplot(aes(x = height)) +
  geom_histogram(binwidth = 5, fill = "tomato", color = "black") +
  scale_x_continuous(n.breaks = 10) +
  xlab("Größe") +
  ylab("Häufigkeit") +
  labs(caption = "N = 68") +
  ggtitle("Größenverteilung in den Top100-Charts") +
  theme(plot.title = element_text(size = 14), axis.line = element_line(color = "black"))

# Verteilungskurve (Folie 18)
chartsger %>%
  ggplot(aes(x = height)) +
  geom_density(color = "tomato") +
  scale_x_continuous(breaks = c(160, 165, 170, 175, 180, 185, 190, 195, 200)) +
  scale_y_continuous(breaks = c(0.00, 0.025, 0.05, 0.075)) +
  ylab("Häufigkeitsdichte") +
  xlab("Größe in cm") +
  labs(caption = "N = 68") +
  ggtitle("Größenverteilung in den Top100-Charts") +
  theme(plot.title = element_text(size = 14), axis.line = element_line(color = "black"))

# Boxplot (Folie 21)
chartsger %>%
  ggplot(aes(y = height)) +
  stat_boxplot(geom = "errorbar", width = 0.1) +
  geom_boxplot(fill = "tomato") +
  scale_x_discrete() +
  scale_y_continuous(breaks = c(160, 165, 170, 175, 180, 185, 190, 195, 200)) +
  labs(y = "Größe in cm", caption = "N = 68") +
  ggtitle("Größenverteilung in den Top100-Charts") +
  theme(plot.title = element_text(size = 14), axis.line = element_line(color = "black"))

# Liniendiagramm (Folie 22)
## neuer Datensatz: Verlauf Chartsplatzierung "Flowers" (Miley Cyrus) vom 20.01.23-12.05.23
date <- c("2023-01-13", "2023-01-20", "2023-01-27", "2023-02-03", "2023-02-10", "2023-02-17", "2023-02-24", "2023-03-03", "2023-03-10", "2023-03-17", "2023-03-24", "2023-03-31", "2023-04-07", "2023-04-14", "2023-04-21", "2023-04-28", "2023-05-05", "2023-05-12")
date <- as.Date(date)
Flowers <- c(NA, 2, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 4, 3, 3, 4, 3, 7)
Sie_weiss <- c(1, 1, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 8, 9, 13, 18, 22, 28)
Unholy <- c(6, 11, 12, 7, 5, 10, 8, 10, 14, 14, 19, 23, 27, 27, 34, 39, 42, 45)
Eyes_Closed <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 13, 29, 29, 30, 38, 43, 21)
Mockingbird <- c(13, 16, 23, 17, 18, 21, 23, 19, 24, 24, 21, 5, 6, 5, 6, 10, 13, 17)
chartsger_progress <- data.frame(date, Flowers, Sie_weiss, Unholy, Eyes_Closed, Mockingbird)

chartsger_progress %>%
  ggplot(aes(x = date)) +
  geom_point(aes(y = Flowers, color = "1"), size = 2) + geom_line(aes(y = Flowers, color = "1")) +
  geom_point(aes(y = Sie_weiss, color = "2"), size = 2) + geom_line(aes(y = Sie_weiss, color = "2")) +
  geom_point(aes(y = Unholy, color = "3"), size = 2) + geom_line(aes(y = Unholy, color = "3")) +
  geom_point(aes(y = Eyes_Closed, color = "4"), size = 2) + geom_line(aes(y = Eyes_Closed, color = "4")) +
  geom_point(aes(y = Mockingbird, color = "5"), size = 2) + geom_line(aes(y = Mockingbird, color = "5")) +
  scale_color_manual(values = c("tomato", "purple", "palegreen", "yellow", "grey"), labels = c("Flowers", "Sie weiß", "Unholy", "Eyes Closed", "Mockingbird")) +
  ylab("Chartplatzierung") +
  xlab("Datum") +
  ggtitle("Entwicklung der Chartplatzierungen") +
  theme(plot.title = element_text(size = 14), axis.line = element_line(color = "black"), axis.text.x = element_text(size = 10, angle = 90, hjust = 1, vjust = 0.25)) +
  scale_y_reverse(breaks = c(1, 5, 10, 15, 20, 25, 30, 35, 40, 45)) +
  scale_x_date(breaks = function(x) seq.Date(from = min(x), to = max(x), by = "1 week"), date_labels = "%d %b")

# Scatterplot 1 (Folie 23)
chartsger %>%
  ggplot(aes(x = byear, y = yt_sub, color = as.factor(gender))) +
  geom_point(size = 1) +
  ggtitle("Zusammenhang zwischen Geburtsjahr und YouTube-Abos\nnach Geschlecht") +
  scale_color_manual(values = c("lightblue", "tomato", "palegreen"), labels = c("männlich", "weiblich", "divers")) +
  scale_y_continuous(breaks = c(0, 10000, 20000, 30000, 40000, 50000)) +
  scale_x_continuous(breaks = c(1940, 1950, 1960, 1970, 1980, 1990, 2000)) +
  ylab("YouTube-Abos in Tausend") +
  xlab("Geburtsjahr") +
  labs(color = "Geschlecht", caption = "N = 70") +
  theme(plot.title = element_text(size = 12))

# Scatterplot (Faceting) (Folie 23)
chartsger$gender_fact <- factor(chartsger$gender, levels = c("1", "2", "3"), labels = c("männlich", "weiblich", "divers"))

chartsger %>%
  ggplot(aes(x = byear, y = yt_sub)) +
  geom_point(size = 1) +
  facet_wrap(~ gender_fact) +
  ggtitle("Zusammenhang zwischen Geburtsjahr und YouTube-Abos\nnach Geschlecht") +
  scale_x_continuous(breaks = c(1940, 1950, 1960, 1970, 1980, 1990, 2000)) +
  scale_y_continuous(breaks = c(0, 10000, 20000, 30000, 40000, 50000)) +
  ylab("") +
  xlab("Geburtsjahr") +
  labs(caption = "N = 70") +
  theme(plot.title = element_text(size = 12), axis.text.x = element_text(angle = 90, vjust = 0.5))

# Verteilung Körpergröße (Folie 25)
starwars_work %>% 
  drop_na(height) %>%
  ggplot(aes(x = height)) +
  geom_density() +
  scale_x_continuous(breaks = seq(0, 280, 20), limits = c(0, 280)) +
  labs(caption = "N = 81") +
  xlab("Körpergröße in cm") +
  theme_minimal() +
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.title.y = element_blank())