library(dplyr)
library(sjmisc)
library(DescTools)
library(ggplot2)
library(car)
library(readr)

# Datensatz
chartsger <- read_csv2("C:/Users/ueneu/Documents/Tutorium/Vorbereitung/Datensätze/chartsger.CSV", trim_ws = TRUE, na = "-77")
Analyse_chartsger <- chartsger
x <- c(0:10)
y <- c(0:10)
y2 <- c(0, 1, 2, 3, 7, 5, 6, 7, 8, 9, 10)
y3 <- c(0, 1, 2, 3, 20, 5, 6, 7, 8, 9, 10)

data1 <- data.frame(x, y)
data2 <- data.frame(x, y2)
data3 <- data.frame(x, y3)

# OLS-Regression (Folie 7)
lm(y2 ~ x, data = data2)
lm(y ~ x, data = data1)
lm(y3 ~ x, data = data3)
data1 %>%
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm") +
  annotate("text", x = 5*1.1, y = 4, label = "y = 1x", color = "blue")

data2$outlier <- ifelse(data2$y2 == 7 & data2$x == 4, "highlight", "normal")
mycolours <- c("highlight" = "red", "normal" = "black")
data2 %>%
  ggplot(aes(x = x, y = y2)) +
  geom_point(aes(colour = outlier)) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_color_manual(values = mycolours) +
  theme(legend.position = "none") +
  annotate("text", x = 6.5, y = 2, label = "y = 0.4091 + 0.9727x", color = "blue") +
  annotate("text", x = 4, y = 7 * 1.1, label = "Ausreißer", size = 3, color = "red")

data3$outlier <- ifelse(data3$y3 == 20 & data3$x == 4, "highlight", "normal")
data3 %>%
  ggplot(aes(x = x, y = y3)) +
  geom_point(aes(colour = outlier)) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_color_manual(values = mycolours) +
  theme(legend.position = "none") +
  annotate("text", x = 6.5, y = 1.5, label = "y = 2.1818 + 0.8545x", color = "blue") +
  annotate("text", x = 4, y = 20 * 1.1, label = "Ausreißer", size = 3, color = "red")

# einfache Regression (Folie 8)
lm(yt_likes ~ yt_sub, data = Analyse_chartsger)

# Veranschaulichung in ggplot (Folie 9)
Analyse_chartsger %>%
  ggplot(aes(x = yt_sub, y = yt_likes)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  annotate("text", x = 40000, y = 1250, label = "y = 744,37 + 0,085x", color = "blue")

# lineare Regression Geschlecht und Abos (Folie 11)
# Dummy erstellen
Analyse_chartsger <- Analyse_chartsger %>%
  mutate(gender_dummy = ifelse(gender == 2, 1, 0))

lm(yt_sub ~ gender_dummy, data = Analyse_chartsger)

Analyse_chartsger %>%
  ggplot(aes(x = gender_dummy, y = yt_sub)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  annotate("text", x = 0.65, y = 10000, label = "y = 7097 - 1362x", color = "blue") +
  scale_x_continuous(breaks = c(0, 1))

# Detailoutput Regression (Folie 14)
sub_to_likes_reg <- lm(yt_likes ~ yt_sub, data = Analyse_chartsger)
summary(sub_to_likes_reg)
summary(Regression2)
