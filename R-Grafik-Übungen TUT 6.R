library("AER")
library("sjmisc")
library("dplyr")
library("DescTools")
library("haven")
library("ggplot2")

#Datensatz laden
Roh_GueW <- read_sav("C:/Users/langm/StatistikMat/Roh_GueW.sav")
glimpse(Roh_GueW)
remove(Roh_GueW)
shapiro.test(Roh_GueW$Alter)

#Histogramme - ganz simple mit R-Basisfunktionen
hist(Roh_GueW$HG_1, main = "Histrogramm GueW")

#Histrogramme mit ggplot2 - schöner und besser anpassbar
ggplot(Roh_GueW, aes(x=HG_1)) +
  geom_histogram(binwidth = 0.5, color = "white", fill = "red") +
  labs(x="Häufigkeit Gespräche mit Freunden", y= "Anzahl") +
  scale_y_continuous(breaks = seq(0, 50, 5))

#Boxplots mit ggplot2
ggplot(Roh_GueW, aes(y = Alter)) +
  stat_boxplot(geom="errorbar", width = 0.1, color = "red") +
  geom_boxplot(fill = "steelblue", color = "red") +
  scale_x_discrete() +
  labs(y="Alter in Jahren")

  