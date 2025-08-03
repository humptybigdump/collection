#Packages installieren
install.packages("tidyverse")
install.packages("readr")

#Packages laden
library(tidyverse)
library(readr)

#Datensatz importieren
chartsger <- read_csv2("C:/Users/ueneu/Documents/Tutorium/Vorbereitung/Datensätze/chartsger.CSV", trim_ws = TRUE, na = "-77")

#Nach maennlichen Beitraegen filtern
male_chartsger <- chartsger %>%
  filter(gender == 1)

#Uebersicht Spotify-Streams
summary(male_chartsger$sfy_streams)

#Grafik
chartsger$gender[chartsger$gender == "1"] <- "male"
chartsger$gender[chartsger$gender == "2"] <- "female"
chartsger$gender[chartsger$gender == "3"] <- "diverse"
chartsger$gender <- as.factor(chartsger$gender)

ggplot(chartsger, aes(x = yt_clicks, y = yt_likes, color = gender, size = yt_sub)) +
  geom_point() +
  ggtitle("YouTube-Likes und -Klicks Top100-Charts") +
  ylab("Likes in Tausend") +
  xlab("Klicks in Mio.") +
  scale_color_discrete(name = "Geschlecht", breaks = c("male", "female", "diverse"), labels = c("männlich", "weiblich", "divers")) +
  scale_size_continuous(name = "Abonnenten in Tausend")

#Hilfe zu Befehlen
?median

#Variablen zuordnen
var1 <- 4