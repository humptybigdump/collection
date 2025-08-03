#Anwendung Wissen aus DataCamp Assignment
#Namensgebung der Kursstufen
k1 <- 103
k2 <- 89

#Oberstufenschüler gesamt
summe_schüler <- k1 + k2
k1 + k2

#15% der Schüler sind versetzungsgefährdet: wie viele Schüler sind das?
(k1+k2) * 0.15
#oder
(k1+k2) * (15/100)



install.packages("ggplot2")


#Datensätze und Packages laden
install.packages("dplyr")
library("dplyr")

install.packages("gapminder")
library("gapminder")
data("gapminder")


#Aufgaben mit gapminder 
class(gapminder)

str(gapminder)

head(gapminder)


#Aufgaben mit dplyr

gapminder %>% filter(country == "Germany")

gapminder_2002 <-
gapminder %>% filter(year == 2002)

gapminder_ger <- gapminder %>% filter(country == "Germany")

median(gapminder$lifeExp)


#Testing für TUT 3 rund um GueW
remove(GueW)
load("C:/Users/langm/StatistikMat/GueW_Auswahl.RData")
summary(GueW)
head(GueW)

GueW %>%
  select(LfdNr, Geburtsjahr, Alter)

GueW %>% filter (Geburtsjahr >= "1990", appkomm == 1) %>% mutate(Alter_heute = 2021 - Geburtsjahr) %>% arrange(Alter_heute) 

GueW %>% group_by(Geschlecht) %>%
  summarize(meanKnifflig = mean(NfC_2))
