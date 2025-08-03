library("AER")
library("sjmisc")
library("dplyr")
library("DescTools")
library("haven")
library("ggplot2")
library("plotrix")

data("gapminder")
gap2002 <- gapminder %>% filter(year == 2002)

mean(gap2002$lifeExp)
#ergibt 65.7

#Standardfehler
#entweder
sd(gap2002$lifeExp)
sqrt(142)
sd(gap2002$lifeExp)/sqrt(142)
#oder
std.error(gap2002$lifeExp)
#SF ist 1.0305


# Konfidenzintervall Alter
(65.69492+(1.0305*1.96)) 
(65.69492-(1.0305*1.96))

#oder

mean(gap2002$lifeExp)+std.error(gap2002$lifeExp)*1.96
mean(gap2002$lifeExp)-std.error(gap2002$lifeExp)*1.96

# 63.67 - 67.71 ist das 95%ige Konfidenzintervall --> 
# mit 95%iger Sicherheit lag die weltweite Lebenserwartung im Jahr 2002 im Intervall [63.67; 67.71] Jahren
shapiro.test()