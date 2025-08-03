# Lösungen zu 3. VL Univariate Analyse

library(sjmisc)
# Aufgabe 1.
# Berechnen Sie Mean, SD und Spannweite für die untenstehende Zahlenfolge. 
# Vergleichen Sie mit dem Beispiel A (Homogene Gruppe). Was fällt Ihnen auf?
GruppeC <- c(18,19,19,20,20,21,22,25,80)
GruppeA <- c(18,19,19,20,20,21,22,25)
mean(GruppeA)
mean(GruppeC)
sd(GruppeA)
sd(GruppeC)
max(GruppeA)-min(GruppeA)
max(GruppeC)-min(GruppeC)

# Aufgabe 2.
# Twenty-one heavy smokers were put on a treadmill at the fastest setting.
# The time in seconds was measured until they fell off from exhaustion: 
# Compute the mode, median, upper and lower quartiles, range and interquartilerange
smokers <- c(18, 16, 18, 24, 23, 22, 22, 23, 26, 29, 32, 34, 34, 36, 36, 43, 42, 49, 46, 46, 57)
frq(smokers, sort.frq ="desc")
median(smokers)
max(smokers)-min(smokers)
summary(smokers)
IQR(smokers)
