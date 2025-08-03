#Laden eines eigenen Datensatzes
load("C:/Users/langm/StatistikMat/GueW_Auswahl.RData")

#Aufgabe 3 - Überblick gewinnen
head(GueW)
summary(GueW)


#Aufgabe 4 - $-Befehl für einzelne Spalten
summary(GueW$Alter)


summary(GueW)


#Aufgabe 5 - Anwendung "select" aus dplyr
#Laden von dplyr
library(dplyr)
GueW_Selected <- GueW %>%
  select(LfdNr, Geburtsjahr, Alter)

#Bonusaufgabe mit verschiedenen dplyr-Funktionen
GueW1 <- GueW %>% 
  filter (Geburtsjahr >= 1990, appkomm == 1) %>% 
  mutate(Alter_heute = 2021 - Geburtsjahr) %>% 
  arrange(Alter_heute) %>%
  select(LfdNr, Geburtsjahr, Alter, Alter_heute)
 

