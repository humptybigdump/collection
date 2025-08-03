library(dplyr)
library(sjmisc)
library(ggplot2)

# Datensatz erstellen
color <- c("green", "green", "blue", "blue", "blue", "green", "blue", "green", "green", "blue", "blue", "blue", "green", "blue", "green", "green", "blue", "blue", "green", "green")
sex <- c("female", "female", "male", "female", "male", "male", "female", "male", "male", "female", "male", "female", "male", "male", "male", "female", "female", "male", "female", "male")
size <- c(27.5, 26.0, 24.3, 27.0, 30.0, 28.5, 28.6, 29.7, 24.4, 23.2, 27.5, 27.6, 26.4, 28.0, 27.5, 27.8, 31.2, 29.6, 26.7, 27.5)
weight <- c(300, 312, 298, 350, 287, 298, 301, 301, 309, 298, 290, 287, 311, 325, 275, 312, 285, 255, 304, 320)
age <- c(6, 4, 10, 12, 11, 5, 7, 9, 13, 15, 12, 10, 11, 16, 7, 5, 6, 15, 13, 12)
fish <- data.frame(color, sex, size, weight, age)

# Haeufigkeitstabellen sjmisc (Folie 6)
frq(fish$color)
frq(fish$sex)

# Haeufigkeitstabellen dplyr (Folie 7)
fish %>%
  count(color, sort = TRUE) %>%
  mutate(percent = 100*n/sum(n), cumulated_percent = cumsum(percent))
fish %>%
  count(sex, sort = TRUE) %>%
  mutate(percent = 100*n/sum(n), cumulated_percent = cumsum(percent))

# Kontingenztabelle (Folie 8)
kreuztabelle <- table(fish$color, fish$sex)
print(kreuztabelle)
# Spaltensumme (Folie 9)
margin.table(kreuztabelle, 2)
# Zeilensumme (Folie 9)
margin.table(kreuztabelle, 1)
# Kontingenztabelle relative Haeufigkeiten (Folie 9)
prop.table(kreuztabelle)
# Kontingenztabelle Spaltenprozente (Folie 10)
prop.table(kreuztabelle, 2)
# Kontingenztabelle Zeilenprozente (Folie 10)
prop.table(kreuztabelle, 1)

# Chi2-Test (Folie 13)
chisq.test(fish$color, fish$sex, correct = FALSE)

# Fisher's Test (Folie 14)
fisher.test(fish$color, fish$sex)

# Eindimensionaler Chi2-Test (Folie 15)
chisq.test(table(fish$sex))

# Cramers V (Folie 16)
library(DescTools)
CramerV(x = fish$color, y = fish$sex)

# Haeufigkeitstabelle Top10 - Aufgabe 1 (Folie 17)
  # Datensatz
  id <- c(1:10)
  gender <- c(1, 1, 1, 1, 1, 1, 1, 2, 1, 1)    
  haircol <- c(1, 5, 3, 3, 3, 2, 3, 1, 2, 1)  
  top10 <- data.frame(id, gender, haircol)
  # Haeufigkeitstabellen
  frq(top10$gender)
  frq(top10$haircol)
  