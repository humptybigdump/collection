if(rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

# Packages laden
library(tidyverse)

# Kreuztabelle (Folie 7)
set.seed(1)
id <- c(1:1000)
color <- sample(c("red", "blue", "green", "white"), size = 1000, replace = TRUE)
gender <- sample(c("male", "female", "non-binary"), size = 1000, replace = TRUE)
fish_data <- data.frame(id, color, gender)
crosstable_fish <- table(fish_data$color, fish_data$gender)
crosstable_fish

# Spaltensumme (Folie 8)
margin.table(crosstable_fish, 2)

# Zeilensumme (Folie 8)
margin.table(crosstable_fish, 1)

# relative Häufigkeiten (Folie 8)
prop.table(crosstable_fish)

# Spaltenprozente (Folie 9)
prop.table(crosstable_fish, 2)

# Zeilenprozente (Folie 9)
prop.table(crosstable_fish, 1)

# Chi2-Test (Folie 12)
chi2_test <- chisq.test(fish_data$color, fish_data$gender, correct = FALSE)
chi2_test

# Indifferenztabelle (Folie 12)
chi2_test$expected

# eindimensionaler Chi2-Test (Folie 14)
chisq.test(table(fish_data$gender))

# Kreuztabelle Übungsaufgabe (Folie 16)
set.seed(2)
roboter <- sample(c("ja", "nein"), size = 1000, replace = TRUE)
bild <- sample(c("Roboter", "Mensch", "Computer", "Sonstige"), size = 1000, replace = TRUE)
ki_bilder <- data.frame(id, roboter, bild)
crosstable_ki <- table(ki_bilder$bild, ki_bilder$roboter)
crosstable_ki

# Zeilenprozente Übungsaufgabe (Folie 16)
prop.table(crosstable_ki, 1)

# Indifferenztabelle Übungsaufgabe (Folie 17)
chi2_ki <- chisq.test(ki_bilder$bild, ki_bilder$roboter)
chi2_ki$expected