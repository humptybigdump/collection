library(dplyr)
library(car)


# Datensatz
beaklength <- c(18.7, 17.3, 19.4, 18.7, 20.3, 16.6, 20.0, 18.2, 18.8, 19.5, 19.1, 18.2, 19.9, 18.1, 17.9)
beakdepth <- c(50.2, 42.4, 50.6, 45.4, 51.7, 43.2, 50.3, 46.1, 50.2, 51.9, 50.9, 49.2, 53.5, 43.5, 46.5)
wings <- c(198, 181, 193, 188, 194, 187, 197, 178, 202, 206, 196, 195, 205, 202, 192)
weight <- c(3775, 3600, 3800, 3525, 3775, 2900, 3300, 3250, 3800, 3950, 3550, 4400, 4500, 3400, 3500)
sex <- c("Female", "Female", "Male", "Female", "Male", "Female", "Male", "Female", "Male", "Male", "Male", "Male", "Male", "Female", "Female")
penguins <- data.frame(beakdepth, beaklength, sex, weight, wings)


# Datensätze nach Geschlecht
male <- penguins %>%
  filter(sex == "Male")

female <- penguins %>%
  filter(sex == "Female")

# Vorannahmen prüfen
shapiro.test(penguins$weight)
leveneTest(penguins$weight, penguins$sex)

# t-Test für eine Stichprobe
t.test(male$weight, mu = 4000, alternative = "greater")

# t-Test für unabhängige Stichproben
t.test(penguins$weight ~ penguins$sex)

# t-Test für abhängige Stichproben
t.test(penguins$beakdepth, penguins$beaklength, paired = TRUE)

# ANOVA: neuer Datensatz
beaklength <- c(18.7, 17.3, 19.4, 18.7, 20.3, 16.6, 20.0, 18.2, 18.8, 19.5, 19.1, 18.2, 19.9, 18.1, 17.9)
beakdepth <- c(50.2, 42.4, 50.6, 45.4, 51.7, 43.2, 50.3, 46.1, 50.2, 51.9, 50.9, 49.2, 53.5, 43.5, 46.5)
wings <- c(198, 181, 193, 188, 194, 187, 197, 178, 202, 206, 196, 195, 205, 202, 192)
weight <- c(3775, 3600, 3800, 3525, 3775, 2900, 3300, 3250, 3800, 3950, 3550, 4400, 4500, 3400, 3500)
agegroup <- c("kid", "adult", "teen", "senior", "senior", "kid", "kid", "adult", "adult", "adult", "teen", "teen", "senior", "senior", "senior")
penguins_2 <- data.frame(beaklength, beakdepth, wings, weight, agegroup)

# Gruppenbildung
kid <- penguins_2 %>%
  filter(agegroup == "kid")
adult <- penguins_2 %>%
  filter(agegroup == "adult")
teen <- penguins_2 %>%
  filter(agegroup == "teen")
senior <- penguins_2 %>%
  filter(agegroup == "senior")

# Vorannahmen prüfen
shapiro.test(kid$wings)
shapiro.test(adult$wings)
shapiro.test(teen$wings)
shapiro.test(senior$wings)
leveneTest(penguins_2$wings, penguins_2$agegroup)

# ANOVA
anova <- aov(wings ~ agegroup, data = penguins_2)
summary(anova)

oneway.test(wings ~ agegroup, data = penguins_2, var.equal = FALSE)

# Post hoc Test
pairwise.t.test(penguins_2$wings, penguins_2$agegroup, p.adjust.method = "bonferroni")