if(rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

# Packages laden
library(tidyverse)
library(car)
library(patchwork)

# Datensatz für Beispiel
rtnorm <- function(n, mean, sd, a = -Inf, b = Inf){
  qnorm(runif(n, pnorm(a, mean, sd), pnorm(b, mean, sd)), mean, sd)
}
set.seed(1)
id <- c(1:100)
fish_length <- rtnorm(100, mean = 30, sd = 3.7, a = 0.1, b = 500)
fish_weight <- rtnorm(100, mean = 1000, sd = 67.4, a = 0.1, b = 680000)
fish_species <- sample(c(0, 1), size = 100, replace = TRUE)
fish_distance <- rtnorm(100, mean = 10, sd = 4, a = 0, b = 100)
fish_data <- data.frame(id, fish_species, fish_length, fish_distance, fish_weight)

# Regressionsmodell
fish_regression <- lm(fish_weight ~ fish_species + fish_length + fish_distance, data = fish_data)
summary(fish_regression)

# partielle Regressionsdiagramme (Folie 4)
distance <- fish_data %>% 
  ggplot(aes(x = fish_distance, y = fish_weight)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal()
length <- fish_data %>% 
  ggplot(aes(x = fish_length, y = fish_weight)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  theme_minimal()
species <- fish_data %>% 
  ggplot(aes(x = fish_species, y = fish_weight)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "green") +
  theme_minimal()
distance +
  length +
  species +
  plot_layout(ncol = 2)

# Exogenität (Folie 5)
fish_data %>% 
  select(fish_species, fish_length, fish_distance) %>% 
  cor()
vif(fish_regression)

# keine Autokorrelation (Folie 5)
durbinWatsonTest(fish_regression)

# Homoskedastizität (Folie 6)
shapiro.test(residuals(fish_regression))
fish_data %>% 
  ggplot(aes(x = fish_weight, y = scale(residuals(fish_regression)))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Masse") +
  ylab("Residuen") +
  theme_minimal()

# Ausreißer (Folie 7)
hatvalues(fish_regression)
scale(residuals(fish_regression))

# multiple Regression (Folie 16)
summary(fish_regression)

# Übungsaufgabe (Folie 21)
influencer_id <- c(1:50)
set.seed(2)
durchschnittliche_videolaenge <- rtnorm(50, mean = 10, sd = 1.1, a = 1, b = 90)
geschlecht <- sample(c(0, 1), size = 50, replace = TRUE)
zeichenzahl_videobeschreibung <- rtnorm(50, mean = 89, sd = 20.4, a = 0, b = 250)
follower <- rtnorm(50, mean = 1247, sd = 320.2, a = 0, b = 1000000)
influencer <- data.frame(geschlecht, durchschnittliche_videolaenge, zeichenzahl_videobeschreibung, follower)

regression_influencer <- lm(follower ~ geschlecht + durchschnittliche_videolaenge + zeichenzahl_videobeschreibung, data = influencer)
summary(regression_influencer)
