if(rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

# Packages laden
library(tidyverse)
library(plotrix)
library(DescTools)

# Punktschätzung (Folie 9)
starwars %>% 
  count(height) %>% 
  summarize(n = n(), mean = mean(height, na.rm = TRUE), sd = sd(height, na.rm = TRUE), se = sd(height, na.rm = TRUE) / sqrt(n))
starwars %>% 
  summarise(mean = mean(height, na.rm = TRUE), se = std.error(height, na.rm = TRUE))

# empirische Verteilung (Folie 10)
starwars %>% 
  ggplot(aes(x = height)) +
  geom_histogram() +
  ylab("Häufigkeit") +
  xlab("Größe in cm") +
  theme_minimal()

# Normalverteilung (Folie 10)
x <- seq(-4, 4, length = 100)
y <- dnorm(x)
normalverteilung <- data.frame(x, y)
normalverteilung %>% 
  ggplot(aes(x = x, y = y)) +
  geom_density(stat = "identity") +
  theme_minimal()

# Intervallschätzung (Folie 13)
starwars %>% 
  summarise(conf_lower = mean(height, na.rm = TRUE) - 1.96 * std.error(height, na.rm = TRUE),
            conf_upper = mean(height, na.rm = TRUE) + 1.96 * std.error(height, na.rm = TRUE))

# Test auf Normalverteilung (Folie 21)
set.seed(1)
fish_weight <- sample(50:1000, 100, replace = TRUE)
fish_id <- c(1:100)
fish_data <- data.frame(fish_id, fish_weight)
shapiro.test(fish_data$fish_weight)

# t-Test für eine Stichprobe (Folie 21)
t.test(fish_data$fish_weight, mu = 250, alternative = "two.sided")

# Übungsaufgabe (Folie 22)
bewertung <- c(4, 5, 6, 7, 1, 5, 3, 3, 2, 7, 1, 0, 5, 5, 7, 6, 5)
t.test(bewertung, mu = 4.2, alternative = "less")
