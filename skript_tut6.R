if(rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

# Packages laden
library(tidyverse)
library(DescTools)

# t-Verteilung (Folie 4)
x <- seq(-3, 3, by = 0.005)
t_verteilung <- data.frame(x)
t_verteilung %>% 
  ggplot(aes(x = x)) +
  xlim(-3, 3) +
  stat_function(fun = dt, args = list(df = 1), color = "black") +
  stat_function(fun = dt, args = list(df = 5), color = "brown1") +
  stat_function(fun = dt, args = list(df = 100), color = "lightblue") +
  annotate(geom = "text", x = 0, y = 0.39, label = "df = 100", color = "lightblue", size = 2) +
  annotate(geom = "text", x = 0, y = 0.27, label = "df = 1", color = "black", size = 2) +
  annotate(geom = "text", x = 0, y = 0.35, label = "df = 5", color = "brown1", size = 2) +
  xlab("x") +
  ylab("y") +
  theme_minimal()

# t-Test für unabhängige Stichproben (Folie 9)
set.seed(1)
fish_weight <- sample(50:1000, 1000, replace = TRUE)
fish_weight2 <- sample(50:1000, 1000, replace = TRUE)
fish_id <- c(1:1000)
fish_gender <- sample(1:2, 1000, replace = TRUE)
fish_data <- data.frame(fish_id, fish_gender, fish_weight, fish_weight2)
shapiro.test(fish_data$fish_weight)
LeveneTest(fish_data$fish_weight, fish_data$fish_gender)
t.test(fish_data$fish_weight, fish_data$fish_gender, alternative = "greater")

# t-Test für abhängige Stichproben (Folie 12)
shapiro.test(fish_data$fish_weight2)
LeveneTest(fish_data$fish_weight, fish_data$fish_weight2)
t.test(fish_data$fish_weight, fish_data$fish_weight2, alternative = "greater", paired = TRUE)

# Datensatz für Übungsaufgabe in R
rtnorm <- function(n, mean, sd, a = -Inf, b = Inf){
  qnorm(runif(n, pnorm(a, mean, sd), pnorm(b, mean, sd)), mean, sd)
}
person_id <- c(1:500)
set.seed(2)
fan <- sample(0:1, 500, replace = TRUE)
  # 0 = nein, 1 = ja 
fernseh_min_monat <- rtnorm(n = 500, mean = 5400, sd = 34.6, a = 0, b = 43800)
em_min <- rtnorm(n = 500, mean = 400, sd = 45, a = 0, b = 5040)
lieblingsnation <- sample(1:24, 500, replace = TRUE)
em_fernsehen <- data.frame(person_id, fan, fernseh_min_monat, em_min, lieblingsnation)
write_csv(em_fernsehen, "em_fernsehminuten_datensatz.csv")
