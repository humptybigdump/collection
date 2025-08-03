if(rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

# Packages laden
library(tidyverse)
library(DescTools)

# F-Verteilung (Folie 5)
x <- seq(-3, 3, by = 0.005)
F_verteilung <- data.frame(x)
F_verteilung %>% 
  ggplot(aes(x = x)) +
  xlim(0, 4) +
  stat_function(fun = df, args = list(df1 = 9, df2 = 20), color = "black") +
  ylab("y") +
  theme_minimal()

# ANOVA (Folie 8)
rtnorm <- function(n, mean, sd, a = -Inf, b = Inf){
  qnorm(runif(n, pnorm(a, mean, sd), pnorm(b, mean, sd)), mean, sd)
}
set.seed(1)
fish_weight <- rtnorm(n = 1000, mean = 500, sd = 75.4, a = 0, b = 1000)
fish_id <- c(1:1000)
fish_color <- sample(1:5, 1000, replace = TRUE)
fish_data <- data.frame(fish_id, fish_color, fish_weight)
anova <- aov(fish_weight ~ fish_color, data = fish_data)
summary(anova)
shapiro.test(fish_data$fish_weight)
LeveneTest(fish_data$fish_weight, fish_data$fish_color)

# Post-hoc-Test (Folie 9)
pairwise.t.test(fish_data$fish_weight, fish_data$fish_color, p.adjust.method = "bonferroni")

# Datensatz für Übungsaufgabe in R
person_id <- c(1:74475)
set.seed(2)
fan <- sample(0:1, 74475, replace = TRUE)
  # 0 = nein, 1 = ja 
fernseh_min_monat <- rtnorm(n = 74475, mean = 5400, sd = 34.6, a = 0, b = 43800)
em_min <- rtnorm(n = 74475, mean = 400, sd = 45, a = 0, b = 5040)
lieblingsnation <- sample(1:24, 74475, replace = TRUE)
em_data <- data.frame(person_id, fan, fernseh_min_monat, em_min, lieblingsnation)
write_csv(em_data, "em_fernsehminuten_datensatz_gross.csv")