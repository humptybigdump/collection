if(rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

# Packages laden
library(tidyverse)

# positiver Zusammenhang (Folie 4)
x_positiv <- seq(1, 100, 1)
y <- seq(1, 100, 1)
positiver_zusammenhang <- data.frame(x_positiv, y)

positiver_zusammenhang %>% 
  ggplot(aes(x = x_positiv, y = y)) +
  geom_point() +
  xlab("unabhängige Variable") +
  ylab("abhängige Variable") +
  theme_minimal()

# negativer Zusammenhang (Folie 4)
x_negativ <- seq(100, 1, -1)
negativer_zusammenhang <- data.frame(x_negativ, y)

negativer_zusammenhang %>% 
  ggplot(aes(x = x_negativ, y = y)) +
  geom_point() +
  xlab("unabhängige Variable") +
  ylab("abhängige Variable") +
  theme_minimal()

# nicht-lineare Zusammenhänge (Folie 7)
y_exponentiell <- c(0, 5, 10, 15, 17, 19, 20:25, 26, 26.5, 27, 27.5, 28, 28.25, 28.5, 28.75, 29, 29.25, 29.5, 29.75, 30)
x_exponentiell <- c(0:24)
exponentieller_zusammenhang <- data.frame(x_exponentiell, y_exponentiell)

exponentieller_zusammenhang %>% 
  ggplot(aes(x = x_exponentiell, y = y_exponentiell)) +
  geom_point() +
  xlab("unabhängige Variable") +
  ylab("abhängige Variable") +
  theme_minimal()

# S-Kurve
y_s <- c(0, 0.25, 0.5, 0.75, 1, 1.5, 2, 3:10, 11.5, 12, 12.5, 13, 13.25, 13.5, 13.75, 14, 14.25, 14.5)
x_s <- c(0:24)
s_kurve <- data.frame(x_s, y_s)

s_kurve %>% 
  ggplot(aes(x = x_s, y = y_s)) +
  geom_point() +
  xlab("unabhängige Variable") +
  ylab("abhängige Variable") +
  theme_minimal()

# Scatterplot (Folie 8)
## Datensatz
set.seed(1)
fish_weight <- sample(0:1000, size = 500, replace = TRUE)
fish_length <- sample(0:50, size = 500, replace = TRUE)
fish_quality <- sample(1:3, size = 500, replace = TRUE)
id <- c(1:500)
fish_data <- data.frame(id, fish_weight, fish_length, fish_quality)

fish_data %>% 
  ggplot(aes(x = fish_length, y = fish_weight)) +
  geom_point() +
  xlab("Länge in cm") +
  ylab("Masse in g") +
  theme_minimal()

# Kovarianz (Folie 10)
cov(fish_data$fish_length, fish_data$fish_weight)

# Scatterplot Kovarianz (Folie 11)
mean_weight <- fish_data %>%
  summarise(mean_weight = mean(fish_weight))
mean_length <- fish_data %>%
  summarise(mean_length = mean(fish_length))
fish_data %>%
  ggplot(aes(x = fish_length, y = fish_weight)) +
  geom_point() +
  ylab("Masse in g") +
  xlab("Länge in cm") +
  geom_vline(data = mean_length, aes(xintercept = mean_length), linetype = "dashed", color = "red", linewidth = 1.25) +
  geom_hline(data = mean_weight, aes(yintercept = mean_weight), linetype = "dashed", color = "red", linewidth = 1.25) +
  theme_minimal()

# t-Test Pearsons r (Folie 13)
cor.test(fish_data$fish_length, fish_data$fish_weight, method = "pearson", alternative = "two.sided")

# OLS-Regression (Folie 20)
## Datensatz
x_regression <- c(0:10)
y_regression <- c(0:10)
y2_regression <- c(0, 1, 2, 3, 7, 5, 6, 7, 8, 9, 10)
y3_regression <- c(0, 1, 2, 3, 20, 5, 6, 7, 8, 9, 10)

data_regression1 <- data.frame(x_regression, y_regression)
data_regression2 <- data.frame(x_regression, y2_regression)
data_regression3 <- data.frame(x_regression, y3_regression)

data_regression1 %>%
  ggplot(aes(x = x_regression, y = y_regression)) +
  geom_smooth(method = "lm", color = "turquoise") +
  geom_point() +
  annotate("text", x = 5*1.1, y = 4, label = "y = 1x", color = "turquoise") +
  xlab("unabhängige Variable") +
  ylab("abhängige Variable") +
  theme_minimal()

data_regression2$outlier <- ifelse(data_regression2$y2_regression == 7 & data_regression2$x_regression == 4, "highlight", "normal")
mycolours <- c("highlight" = "red", "normal" = "black")
data_regression2 %>%
  ggplot(aes(x = x_regression, y = y2_regression)) +
  geom_smooth(method = "lm", se = TRUE, color = "turquoise") +
  geom_point(aes(colour = outlier)) +
  scale_color_manual(values = mycolours) +
  annotate("text", x = 6.5, y = 2, label = "y = 0.4091 + 0.9727x", color = "turquoise") +
  annotate("text", x = 4, y = 7 * 1.1, label = "Ausreißer", size = 3, color = "red") +
  xlab("unabhängige Variable") +
  ylab("abhängige Variable") +
  theme_minimal() +
  theme(legend.position = "none")

data_regression3$outlier <- ifelse(data_regression3$y3_regression == 20 & data_regression3$x_regression == 4, "highlight", "normal")
data_regression3 %>%
  ggplot(aes(x = x_regression, y = y3_regression)) +
  geom_smooth(method = "lm", se = TRUE, color = "turquoise") +
  geom_point(aes(colour = outlier)) +
  scale_color_manual(values = mycolours) +
  annotate("text", x = 6.5, y = 1.5, label = "y = 2.1818 + 0.8545x", color = "turquoise") +
  annotate("text", x = 4, y = 20 * 1.1, label = "Ausreißer", size = 3, color = "red") +
  xlab("unabhängige Variable") +
  ylab("abhängige Variable") +
  theme_minimal() +
  theme(legend.position = "none")

# Regression (Folie 25)
fish_regression <- lm(fish_quality ~ fish_length, data = fish_data)
summary(fish_regression)
