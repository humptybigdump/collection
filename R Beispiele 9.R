library(dplyr)
library(ggplot2)

x <- c(0:10)
y <- c(0:10)
y2 <- c(10:0)

data1 <- data.frame(x, y)
data2 <- data.frame(x, y2)

# Positiver Zusammenhang
data1 %>%
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  geom_line() +
  scale_y_continuous("y", 1:10) +
  scale_x_continuous("x", 1:10)

# Negativer Zusammenhang
data2 %>%
  ggplot(aes(x = x, y = y2)) +
  geom_point() +
  geom_line() +
  scale_y_continuous("y", 1:10) +
  scale_x_continuous("x", 1:10)

# Nicht-lineare Zusammenhänge
y3 <- c(0, 2, 4, 6, 7.5, 9, 10, 10.5, 11, 11.25, 11.5)
data3 <- data.frame(x, y3)
data3 %>%
  ggplot(aes(x = x, y = y3)) +
  geom_point() +
  geom_line() +
  scale_y_continuous("y", 0:12) +
  scale_x_continuous("x", 0:10)

y4 <- c(0, 0.25, 0.5, 1, 1.5, 2, 4, 6, 6.5, 6.75, 7)
data4 <- data.frame(x, y4)
data4 %>%
  ggplot(aes(x = x, y = y4)) +
  geom_point() +
  geom_line() +
  scale_y_continuous("y", 0:8) +
  scale_x_continuous("x", 0:10)

data("starwars")
starwars %>%
  filter(mass < 1000) %>%
  ggplot(aes(x = height, y = mass)) +
  geom_point() +
  ggtitle("Zusammenhang zwischen Größe und Körpergewicht") +
  ylab("Körpergewicht in kg") +
  xlab("Größe in cm")

mean_height <- starwars %>%
  na.omit() %>%
  summarise(mean_height = mean(height))
mean_mass <- starwars %>%
  na.omit() %>%
  summarise(mean_mass = mean(mass))
starwars %>%
  filter(mass < 1000) %>%
  ggplot(aes(x = height, y = mass)) +
  geom_point() +
  ggtitle("Zusammenhang zwischen Größe und Körpergewicht") +
  ylab("Körpergewicht in kg") +
  xlab("Größe in cm") +
  geom_vline(data = mean_height, aes(xintercept = mean_height)) +
  geom_hline(data = mean_mass, aes(yintercept = mean_mass))

# Korrelation Größe und Masse
  cov(starwars$height, starwars$mass, use = "complete.obs")
  cor(starwars$height, starwars$mass, use = "complete.obs", method = "pearson")
  cor.test(starwars$height, starwars$mass, use = "complete.obs") 
  install.packages("correlation")
  library(correlation)
  summary(correlation(starwars))

# Korrelation Größe und Masse Downgrade
  cor.test(starwars$birth_year, starwars$height, use = "complete.obs", method = "kendall", exact = FALSE)
  cor.test(starwars$birth_year, starwars$height, use = "complete.obs", methode = "spearman", exact = FALSE)
  