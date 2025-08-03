library(tidyverse)
library(readr)

# Chi2 mit xtabs (Folie 4)
xtabs(~ color + sex, data = fish) %>%
  summary()

# Arten von Zusammenhängen (Folie 5)
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
    scale_y_continuous("abhängige Variable", 1:10) +
    scale_x_continuous("unabhängige Variable", 1:10)

  # Negativer Zusammenhang
  data2 %>%
    ggplot(aes(x = x, y = y2)) +
    geom_point() +
    geom_line() +
    scale_y_continuous("abhängige Variable", 1:10) +
    scale_x_continuous("unabhängige Variable", 1:10)

# Nicht-lineare Zusammenhaenge (Folie 8)
y3 <- c(0, 2, 4, 6, 7.5, 9, 10, 10.5, 11, 11.25, 11.5)
data3 <- data.frame(x, y3)
data3 %>%
  ggplot(aes(x = x, y = y3)) +
  geom_point() +
  geom_smooth(color = "black", linewidth = 0.5, se = FALSE) +
  scale_y_continuous("abhängige Variable", 0:12) +
  scale_x_continuous("unabhängige Variable", 0:10)

y4 <- c(0, 0.25, 0.5, 1, 1.5, 2, 4, 6, 6.5, 6.75, 7)
data4 <- data.frame(x, y4)
data4 %>%
  ggplot(aes(x = x, y = y4)) +
  geom_point() +
  geom_smooth(color = "black", linewidth = 0.5, se = FALSE) +
  scale_y_continuous("abhängige Variable", 0:8) +
  scale_x_continuous("unabhängige Variable", 0:10)

# Scatterplot (Folie 9)
chartsger <- read_csv2("C:/Users/ueneu/Documents/Tutorium/Vorbereitung/Datensätze/chartsger.CSV", trim_ws = TRUE, na = "-77")
Analyse_chartsger <- chartsger
  # Ausreißer markieren
  Analyse_chartsger$outlier <- ifelse(Analyse_chartsger$yt_sub > 40000 | Analyse_chartsger$yt_likes > 7500, "highlight", "normal")
  mycolours <- c("highlight" = "red", "normal" = "black")
  Analyse_chartsger %>%
    ggplot(aes(x = yt_sub/1000, y = yt_likes/1000)) +
    geom_point(aes(colour = outlier)) +
    scale_color_manual(values = mycolours) +
    ggtitle("YouTube-Abos und -Likes") +
    xlab("YouTube-Abos in Mio.") +
    ylab("YouTube-Likes in Mio.") +
    theme(legend.position = "none")

# Scatterplot mit Mittelwerten (Folie 12)
mean_yt_sub <- Analyse_chartsger %>%
  summarise(mean_yt_sub = mean(yt_sub/1000, na.rm = TRUE))
mean_yt_likes <- Analyse_chartsger %>%
  summarise(mean_yt_likes = mean(yt_likes/1000))
Analyse_chartsger %>%
  ggplot(aes(x = yt_sub/1000, y = yt_likes/1000)) +
  geom_point() +
  ggtitle("YouTube-Abos und -Likes") +
  ylab("YouTube-Likes in Mio.") +
  xlab("YouTube-Abos in Mio.") +
  geom_vline(data = mean_yt_sub, aes(xintercept = mean_yt_sub), linetype = "dashed", color = "turquoise", linewidth = 1) +
  geom_hline(data = mean_yt_likes, aes(yintercept = mean_yt_likes), linetype = "dashed", color = "turquoise", linewidth = 1)

# Kovarianz (Folie 11)
cov(Analyse_chartsger$yt_sub/1000, Analyse_chartsger$yt_likes/1000, use = "complete.obs")

# Pearsons r (Folie 13)  
cor(Analyse_chartsger$yt_sub, Analyse_chartsger$yt_likes, use = "complete.obs")

# t-Test für Pearsons r (Folie 14)  
cor.test(Analyse_chartsger$yt_sub, Analyse_chartsger$yt_likes, use = "complete.obs") 

# Korrelationsmatrix (Folie 15)
  install.packages("correlation")
  library(correlation)
  matrix_chartsger <- Analyse_chartsger %>%
    select(-id, -gender, -byear, -haircol, -crank, -britcharts, -outlier)
  summary(correlation(matrix_chartsger))
  cor(matrix_chartsger, use = "complete.obs")
  
# Kendalls tau (Folie 17)
cor.test(Analyse_chartsger$byear, Analyse_chartsger$crank, use = "complete.obs", method = "kendall")

# Spearmans rho (Folie 18)  
cor.test(Analyse_chartsger$byear, Analyse_chartsger$crank, use = "complete.obs", method = "spearman")  
  