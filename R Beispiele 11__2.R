library(dplyr)
library(ggplot2)
library(readr)
install.packages("patchwork")
library(patchwork)
install.packages("QuantPsyc")
library(QuantPsyc)

# Datensatz
chartsger <- read_csv2("Dateipfad/chartsger.CSV", trim_ws = TRUE, na = "-77")
Analyse_chartsger <- chartsger

# Zusammenhang YT-Klicks und YT-Likes (Folie 5)
cor.test(Analyse_chartsger$yt_clicks, Analyse_chartsger$yt_likes)
lm(yt_likes ~ yt_clicks, data = Analyse_chartsger) %>% summary()

# Residuen YT-Abos und YT-Klicks (Folie 6)
yt_clicks_clean <- lm(yt_clicks ~ yt_sub, data = Analyse_chartsger)$residuals
yt_likes_clean <- lm(yt_likes ~ yt_sub, data = Analyse_chartsger)$residuals

# bereinigter Zusammenhang (Folie 6)
lm(yt_likes_clean ~ yt_clicks_clean)
cor.test(yt_clicks_clean, yt_likes_clean)

# partielle Korrelation (Folie 7)
  # Korrelation YT-Klicks und YT-Likes (rxy)
  clicks_likes <- cor(Analyse_chartsger$yt_clicks, Analyse_chartsger$yt_likes)
  # Korrelation YT-Klicks und YT-Abos (rxz)
  Analyse_chartsger <- Analyse_chartsger %>%
    filter(!is.na(yt_sub))
  clicks_sub <- cor(Analyse_chartsger$yt_clicks, Analyse_chartsger$yt_sub)
  # Korrelation YT-Likes und YT-Abos (ryz)
  likes_sub <- cor(Analyse_chartsger$yt_likes, Analyse_chartsger$yt_sub)
  # Partielle Korrelation "von Hand"
  clicks_likes_clean <- (clicks_likes - clicks_sub * likes_sub) / (sqrt(1 - clicks_sub^2) * sqrt(1 - likes_sub^2))
  Analyse_chartsger %>%
    summarize(cor_clicks_likes = cor(yt_clicks, yt_likes), cor_clicks_sub = cor(yt_clicks, yt_sub), cor_likes_sub = cor(yt_likes, yt_sub), cor_clean = (cor_clicks_likes - cor_clicks_sub * cor_likes_sub) / (sqrt(1 - cor_clicks_sub^2) * sqrt(1 - cor_likes_sub^2)))

# Geschlechter-Dummy und Altersvariable erstellen
reg_chartsger <- chartsger %>%
  mutate(age = 2023 - byear, gender_dummy = ifelse(gender == 2, 1, 0))

# Multiple Regression (AV: YT-Likes, UVs: YT-Abos, Alter, weibliches Geschlecht) (Folie 11)
yt_likes_multreg <- lm(yt_likes ~ yt_sub + age + gender_dummy, data = reg_chartsger)
yt_likes_multreg

# Visualisierung (Folie 12)
reg_chartsger <- reg_chartsger %>%
  filter(!is.na(yt_sub), !is.na(age), !is.na(gender_dummy))
# Konstanthalten Alter, Geschlecht
reg_chartsger$sub_res <- lm(yt_sub ~ age + gender_dummy, data = reg_chartsger)$residuals
reg_chartsger$likes_sub_res <- lm(sub_res ~ age_res + gender_res, data = reg_chartsger)$residuals
plot1 <- reg_chartsger %>%
  ggplot(aes(x = sub_res, y = likes_sub_res)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "green") +
  xlab("Residuen Abos") +
  ylab("YouTube-Likes") +
  theme(axis.title = element_text(size = 5))
# Konstanthalten Abos, Geschlecht
reg_chartsger$age_res <- lm(age ~ yt_sub + gender_dummy, data = reg_chartsger)$residuals
reg_chartsger$likes_age_res <- lm(age_res ~ sub_res + gender_res, data = reg_chartsger)$residuals
plot2 <- reg_chartsger %>%
  ggplot(aes(x = age_res, y = likes_age_res)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "lightblue") +
  xlab("Residuen Alter") +
  ylab("YouTube-Likes") +
  theme(axis.title = element_text(size = 5))
# Konstanthalten Abos, Alter
reg_chartsger$gender_res <- lm(gender_dummy ~ yt_sub + age, data = reg_chartsger)$residuals
reg_chartsger$likes_gender_res <- lm(gender_res ~ sub_res + age_res, data = reg_chartsger)$residuals
plot3 <- reg_chartsger %>%
  ggplot(aes(x = gender_res, y = likes_gender_res)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "purple") +
  xlab("Residuen Geschlecht") +
  ylab("YouTube-Likes") +
  labs(caption = "1 = weiblich") +
  theme(axis.title = element_text(size = 5), plot.caption = element_text(size = 5))

(plot1 +
    plot2 +
    plot_layout(ncol = 2)) +
  plot3 +
  plot_layout(widths = c(1,1)) +
  plot_annotation(title = "Multiple Regression YouTube-Likes", theme = theme(plot.title = element_text(hjust = 0.5)))

# standardisierte Koeffizienten (Folie 14)
reg_chartsger <- reg_chartsger %>%
  mutate(yt_likes_s = scale(yt_likes), yt_sub_s = scale(yt_sub), age_s = scale(age), gender_dummy_s = scale(gender_dummy))
yt_likes_multreg_s <- lm(yt_likes_s ~ yt_sub_s + age_s + gender_dummy_s, data = reg_chartsger)
yt_likes_multreg_s
lm.beta(yt_likes_multreg)

# Modellgüte und Signifikanz (Folie 18)
summary(yt_likes_multreg)

# Vergleich zweier Regressionsmodelle (Folie 19)
reg_chartsger <- reg_chartsger %>%
  mutate(int_topsong = ifelse(!is.na(britcharts), 1, 0))
yt_likes_multreg2 <- lm(yt_likes ~ yt_sub + age + gender_dummy + int_topsong, data = reg_chartsger)
anova(yt_likes_multreg, yt_likes_multreg2)

# Konfidenzintervalle Koeffizienten (Folie 19)
confint(yt_likes_multreg)