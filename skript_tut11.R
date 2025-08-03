if(rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

# Packages laden
library(tidyverse)
library(plotrix)
library(DescTools)
library(car)

# Datensatz einlesen und aufbereiten
superhelden <- read_csv("marvel-wikia-data.csv")
superhelden_clean <- superhelden %>% 
  separate(col = name,
           into = c("held", "bezug"),
           sep = "\\(|\\)") %>% 
  select(-c(bezug, urlslug, GSM))
superhelden_clean <- superhelden_clean %>% 
  mutate(ID = str_remove(ID, " Identity"),
         ALIGN = str_remove(ALIGN, " Characters"),
         EYE = str_remove(EYE, " Eyes"),
         HAIR = str_remove(HAIR, " Hair"),
         SEX = str_remove(SEX, " Characters"),
         ALIVE = str_remove(ALIVE, " Characters"))
superhelden_clean <- superhelden_clean %>% 
  rename(identity = ID,
         orientation = ALIGN,
         eye_color = EYE,
         hair_color = HAIR,
         gender = SEX,
         appearances = APPEARANCES,
         first_appearance = Year) %>% 
  select(-`FIRST APPEARANCE`)
superhelden_clean <- superhelden_clean %>% 
  rename(hero = held) %>% 
  mutate(id = row_number()) %>% 
  select(-page_id) %>% 
  select(id, hero:first_appearance)
superhelden_clean <- superhelden_clean %>% 
  rename(alive = ALIVE)
write_excel_csv(superhelden_clean, "superhelden_data.csv")

# Scatterplot Frage Anwendung schwer
rtnorm <- function(n, mean, sd, a = -Inf, b = Inf){
  qnorm(runif(n, pnorm(a, mean, sd), pnorm(b, mean, sd)), mean, sd)
}
set.seed(2)
folgenlaenge <- rtnorm(50, mean = 19.7, sd = 4.6, a = 5.1, b = 60.2)
anzahl_abrufe <- rtnorm(50, mean = 500000, sd = 457097, a = 0, b = 2000000)
data <- data.frame(folgenlaenge, anzahl_abrufe)

data %>% 
  ggplot(aes(x = folgenlaenge, y = anzahl_abrufe / 1000000)) +
  geom_point() +
  ylab("Abrufe in Mio.") +
  xlab("Folgenlänge in Min.") +
  theme_minimal()

# R-Output leicht
cor.test(folgenlaenge, anzahl_abrufe)

# R-Output mittel
set.seed(3)
fangruppen <- sample(c("Riverdale", "How I Met Your Mother", "Maxton Hall"), size = 50, replace = TRUE)
data <- data.frame(data, fangruppen)
anova <- aov(anzahl_abrufe ~ fangruppen, data = data)
summary(anova)

# R-Output schwer
data2 <- data %>% 
  mutate(riverdale_fan = if_else(fangruppen == "Riverdale", 1, 0))
regression <- lm(anzahl_abrufe ~ folgenlaenge + riverdale_fan, data = data2)
summary(regression)

# R-Output sehr schwer
durbinWatsonTest(regression)

# Battle 2
chisq.test(superhelden_clean$orientation, superhelden_clean$alive, correct = TRUE)
CramerV(superhelden_clean$orientation, superhelden_clean$alive)

# Battle 3
superhelden_clean <- superhelden_clean %>% 
  mutate(good_orientation = if_else(orientation == "Good", 1, 0),
         male = if_else(gender == "Male", 1, 0))
regression_superhelden <- lm(appearances ~ first_appearance + male + good_orientation, data = superhelden_clean)
summary(regression_superhelden)

# Battle 4
anova_superhelden <- aov(appearances ~ identity, data = superhelden_clean)
summary(anova_superhelden)
