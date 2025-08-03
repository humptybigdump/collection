# dataset: https://github.com/uoepsy/data/blob/master/ReactionTime.csv
# by university of endingburgh https://uoepsy.github.io
# hint: consider to review course DAPR1 for more details
# also recommended: https://statistik-dresden.de/methodenberatung-welcher-statistische-test-passt-zu-meiner-fragestellung-und-meinen-daten/
# https://www.statistik-nachhilfe.de/ratgeber/datenanalyse-generator/uebersicht

# A reaction time experiment was conducted involving 278 participants. 
# Demographics such as age, gender, and handedness were collected through an 
# introductory questionnaire. Participants were recruited randomly via mailing 
# lists and performed the tests online. Each participant performed the 
# experiment starting randomly with either auditory or visual cues, with five 
# trials for each cue type.
#
# define the variables of the experiment and their corresponding data type

# Gender	      - Nominal	Categories of gender.
# Handedness	  - Nominal	Categories of dominant hand.
# Cue Type	    - Nominal	Type of cue (Visual or Auditory).
# Trial Number  -	Ordinal	Order of trials within each cue type.
# Reaction Time	- Ratio	Measured in milliseconds, with a meaningful zero.
# Age	          - Ratio	Measured in years, with a meaningful zero.
#
# Define the variables of the experiment
# IV:
# Cue type
# DV:
# Reaction time
# Random:
# (demographics (only >=18 years as requirement))
# environment, display size laptop, ...
# potential confoundings:
# sleep time, unawareness, speaker volume, display brightness


# load the data and show the distributions of the demographics
# hint: the field participation_order should be ignored
# Laden der notwendigen Pakete
library(tidyverse)

# Daten einlesen
data <- read.csv("ReactionTime.csv")

# Überblick über die Datenstruktur
str(data)

# Verteilung des Alters (Histogramm)
ggplot(data, aes(x = age)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Altersverteilung der Teilnehmer",
       x = "Alter",
       y = "Häufigkeit") +
  theme_minimal()

# Verteilung des Geschlechts (Balkendiagramm)
ggplot(data, aes(x = gender)) +
  geom_bar(fill = "orange", color = "black", alpha = 0.7) +
  labs(title = "Verteilung des Geschlechts",
       x = "Geschlecht",
       y = "Anzahl der Teilnehmer") +
  theme_minimal()

# Verteilung der Händigkeit (Balkendiagramm)
ggplot(data, aes(x = handedness)) +
  geom_bar(fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Verteilung der Händigkeit",
       x = "Händigkeit",
       y = "Anzahl der Teilnehmer") +
  theme_minimal()

# Dichteplot nach Geschlecht
ggplot(data, aes(x = age, fill = gender)) +
  geom_density(alpha = 0.5) +
  labs(title = "Dichteplot des Alters nach Geschlecht",
       x = "Alter",
       y = "Dichte",
       fill = "Geschlecht") +
  theme_minimal()

# visualize the distribution of all reaction times depended on cue type

# Daten vorbereiten: Reaktionszeiten in ein langes Format bringen
reaction_data_long <- data %>%
  pivot_longer(
    cols = matches("visualcue_attempt|auditorycue_attempt"),
    names_to = "cue_type",
    values_to = "reaction_time"
  ) %>%
  mutate(
    cue_type = case_when(
      str_detect(cue_type, "visual") ~ "Visual",
      str_detect(cue_type, "auditory") ~ "Auditory"
    )
  )

# many young people, "strange" cue densities, more right handed people, more women

####################################################################################

# find out if reaction times differ by cue type, what is the Nullhypothesis?

# Test auf Normalverteilung (Shapiro-Wilk-Test)
shapiro_visual <- shapiro.test(reaction_data_long$reaction_time[reaction_data_long$cue_type == "Visual"])
shapiro_auditory <- shapiro.test(reaction_data_long$reaction_time[reaction_data_long$cue_type == "Auditory"])

print(shapiro_visual)
print(shapiro_auditory)
# p<0.05 = no normal distribution!

# Daten für visuelle und auditive Reaktionszeiten filtern
visual_reactions <- reaction_data_long$reaction_time[reaction_data_long$cue_type == "Visual"]
auditory_reactions <- reaction_data_long$reaction_time[reaction_data_long$cue_type == "Auditory"]

# Wilcoxon-Test für den Unterschied zwischen den Gruppen
wilcox_test_result <- wilcox.test(
  x = visual_reactions,
  y = auditory_reactions,
  paired = FALSE # Ungepaarte Daten
)

print(wilcox_test_result)

# both are significantly different!

ggplot(reaction_data_long, aes(x = cue_type, y = reaction_time, fill = cue_type)) +
  geom_boxplot() +
  labs(
    title = "Vergleich der Reaktionszeiten nach Hinweisart",
    x = "Hinweisart",
    y = "Reaktionszeit (ms)"
  ) +
  theme_minimal()



# was the study well powered to state the results?

install.packages("pwr")
library(pwr)

# Effektgröße berechnen
cohens_d <- (mean(visual_reactions, na.rm = TRUE) - mean(auditory_reactions, na.rm = TRUE)) /
  sqrt((sd(visual_reactions, na.rm = TRUE)^2 + sd(auditory_reactions, na.rm = TRUE)^2) / 2)

# Power-Analyse für Wilcoxon-Test
power_result <- pwr.t.test(
  n = min(length(visual_reactions), length(auditory_reactions)),
  d = cohens_d,
  sig.level = 0.05,
  type = "two.sample",
  alternative = "two.sided"
)
print(power_result)


# visualize the reaction times depended on handedness


# Box Plot of Reaction Times by Handedness
ggplot(reaction_data_long, aes(x = handedness, y = reaction_time, fill = handedness)) +
  geom_boxplot() +
  labs(
    title = "Distribution of Reaction Times by Handedness",
    x = "Handedness",
    y = "Reaction Time (ms)"
  ) +
  theme_minimal()

# Density Plot of Reaction Times by Handedness
ggplot(reaction_data_long, aes(x = reaction_time, fill = handedness)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Density of Reaction Times by Handedness",
    x = "Reaction Time (ms)",
    y = "Density",
    fill = "Handedness"
  ) +
  theme_minimal()


# find out if reaction times differ by handedness, what is the Nullhypothesis?

# Verteilung der Reaktionszeiten nach Händigkeit prüfen
table(reaction_data_long$handedness)

# Wilcoxon-Test oder Mann-Whitney-Test für zwei Gruppen (z. B. Rechts- vs. Linkshänder)
if (length(unique(reaction_data_long$handedness)) == 2) {
  # Daten filtern
  right_handed <- reaction_data_long$reaction_time[reaction_data_long$handedness == "Right"]
  left_handed <- reaction_data_long$reaction_time[reaction_data_long$handedness == "Left"]
  
  # Wilcoxon-Test durchführen
  wilcox_test_result <- wilcox.test(
    x = right_handed,
    y = left_handed,
    paired = FALSE
  )
  print(wilcox_test_result)
} else {
  # Kruskal-Wallis-Test für mehr als zwei Gruppen
  kruskal_test_result <- kruskal.test(
    reaction_time ~ handedness,
    data = reaction_data_long
  )
  print(kruskal_test_result)
}


# was the study well powered to state the results?

# Anzahl der Teilnehmer pro Gruppe
n_right <- length(right_handed)
n_left <- length(left_handed)

# Mittelwerte und Standardabweichungen berechnen
mean_right <- mean(right_handed, na.rm = TRUE)
mean_left <- mean(left_handed, na.rm = TRUE)
sd_right <- sd(right_handed, na.rm = TRUE)
sd_left <- sd(left_handed, na.rm = TRUE)

# Effektgröße berechnen (Cohen's d)
pooled_sd <- sqrt(((n_right - 1) * sd_right^2 + (n_left - 1) * sd_left^2) / (n_right + n_left - 2))
cohens_d <- abs(mean_right - mean_left) / pooled_sd

# Power berechnen
library(pwr)
power_result <- pwr.t2n.test(
  n1 = n_right,
  n2 = n_left,
  d = cohens_d,
  sig.level = 0.05
)
print(power_result)


# visualize reaction times of men and women

# Boxplot der Reaktionszeiten nach Geschlecht
ggplot(reaction_data_long, aes(x = gender, y = reaction_time, fill = gender)) +
  geom_boxplot() +
  labs(
    title = "Verteilung der Reaktionszeiten nach Geschlecht",
    x = "Geschlecht",
    y = "Reaktionszeit (ms)"
  ) +
  theme_minimal()


# find out if reaction times differ between men and women

# Installiere und lade das Paket car für den Levene-Test
# Installiere und lade das Paket car für den Levene-Test
if (!require(car)) install.packages("car")
library(car)

# Levene-Test für die Gleichheit der Varianzen
levene_test_result <- leveneTest(reaction_time ~ gender, data = reaction_data_long)
print(levene_test_result)

# Normalverteilung testen
shapiro_male <- shapiro.test(male_reactions)
shapiro_female <- shapiro.test(female_reactions)

print(shapiro_male)
print(shapiro_female)

# Entscheidung basierend auf Levene-Test und Normalverteilung
if (levene_test_result$`Pr(>F)`[1] > 0.05) {
  if (shapiro_male$p.value > 0.05 && shapiro_female$p.value > 0.05) {
    # Normalverteilung und Varianzgleichheit: t-Test
    t_test_result <- t.test(
      male_reactions,
      female_reactions,
      paired = FALSE,
      alternative = "two.sided",
      var.equal = TRUE
    )
    print(t_test_result)
  } else {
    # Nicht normalverteilt, aber Varianzgleichheit: Wilcoxon-Test
    wilcox_test_result <- wilcox.test(
      male_reactions,
      female_reactions,
      paired = FALSE,
      alternative = "two.sided"
    )
    print(wilcox_test_result)
  }
} else {
  # Varianzen ungleich: Welch-t-Test oder robustere Methoden
  t_test_result <- t.test(
    male_reactions,
    female_reactions,
    paired = FALSE,
    alternative = "two.sided",
    var.equal = FALSE
  )
  print(t_test_result)
}

# mit bootstrapping

# Installiere und lade das Paket car für den Levene-Test
if (!require(car)) install.packages("car")
library(car)

# Levene-Test für die Gleichheit der Varianzen
levene_test_result <- leveneTest(reaction_time ~ gender, data = reaction_data_long)
print(levene_test_result)

# Normalverteilung testen
shapiro_male <- shapiro.test(male_reactions)
shapiro_female <- shapiro.test(female_reactions)

print(shapiro_male)
print(shapiro_female)

# Entscheidung basierend auf Levene-Test und Normalverteilung
if (levene_test_result$`Pr(>F)`[1] > 0.05) {
  if (shapiro_male$p.value > 0.05 && shapiro_female$p.value > 0.05) {
    # Normalverteilung und Varianzgleichheit: t-Test
    t_test_result <- t.test(
      male_reactions,
      female_reactions,
      paired = FALSE,
      alternative = "two.sided",
      var.equal = TRUE
    )
    print(t_test_result)
  } else {
    # Nicht normalverteilt, aber Varianzgleichheit: Wilcoxon-Test
    wilcox_test_result <- wilcox.test(
      male_reactions,
      female_reactions,
      paired = FALSE,
      alternative = "two.sided"
    )
    print(wilcox_test_result)
  }
} else {
  # Varianzen ungleich: Welch-t-Test oder robustere Methoden
  t_test_result <- t.test(
    male_reactions,
    female_reactions,
    paired = FALSE,
    alternative = "two.sided",
    var.equal = FALSE
  )
  print(t_test_result)
}

# Dichteplot der Reaktionszeiten nach Geschlecht
ggplot(reaction_data_long, aes(x = reaction_time, fill = gender)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Dichte der Reaktionszeiten nach Geschlecht",
    x = "Reaktionszeit (ms)",
    y = "Dichte",
    fill = "Geschlecht"
  ) +
  theme_minimal()

# was the study well powered to state the results?

# Installiere und lade das Paket "pwr"
if (!require(pwr)) install.packages("pwr")
library(pwr)

# Effektgröße berechnen
mean_male <- mean(male_reactions, na.rm = TRUE)
mean_female <- mean(female_reactions, na.rm = TRUE)
sd_male <- sd(male_reactions, na.rm = TRUE)
sd_female <- sd(female_reactions, na.rm = TRUE)

pooled_sd <- sqrt(((length(male_reactions) - 1) * sd_male^2 +
                     (length(female_reactions) - 1) * sd_female^2) /
                    (length(male_reactions) + length(female_reactions) - 2))

cohen_d <- abs(mean_male - mean_female) / pooled_sd

# Power berechnen
power_result <- pwr.t2n.test(
  n1 = length(male_reactions),
  n2 = length(female_reactions),
  d = cohen_d,
  sig.level = 0.05
)
print(power_result)


# significant but not well powered!

