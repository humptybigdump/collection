library(dplyr)
library(car)
library(readr)
library(ggplot2)


# Datensatz
chartsger <- read_csv2("Dateipfad/chartsger.CSV", trim_ws = TRUE, na = "-77")
Analyse_chartsger <- chartsger %>%
  filter(gender != 3)

# Vorannahmen prüfen (Folie 6)
shapiro.test(Analyse_chartsger$sfy_streams)
Analyse_chartsger %>%
  ggplot(aes(x = sfy_streams)) +
  geom_density()
leveneTest(Analyse_chartsger$sfy_streams, Analyse_chartsger$gender)

# t-Test für eine Stichprobe (Folie 6)
t.test(Analyse_chartsger$sfy_streams, mu = 1, alternative = "greater")

# t-Test für unabhängige Stichproben (Folie 7)
t.test(Analyse_chartsger$sfy_streams ~ Analyse_chartsger$gender)

# t-Test für abhängige Stichproben (Folie 8)
t.test(Analyse_chartsger$sfy_streams, Analyse_chartsger$yt_clicks, paired = TRUE)

# ANOVA-Vorannahmen prüfen
shapiro.test(chartsger$sfy_streams)
leveneTest(chartsger$sfy_streams, chartsger$gender)

# ANOVA (Folie 11(B)/12(A))
anova <- aov(sfy_streams ~ gender, data = chartsger)
summary(anova)

oneway.test(sfy_streams ~ gender, data = chartsger, var.equal = TRUE)

# Post hoc Test (Folie 12(B)/13(A))
pairwise.t.test(chartsger$sfy_streams, chartsger$gender, p.adjust.method = "bonferroni")