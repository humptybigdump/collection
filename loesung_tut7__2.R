if(rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

# Packages laden
library(tidyverse)
library(DescTools)

# Datensatz einlesen
em_daten <- read_csv("em_fernsehminuten_datensatz_gross.csv")

# Fernsehzeit Finalzuschauer:innen vs. deutscher Durchschnitt -> t-Test für eine Stichprobe
  # H0: Fernsehzeit Befragte = 5460 Minuten
  # H1: Fernsehzeit Befragte != 5460 Minuten
  # Signifikanzniveau: alpha = 0.05
## Vorannahmen
  # metrische Variable? Ja
  # Normalverteilung? (Kolmogorov-Smirnov-Test, da Shapiro-Wilks nur für kleine Stichproben funktioniert)
ks.test(em_daten$fernseh_min_monat, "pnorm", mean = mean(em_daten$fernseh_min_monat), sd = sd(em_daten$fernseh_min_monat))
  # p-value = 0.8596 > 0.05 -> Normalverteilung liegt vor
## t-Test für eine Stichprobe
t.test(em_daten$fernseh_min_monat, mu = 5460, alternative = "two.sided")
  # p-value < 2.2e-16 < 0.05 -> H0 wird abgelehnt, der Mittelwert der Finalbesucher:innen entspricht nicht dem deutschen Durchschnitt.

# EM-Minuten Fans vs. Nicht-Fans -> t-Test für unabhängige Stichproben
  # H0: EM-Minuten Fans <= EM-Minuten Nicht-Fans
  # H1: EM-Minuten Fans > EM-Minuten Nicht-Fans
  # Signifikanzniveau: alpha = 0.05
## Vorannahmen
  # metrische Variable? Ja
  # Normalverteilung?
ks.test(em_daten$em_min, "pnorm", mean = mean(em_daten$em_min), sd = sd(em_daten$em_min))
  # p-value = 0.7787 > 0.05 -> Normalverteilung liegt vor
  # Varianzhomogenität?
LeveneTest(em_daten$em_min, em_daten$fan)
  # p-value = 0.4607 > 0.05 -> varianzhomogenität liegt vor
## t-Test für unabhängige Stichproben
t.test(em_daten$em_min ~ em_daten$fan, alternative = "less")
  # p-value = 0.216 > 0.05 -> H0 wird beibehalten, Fans schauen nicht mehr EM-Minuten als Nicht-Fans

# Fernsehnutzung x EM-Minuten -> t-Test für abhängige Stichproben
  # H0: Fernsehminuten <= EM-Minuten
  # H1: Fernsehminuten > EM-Minuten
  # Signifikanzniveau: alpha = 0.05
## Vorannahmen
  # metrische Variablen? Ja
  # Normalverteilung?
ks.test(em_daten$fernseh_min_monat, "pnorm", mean = mean(em_daten$fernseh_min_monat), sd = sd(em_daten$fernseh_min_monat))
  # p-value = 0.8596 > 0.05 -> Normalverteilung liegt vor
ks.test(em_daten$em_min, "pnorm", mean = mean(em_daten$em_min), sd = sd(em_daten$em_min))
  # p-value = 0.7787 > 0.05 -> Normalverteilung liegt vor
  # Varianzhomogenität?
LeveneTest(em_daten$em_min, em_daten$fernseh_min_monat)
  # nicht durchführbar -> unklare Varianzhomogenität
## t-Test für abhängige Stichproben
t.test(em_daten$em_min, em_daten$fernseh_min_monat, alternative = "greater", paired = TRUE)
  # p-value = 1 > 0.05 -> H0 wird beibehalten, Höhere monatliche Fernsehnutzung geht nicht mit höhere Anzahl an geschauten EM-Minuten einher.

# ANOVA: EM-Minuten nach Lieblingsnation
  # H0: Es gibt keinen Unterschied in den geschauten EM-Minuten in Zusammenhang mit der jeweiligen Lieblingsnation.
  # H1: Es gibt einen Unterschied in den geschauten EM-Minuten in Zusammenhang mit der jeweiligen Lieblingsnation.
  # Signifikanzniveau: alpha = 0.05
## Vorannahmen
  # metrische Variablen? Ja
  # Unabhängige Stichproben? Ja
  # Normalverteilung?
ks.test(em_daten$em_min, "pnorm", mean = mean(em_daten$em_min), sd = sd(em_daten$em_min))
  # p-value = 0.7787 > 0.05 -> Normalverteilung liegt vor
  # Varianzhomogenität?
LeveneTest(em_daten$em_min, em_daten$lieblingsnation)
  # p-value = 0.113 > 0.05 -> varianzhomogenität liegt vor
## ANOVA
anova_em_lieblingsnation <- aov(em_min ~ lieblingsnation, data = em_daten)
summary(anova_em_lieblingsnation)
  # p-value = 0.0171 < 0.05 -> H0 kann abgelehnt werden: Es gibt einen Unterschied in den geschauten EM-Minuten je nach Lieblingsnation

# ANOVA: Fernsehminuten nach Lieblingsnation
  # H0: Es gibt keinen Unterschied in den Fernsehminuten/Monat nach Lieblingsnation
  # H1: Es gibt einen Unterschied in den Fernsehminuten/Monat nach Lieblingsnation
  # Signifikanzniveau: alpha = 0.05
## Vorannahmen
  # metrische Variablen? Ja
  # unabhängige Stichproben? Ja
  # Normalverteilung?
ks.test(em_daten$fernseh_min_monat, "pnorm", mean = mean(em_daten$fernseh_min_monat), sd = sd(em_daten$fernseh_min_monat))
  # p-value = 0.8596 > 0.05 -> Normalverteilung liegt vor
  # Varianzhomogenität?
LeveneTest(em_daten$fernseh_min_monat, em_daten$lieblingsnation)
  # p-value = 0.9884 > 0.05 -> Varianzhomogenität liegt vor
## ANOVA
anova_fernsehmin_lieblingsnation <- aov(fernseh_min_monat ~ lieblingsnation, data = em_daten)
summary(anova_fernsehmin_lieblingsnation)
  # p-value = 0.243 > 0.05 -> H0 wird beibehalten: Es gibt keinen Unterschied in den Fersehminuten/Monat in Zusammenhang mit der Lieblingsnation

# Bonus:
## Gruppeneinteilung
em_daten <- em_daten %>% 
  mutate(fernsehnutzung = if_else(fernseh_min_monat < 5000, 1, if_else(fernseh_min_monat > 6000, 3, 2)))
## ANOVA
  # H0: Es gibt keinen Unterschied zwischen unterschiedlichen Fernsehnutzer:innen.
  # H1: Es gibt einen Unterschied zwischen unterschiedlichen Fernsehnutzer:innen.
  # Signifikanzniveau: alpha = 0.05
  # Vorannahmen:
    # metrische Variablen? Ja
    # unabhängige Stichproben? Ja
    # Normalverteilung?
ks.test(em_daten$em_min, "pnorm", mean = mean(em_daten$em_min), sd = sd(em_daten$em_min))
    # p-value = 0.7787 > 0.05
    # Varianzhomogenität
LeveneTest(em_daten$em_min, em_daten$fernsehnutzung)
    # nicht durchführbar, da tatsächlich die Gruppen 1 und 3 nicht auftreten -> ANOVA nicht durchführbar