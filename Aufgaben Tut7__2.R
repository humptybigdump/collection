library(car)
library(dplyr)
library(ggplot2)

# t-Test
  # a) t-Test für eine Stichprobe
    # H1: mu(Top100) > mu(theor: 1973)
    # H0: mu(Top100) <= mu(theor)
    # alpha = 0.05
    t.test(chartsger$byear, mu = 1973, alternative = "greater")
    # p < 0.05: H0 ablehnen -> Musiker*innen sind jünger als 50.
  # b) t-Test für abhängige Stichproben
    # H1: mu(Likes) > mu(Klicks)
    # H0: mu(Likes) <= mu(Klicks)
    # alpha = 0.05
    t.test(chartsger$yt_likes/1000, chartsger$yt_clicks, alternative = "greater", paired = TRUE)
    # p > 0.05: H0 beibehalten -> Top100-Songs haben nicht mehr YT-Likes als Klicks.
  # c) t-Test für unabhängige Stichproben
    # H1: mu(male) > mu(female)
    # H2: mu(male) <= mu(female)
    # alpha = 0.05
    cis_chartsger <- chartsger %>%
      filter(gender != 3)
    t.test(cis_chartsger$height ~ cis_chartsger$gender, alternative = "greater")    
    # p < 0.05: H0 ablehnen -> Männer sind größer als Frauen.
  # d) Vorannahmen (C)
    # metrische Variablen -> Größe in cm = verhältnisskaliert
    # Normalverteilung?
    shapiro.test(cis_chartsger$height)
    cis_chartsger %>%
      ggplot(aes(x = height)) +
        geom_density()
      # p < 0.05 -> keine Normalverteilung, Test nicht zulässig
    # Varianzhomogenität?
    leveneTest(cis_chartsger$height, cis_chartsger$gender)
      # p > 0.05 -> Varianzhomogenität

# ANOVA
  # a) Haarfarbe/Spotify-Streams
    # H1: mu(1) != mu(2) != mu(3) != mu(4) != mu(5) != mu(6) != mu(7)
    # H0: mu(1) = mu(2) = mu(3) = mu(4) = mu(5) = mu(6) = mu(7)
    # alpha = 0.05
    anova <- aov(sfy_streams ~ haircol, data = chartsger)
    summary(anova)
    # oder:
    oneway.test(sfy_streams ~ haircol, data = chartsger, var.equal = FALSE) # -> Gruppen nicht groß genug
    # p > 0.05 -> kein Unterschied in Spotify-Streams je nach Haarfarbe
  # b) Post hoc: nicht nötig -> keine Unterschiede
    pairwise.t.test(chartsger$sfy_streams, chartsger$haircol, p.adjust.method = "bonferroni")
  # c) Vorannahmen
    # metrisch: Ja (Streams = verhältnisskaliert)
    # Normalverteilung?
    shapiro.test(chartsger$sfy_streams)
    chartsger %>%
      ggplot(aes(x = sfy_streams)) +
        geom_density()
      # p < 0.05 -> keine Normalverteilung; Test nicht zulässig
    # Varianzhomogenität?
    leveneTest(chartsger$sfy_streams, chartsger$haircol)
      # p < 0.05 -> keine Varianzhomogenität; Test nicht zulässig
    # unabhängige Stichprobe: Ja, verschiedene Haarfarben

# Für Schnelle: Altersgruppen/Spotify-Streams
    age_chartsger <- chartsger %>%
      mutate(agegroup = case_when(
        byear > 2003 ~ "U20",
        byear <= 2003 & byear > 1983 ~ "aktiv",
        byear <= 1983 ~ "Ü40"
      ))
    # H1: mu(U20) > mu(20-40) > mu(Ü40)
    # H0: mu(U20) <= mu(20-40) <= mu(Ü40)
    # alpha = 0.05
    anova2 <- aov(sfy_streams ~ agegroup, data = age_chartsger)    
    summary(anova2)
    # p > 0.05: H0 beibehalten -> kein Unterschied zwischen Altersgruppen
    pairwise.t.test(age_chartsger$sfy_streams, age_chartsger$agegroup, p.adjust.method = "bonferroni")
    # Vorannahmen
    shapiro.test(age_chartsger$sfy_streams) #p<0.05 -> keine Normalverteilung
    leveneTest(age_chartsger$sfy_streams, age_chartsger$agegroup) #p>0.05 -> Varianzhomogenität
    # metrisch und unabhängige Stichprobe: ja
    