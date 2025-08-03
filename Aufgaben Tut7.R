library(car)
library(dplyr)

# t-Test
  # a) Erstelle vier Vektoren:
  color <- c(1, 1, 0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1)
  size <- c(27.5, 26.0, 24.3, 27.0, 30.0, 28.5, 28.6, 29.7, 24.4, 23.2, 27.5, 27.6, 26.4, 28.0, 27.5, 27.8, 31.2, 29.6, 26.7, 27.5)
  weight <- c(300, 312, 298, 350, 287, 298, 301, 301, 309, 298, 290, 287, 311, 325, 275, 312, 285, 255, 304, 320)
  age <- c(6, 4, 10, 12, 11, 5, 7, 9, 13, 15, 12, 10, 11, 16, 7, 5, 6, 15, 13, 12)
  
  # b) Erstelle einen Datensatz aus den Vektoren:
  fish <- data.frame(color, size, weight, age)
  
  # c) Prüfe folgende Hypothese: Blaue Fische sind schwerer als grüne Fische.
    # Vorannahmen prüfen:
      # metrisch? -> Ja
      # Normalverteilung?
      green <- fish %>%
        filter(color == 1)
      blue <- fish %>%
        filter(color == 0)
      shapiro.test(green$weight) # -> p > 0,05 -> Normalverteilung
      shapiro.test(blue$weight) # -> p > 0,05 -> Normalverteilung
      shapiro.test(fish$weight)
      # Varianzhomogenität?
      leveneTest(fish$weight, fish$color) # -> Pr(>F) > 0,05 -> Varianzhomogenität
      
    # t-Test für unabhängige Stichproben
      #H0: Blaue Fische sind leichter oder gleich schwer wie grüne Fische. mu(grün) <= mu(blau)
      #H1: Blaue Fische sind schwerer als grüne Fische. mu(blau) > mu(grün)
      t.test(fish$weight ~ fish$color, alternative = "greater")
      # -> p = 0,76 > 0,05 -> H0 beibehalten
  
  # d) Prüfe folgende Hypothese: Es gibt einen Unterschied zwischen grünen und blauen Fischen im Hinblick auf ihr Alter.
    # Vorannahmen prüfen:
      # metrisch? -> Ja
      # Normalverteilung?
      shapiro.test(green$age) # -> p > 0,05 -> Normalverteilung
      shapiro.test(blue$age) # -> p > 0,05 -> Normalverteilung
      # Varianzhomogenität?
      leveneTest(fish$age, fish$color) # -> Pr(>F) > 0,05 -> Varianzhomogenität
      
      # t-Test für unabhängige Stichproben
        #H0: Es gibt keinen Unterschied zwischen grünen und blauen Fischen. mu(grün) = mu(blau)
        #H1: Es gibt einen Unterschied zwischen grünen und blauen Fischen. mu(grün) | mu(blau)
        t.test(fish$age ~ fish$color, alternative = "two.sided")
        # -> p = 0,08 > 0,05 -> H0 beibehalten
        
# ANOVA
  # a) Erstelle vier Vektoren:
    color_2 <- c(1, 1, 0, 2, 0, 1, 2, 1, 1, 2, 0, 0, 1, 0, 2, 1, 0, 0, 1, 2)
    size_2 <- c(27.5, 26.0, 24.3, 27.0, 30.0, 28.5, 28.6, 29.7, 24.4, 23.2, 27.5, 27.6, 26.4, 28.0, 27.5, 27.8, 31.2, 29.6, 26.7, 27.5)
    weight_2 <- c(300, 312, 298, 350, 287, 298, 301, 301, 309, 298, 290, 287, 311, 325, 275, 312, 285, 255, 304, 320)
    age_2 <- c(6, 4, 10, 12, 11, 5, 7, 9, 13, 15, 12, 10, 11, 16, 7, 5, 6, 15, 13, 12)
  # b) Erstelle einen Datensatz aus den Vektoren:
    fish_2 <- data.frame(color_2, size_2, weight_2, age_2)
  # c) Prüfe folgende Hypothese: Verschiedenfarbige Fische unterscheiden sich in ihrem Alter.
    # Vorannahmen prüfen:
    # metrisch? -> Ja, unabhängig? -> Ja
    # Normalverteilung?
    green_2 <- fish_2 %>%
      filter(color_2 == 1)
    blue_2 <- fish_2 %>%
      filter(color_2 == 0)
    red <- fish_2 %>%
      filter(color_2 == 2)
    shapiro.test(green_2$age_2) # -> p > 0,05 -> Normalverteilung
    shapiro.test(blue_2$age_2) # -> p > 0,05 -> Normalverteilung
    shapiro.test(red$age_2) # -> p > 0,05 -> Normalverteilung
    # Varianzhomogenität?
    leveneTest(fish_2$age_2, fish_2$color_2) # -> Pr(>F) > 0,05 -> Varianzhomogenität
    
    # ANOVA
      # H0: Es gibt keinen Unterschied zwischen den Gruppen.
      # H1: Es gibt einen Unterschied zwischen den Gruppen.
      anova_fish <- aov(age_2 ~ color_2, data = fish_2)
      summary(anova_fish)    
      oneway.test(age_2 ~ color_2, data = fish_2)
      # Pr(>F) > 0,05 -> H0 beibehalten -> kein Post hoc Test