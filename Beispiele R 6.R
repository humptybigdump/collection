library(dplyr)
library(ggplot2)
library(tidyr)
library(sjmisc)

# Datensatz vorbereiten
data("starwars")
Analyse_starwars <- starwars %>%
  select(name, height, mass, hair_color, eye_color, birth_year, sex)
Analyse_starwars <- na.omit(Analyse_starwars)

# Annahme: Stichprobe = 32 Charaktere aus insgesamt 87 -> n = 32

# Punktschätzung Größe
  # händisch (n = 32):
  mean(Analyse_starwars$height)
  sd(Analyse_starwars$height)

  sd(Analyse_starwars$height) / sqrt(32)
  
  # dplyr:
  Analyse_starwars %>%
    summarize(meanHeight = mean(height), sdHeight = sd(height), seHeight = sd(height) / sqrt(32))

  # plotrix:
  install.packages("plotrix")
  library(plotrix)  
  mean(Analyse_starwars$height)
  std.error(Analyse_starwars$height)  
  
# Intervallschätzung Größe
  # händisch:
  mean(Analyse_starwars$height)
  std.error(Analyse_starwars$height)  
  175.63 + 1.96 * 5.24  
  175.63 - 1.96 * 5.24  

  # weniger Schritte
  mean(Analyse_starwars$height) + 1.96 * std.error(Analyse_starwars$height)
  mean(Analyse_starwars$height) - 1.96 * std.error(Analyse_starwars$height)  
  
  # dplyr
  Analyse_starwars %>%
    summarize(mean = mean(height), sd = sd(height), se = std.error(height), UG = mean(height) - 1.96 * std.error(height), OG = mean(height) + 1.96 * std.error(height))
    # verkürzt
    Analyse_starwars %>%
      summarize(mean = mean(height), se = std.error(height), UG = mean - 1.96 * se, OG = mean + 1.96 * se)
    
# z-Test des Mittelwerts in R
    shapiro.test(Analyse_starwars$height)
    mean_height <- mean(Analyse_starwars$height)
    se_height <- std.error(Analyse_starwars$height)   
    mean_height - 1.96 * se_height # -> 165.35
    mean_height + 1.96 * se_height # -> 185.90