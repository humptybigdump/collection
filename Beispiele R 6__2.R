library(dplyr)
library(ggplot2)
library(tidyr)
library(sjmisc)
library(readr)

# Datensatz vorbereiten
chartsger <- read_csv2("C:/Users/ueneu/Documents/Tutorium/Vorbereitung/Datensätze/chartsger.CSV", trim_ws = TRUE, na = "-77")
Analyse_chartsger <- chartsger

# Annahme: Top100 als Stichprobe aller Musiker*innen

# Punktschätzung Größe (Folie 10)
  # händisch (n = 68):
  mean(Analyse_chartsger$height, na.rm = TRUE)
  sd(Analyse_chartsger$height, na.rm = TRUE)
  
  sd(Analyse_chartsger$height, na.rm = TRUE) / sqrt(68)
  
  # dplyr (Folie 11):
  Analyse_chartsger %>%
    drop_na() %>%
    summarize(meanHeight = mean(height), sdHeight = sd(height), seHeight = sd(height) / sqrt(68))

  # plotrix (Folie 12):
  install.packages("plotrix")
  library(plotrix)  
  mean(Analyse_chartsger$height, na.rm = TRUE)
  std.error(Analyse_chartsger$height, na.rm = TRUE)  
  
# Intervallschätzung Größe (n = 68)
  # händisch (Folie 14):
  mean(Analyse_chartsger$height, na.rm = TRUE)
  std.error(Analyse_chartsger$height, na.rm = TRUE)  
  176.34 + 1.96 * 1.03  
  176.34 - 1.96 * 1.03  

  # weniger Schritte (Folie 14)
  mean(Analyse_chartsger$height, na.rm = TRUE) + 1.96 * std.error(Analyse_chartsger$height, na.rm = TRUE)
  mean(Analyse_chartsger$height, na.rm = TRUE) - 1.96 * std.error(Analyse_chartsger$height, na.rm = TRUE)  
  
  # dplyr (Folie 15)
  Analyse_chartsger %>%
    drop_na() %>%
    summarize(mean = mean(height), sd = sd(height), se = std.error(height), UG = mean(height) - 1.96 * std.error(height), OG = mean(height) + 1.96 * std.error(height))
    # verkürzt
    Analyse_chartsger %>%
      drop_na() %>%
      summarize(mean = mean(height), se = std.error(height), UG = mean - 1.96 * se, OG = mean + 1.96 * se)
    
# z-Test des Mittelwerts in R (Folie 18)
    shapiro.test(Analyse_chartsger$height)
    mean_height <- mean(Analyse_chartsger$height, na.rm = TRUE)
    se_height <- std.error(Analyse_chartsger$height, na.rm = TRUE)   
    mean_height - 1.96 * se_height # -> 174.31
    mean_height + 1.96 * se_height # -> 178.37
    
# Annahmebereiche (Folie 20)
    # zweiseitig
    ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +
      stat_function(fun = dnorm, color = "grey") +
      geom_vline(xintercept = -1.96, linetype = "solid") +
      geom_vline(xintercept = 1.96, linetype = "solid") +
      geom_text(aes(x = 0, y = 0.2, label = "Annahmebereich H0")) +
      theme_classic()
    
    # rechtsseitig
    ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +
      stat_function(fun = dnorm, color = "grey") +
      geom_vline(xintercept = 1.64, linetype = "solid") +
      geom_text(aes(x = -1.5, y = 0.2, label = "Annahmebereich H0")) +
      theme_classic()
    
    # linksseitig
    ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +
      stat_function(fun = dnorm, color = "grey") +
      geom_vline(xintercept = -1.64, linetype = "solid") +
      geom_text(aes(x = 1.5, y = 0.2, label = "Annahmebereich H0")) +
      theme_classic()