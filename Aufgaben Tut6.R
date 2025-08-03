library(dplyr)
library(plotrix)

# Punktschätzung in R
  # Schätzer für Geburtsjahr + Standardfehler
    # händisch
    mean(Analyse_starwars$birth_year) # -> 77.82
    sd(Analyse_starwars$birth_year) / sqrt(32) # -> 27.14
    # Interpretation: Durchschnittlich weichen die in den Stichproben beobachteten Mittelwerte für das Geburtsjahr um 27.14 Jahre vom tatsächlichen Mittelwert ab. 

    # dplyr
    Analyse_starwars %>%
      summarize(mean = mean(birth_year), sd = sd(birth_year), se = sd(birth_year) / sqrt(32)) # -> mean = 77.8, sd = 154, se = 27.1

    # plotrix
    mean(Analyse_starwars$birth_year) # -> 77.82
    std.error(Analyse_starwars$birth_year) # -> 27.14
    
# Intervallschätzung in R
  # Konfidenzintervall für Masse
  mean(Analyse_starwars$mass) + 1.96 * std.error(Analyse_starwars$mass) # -> OG = 87.53
  mean(Analyse_starwars$mass) - 1.96 * std.error(Analyse_starwars$mass) # -> UG = 68.81
  # Interpretation: Mit einer Sicherheit von 95% liegt die durchschnittliche Masse im Intervall zwischen 68.81 und 87.53 kg.
  
  # dplyr
  Analyse_starwars %>%
    summarize(mean = mean(mass), se = std.error(mass), UG = mean(mass) - 1.96 * std.error(mass), OG = mean(mass) + 1.96 * std.error(mass))
    # verkürzt:
    Analyse_starwars %>%
      summarize(mean = mean(mass), se = std.error(mass), UG = mean - 1.96 * se, OG = mean + 1.96 * se)

# Vergleich von Konfidenzintervallen
    weiblich <- Analyse_starwars %>%
      filter(sex == "female")
    männlich <- Analyse_starwars %>%
      filter(sex == "male")
    # Konfidenzintervall Frauen
    mean(weiblich$height) - 1.96 * std.error(weiblich$height)
    mean(weiblich$height) + 1.96 * std.error(weiblich$height)        
      # -> 158.36 cm - 172.98 cm
    # Konfidenzintervall Männer
    mean(männlich$height) - 1.96 * std.error(männlich$height)
    mean(männlich$height) + 1.96 * std.error(männlich$height)
      # -> 164.24 cm - 189.84 cm
    #Vergleich: Die Intervalle überschneiden sich -> eher kein Unterschied im Hinblick auf die Größe
# z-Test des Mittelwerts in R
    mean_mass <- mean(Analyse_starwars$mass)
    se_mass <- std.error(Analyse_starwars$mass)    
  # Konfidenzintervall
    mean_mass - 1.96 * se_mass
    mean_mass + 1.96 * se_mass    
    # -> nur in 5 % der Stichproben würden Mittelwerte vorkommen, die kleiner als 68.81 kg und größer als 87.53 kg sind.
      # -> 78 kg liegt im angegebenen Intervall -> H0 beibehalten