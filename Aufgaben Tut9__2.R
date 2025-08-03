library(tidyverse)
library(correlation)
library(DescTools)

# Aufgabe 1 und 2 im PDF, in diesem Skript sind nur die Lösungen zu Aufgabe 3

# A: Zusammenhang zwischen sfy_streams und yt_sub
  # 2 metrische Variablen -> Pearsons r
cor(Analyse_chartsger$sfy_streams, Analyse_chartsger$yt_sub, use = "complete.obs")
  # r = 0.48 -> mittelstarker, positiver Zusammenhang

# B: Zusammenhang zwischen crank und britcharts
  # 2 ordinalskalierte Variablen -> Spearmans Rho / Kendalls Tau
cor(Analyse_chartsger$crank, Analyse_chartsger$britcharts, use = "complete.obs", method = "kendall")
  # tau = 0.41 -> mittelstarker, positiver Zusammenhang
cor(Analyse_chartsger$crank, Analyse_chartsger$britcharts, use = "complete.obs", method = "spearman")
  # rho = 0.64 -> starker, positiver Zusammenhang

# C: Zusammenhang zwischen crank und height
  # 1 ordinal, 1 metrisch -> Spearmans Rho / Kendalls Tau
cor(Analyse_chartsger$crank, Analyse_chartsger$height, use = "complete.obs", method = "kendall")
  # tau = 0.12 -> schwacher, positiver Zusammenhang
cor(Analyse_chartsger$crank, Analyse_chartsger$height, use = "complete.obs", method = "spearman")
  # rho = 0.18 -> schwacher, positiver Zusammenhang

# D: Zusammenhang zwischen sfy_streams und crank
  # 1 metrisch, 1 ordinal -> Spearmans rho / Kendalls tau
cor(Analyse_chartsger$sfy_streams, Analyse_chartsger$crank, method = "kendall")
  # tau = 0.04 -> kein Zusammenhang
cor(Analyse_chartsger$sfy_streams, Analyse_chartsger$crank, method = "spearman")
  # rho = 0.06 -> schwacher, positiver Zusammenhang

# E: Zusammenhang zwischen gender und haircol
  # 2 nominalskalierte Variablen -> Cramers V
CramerV(Analyse_chartsger$gender, Analyse_chartsger$haircol)
  # V = 0.30 -> mittelstarker Zusammenhang

# F: Signifikanztests
  # A: sfy_streams & yt_sub
  cor.test(Analyse_chartsger$sfy_streams, Analyse_chartsger$yt_sub)
    # p < 0.05 -> signifikant
  # B: crank & britcharts
  cor.test(Analyse_chartsger$crank, Analyse_chartsger$britcharts, method = "kendall")
    # p < 0.05 -> signifikant
  # C: crank & height
  cor.test(Analyse_chartsger$crank, Analyse_chartsger$height, method = "spearman")
    # p > 0.05 -> nicht signifikant
  # D: sfy_streams & crank
  cor.test(Analyse_chartsger$sfy_streams, Analyse_chartsger$crank, method = "spearman")
    # p > 0.05 -> nicht signifikant
  # E: gender & haircol
  chisq.test(Analyse_chartsger$gender, Analyse_chartsger$haircol, correct = FALSE)
    # p > 0.05 -> nicht signifikant

# G: Visualisierung signifikanter Zusammenhänge
  # sfy_streams & yt_sub
  Analyse_chartsger %>%
    ggplot(aes(x = sfy_streams, y = yt_sub)) +
    geom_point()
  # crank % britcharts
  Analyse_chartsger %>%
    ggplot(aes(x = crank, y = britcharts)) +
    geom_point()

# H: Korrelationsmatrix
  summary(correlation(Analyse_chartsger, method = "kendall"))
  