rm(list = ls())

library(dplyr)
setwd("C:/Users/Andreas Eberl/Documents/Lehrveranstaltungen/applied_econometrics_2023_24/Tutorials/7")

df_rounded <- read.csv("table_review2.csv") %>% round(2)
#'df_exact <- df_rounded %>% 
#'transmute(y = round(y, 2), x = round(x, 2), x2 = x^2, xy = x*y) %>%
#'arrange(x)

c <- 2; h <- 1

# Select observations within bandwidth on positive (p)
# and negative (m) side
df4_p <- df_rounded %>% filter(x >= c & x < c+h) %>% mutate(xm2 = x-c)
df4_m <- df_rounded %>% filter(x < c & x >= c-h) %>% mutate(xm2 = x-c)

# Compute RD estimates
coef_p_exact <- lm(y~xm2, data = df4_p) %>% coefficients
coef_m_exact <- lm(y~xm2, data = df4_m) %>% coefficients
# Check "by hand"
coef_p_rounded <- df4_p %>% 
  summarise(slope = (mean(xy)-mean(x)*mean(y))/(mean(x2) - (mean(x)^2)),
            int = mean(y) - slope*(mean(x)-c))
coef_m_rounded <- df4_m %>% 
  summarise(slope = (mean(xy)-mean(x)*mean(y))/(mean(x2) - (mean(x)^2)),
            int = mean(y) - slope*(mean(x)-c))

# Estimate sharp regression discontinuity effect
tau_SRD_exact <- coef_p_exact["(Intercept)"] - coef_m_exact["(Intercept)"]
tau_SRD_rounded <- coef_p_rounded["int"] - coef_m_rounded["int"]
tau_SRD_exact
tau_SRD_rounded

# Create scatterplot with line at cutoff
plot(df_rounded$x, df_rounded$y, pch = 16, xlab = "x", ylab = "y")
abline(v = c, lty = 3); mtext(expression(c), at = 2)
# Draw line for upper bandwidth limit and color points accordingly
abline(v = c+h, lty = 3, col = "green"); mtext(expression(c+h), at = c+h, col = "green")
points(df4_p$x, df4_p$y, pch = 16, col = "green")
# Draw line for lower bandwidth limit and color points accordingly
abline(v = c-h, lty = 3, col = "red"); mtext(expression(c-h), at = c-h, col = "red")
points(df4_m$x, df4_m$y, pch = 16, col = "red")
# Draw regression line after cutoff
abline(coef = c(coef_p_exact[1]-c*coef_p_exact[2],
                coef_p_exact[2]),
       col = "green", lty = 2)
segments(x0 = c, y0 = coef_p_exact[1],
         x1 = c+h, y1 = coef_p_exact[1] + h*coef_p_exact[2],
         col = "green", lwd = 2)
# Draw regression line before cutoff
abline(coef = c(coef_m_exact[1]-c*coef_m_exact[2],
                coef_m_exact[2]),
       col = "red", lty = 2)
segments(x0 = c, y0 = coef_m_exact[1],
         x1 = c-h, y1 = coef_m_exact[1] - h*coef_m_exact[2],
         col = "red", lwd = 2)
