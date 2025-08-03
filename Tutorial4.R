rm(list = ls())
library(reshape2)
library(dplyr)
library(ggplot2)

library(zoo)

# Load data
# (Current quarter `nowcasts' of US GDP growth)
# Realization variable called "value", forecast variable called "spf"
df <- read.table("spf_nowcasts.csv", sep = ",", header = TRUE)

ggplot(melt(df, id.vars = "date"), aes(x = as.yearqtr(date), y = value, color = variable)) + 
  geom_line() + 
  scale_color_viridis_d(name = "", breaks = c("spf", "value"),
                                      labels = c("SPF", "Realization")) + 
  theme_minimal(base_size = 14) + 
  xlab("Year") + 
  theme(legend.position = "top") + 
  ylab("Growth (%, annualized)")


# Alternative: Make time series plot
pl1 <- ggplot(df, aes(x = 1:nrow(df))) + 
  geom_line(aes(y = value, color = "Realization")) +
  geom_line(aes(y = spf, color = "SPF")) + 
  #scale_color_viridis_d(name = "", breaks = c("spf", "value"),
  #                      labels = c("SPF", "Realization")) + 
  theme_minimal(base_size = 14) + 
  xlab("Year") + 
  ylab("Growth (%, annualized)") + 
  scale_x_discrete(labels= date)

## Show time series plot
pl1


# Make scatter plot of $Y$ on $\hat Y$ 

pl2 <- ggplot(df, aes(x = spf, y = value)) + geom_point(alpha = .5) + 
  theme_minimal(base_size = 14) + 
  xlab("SPF") + 
  ylab("Realization") + 
  geom_smooth(method = "lm", se = FALSE) + 
  xlim(c(-12, 12)) + 
  ylim(c(-12, 12))

# Show scatter plot
pl2



# Mincer-Zarnowitz Regression

fit <- lm(value~spf, data = df)
summary(fit)$r.squared %>% round(3)

#Coefficient estimates
coef(fit) %>% round(3)


# 95% confidence intervals
library(lmtest)
library(sandwich)
coefci(fit, vcov. = NeweyWest) %>% round(3)


# Diebold-Mariano Regression
dm <- function(x1, x2, y){
  d <- (y-x1)^2 - (y-x2)^2
  fit <- lm(d~1)
  coefci(fit, vcov. = NeweyWest)
}
df0 <- df %>% mutate(rw = c(NA, value[-nrow(df)])) %>% na.omit

dm(df0$spf, df0$rw, df0$value)

