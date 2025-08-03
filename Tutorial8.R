### Problem 2

library(dplyr)
library(hdrcde)

rm(list = ls())
load("SPF_inflation.RData")
str(SPF_inflation)

# Training data: Data before 2018
dat_train <- SPF_inflation %>% filter(date < 2018)
# Test data: Latest observation
dat_test <- tail(SPF_inflation, 1)
# Grid for predictive density
y_grid <- seq(from = min(dat_train$outcome), 
              to = max(dat_train$outcome), length.out = 1e3)
# Set up empty data frame (store results later)
df_cde <- df_ml <- data.frame()

# Compute nonparametric densities for all five horizons
for (hh in 1:5){
  cde_tmp <- cde(x = dat_train[,2+hh], y = dat_train$outcome, 
                 x.margin = dat_test[1,2+hh], y.margin = y_grid)
  df_cde <- rbind(df_cde, data.frame(y_grid = y_grid, 
                                     fhat = as.numeric(cde_tmp$z),
                                     horizon = hh))
}

# Compute Gaussian (ML) densities for all five horizons
# in our case, minimizing LS is equal to MLE
# helpful links:
# https://stats.stackexchange.com/questions/143705/maximum-likelihood-method-vs-least-squares-method
# https://stats.stackexchange.com/questions/303232/variance-of-residuals-vs-mle-of-the-variance-of-the-error-term
for (hh in 1:5){
  ml_tmp <- lm(as.formula(paste0("outcome~", "fcst", hh-1)), 
               data = dat_train) 
  ml_tmp2 <- predict(ml_tmp, newdata = dat_test)
  ml_tmp3 <- ml_tmp %>% residuals %>% (function(x) sqrt(mean(x^2)))
  df_ml <- rbind(df_ml, 
                 data.frame(y_grid = y_grid, 
                            fhat = dnorm(y_grid, 
                                         mean = ml_tmp2, sd = ml_tmp3),
                            horizon = hh))
}

# Plot for nonparametric densities
library(ggplot2)
range_fhat <- c(df_cde$fhat, df_ml$fhat) %>% range
plot_cde <- 
  ggplot(df_cde, aes(x = y_grid, y = fhat, 
                     color = as.factor(horizon))) + 
  geom_line() + theme_minimal(base_size = 14) + 
  scale_color_viridis_d(name = "Horizon") + 
  xlab(expression(y)) + 
  ylab(expression(hat(f)(y))) + 
  ylim(range_fhat) + 
  geom_vline(xintercept = dat_test$outcome) + 
  ggtitle("Nonparametric conditional density")
plot_cde

# Plot for Gaussian densities
plot_ml <- 
  ggplot(df_ml, aes(x = y_grid, y = fhat,
                    color = as.factor(horizon))) + 
  geom_line() + theme_minimal(base_size = 14) + 
  scale_color_viridis_d(name = "Horizon") + 
  xlab(expression(y)) + 
  ylab(expression(hat(f)(y))) + 
  ylim(range_fhat) + 
  geom_vline(xintercept = dat_test$outcome) + 
  ggtitle("Gaussian conditional density")
plot_ml

### Problem 3

# Log likelihood as a function of theta
ll <- function(theta, y, x){
  b0 <- theta[1]
  b1 <- theta[2]
  g <- theta[3]
  # Cond. mean of y|x
  m <- b0 + b1*x
  # Need to ensure that nu is > 0
  nu <- exp(theta[4])
  # Scale of y|x 
  scl <-  exp(g*x) #* sqrt(nu/(nu-2))

  # Density for each obs.
  d <- dt((y-m)/scl, df = nu)/scl
  # Return negative (!) log likelihood
  -sum(log(d))
}

# Set seed
set.seed(7531)
# Simulate data
N <- 1e3
X <- runif(N)
Y <- 2*X + exp(X)*rt(N, df = 8)

# Find ML estimator
# Starting values (arbitrary)
theta0 <- c(0,0,0,log(6))#c(1, 1, 1, log(6))
ml <- optim(par = theta0, fn = ll, 
            y = Y, x = X)


library(knitr)
data.frame(b0 = ml$par[1], b1 = ml$par[2], 
           g = ml$par[3], nu = exp(ml$par[4])) %>% 
  kable(digits = 3)
# true values: 0, 2, 1, 8

