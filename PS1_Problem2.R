rm(list = ls())

library(dplyr)

## Data preparation
# Enter directory where wagedata.csv is located
setwd("C:/Users/Andreas Eberl/Documents/Lehrveranstaltungen/microeconometrics_2024/Tutorials/1")

# Set size of training sample (needed below)
n_train <- 4e4

# Load data, define new wage and region variables
dataset <- read.csv("wagedata.csv", sep = ",", header = TRUE)
dat <- dataset %>%
  mutate(wage = earnings/(hours*weeks), 
         region2 = region == 2, region3 = region == 3, 
         region4 = region == 4, lwage = log(wage))
dat_train <- dat[1:n_train, ]
dat_test <- dat[(n_train+1):nrow(dat), ]


## Estimate non-linear regression model
# Define objective function
ff <- function(b, y, x){
  pred <- exp(x%*%b) # predictions (x in matrix form)
  return(sum((y - pred)^2)) # return sum of squared prediction errors
}
# Define gradient
gg <- function(b, y, x){
  pred <- exp(x%*%b) # predictions in matrix form
  e <- (y - pred)
  return(-2* (t(x) %*% (e*pred)))
}
# Collect relevant variables in vector
v_sel <- c("female", "education", "region2", "region3", "region4")
# X matrix from training sample (select via vector v_sel)
x_train <- cbind(1, as.matrix(dat_train[, v_sel])) 
# Use all zeros as starting values
sv <- rep(0, 6)
# Use optim function for optimization
# Note that method "BFGS" yields more stable results here
opt <- optim(sv, fn = ff, gr = gg, y = dat_train$wage, 
             x = x_train, method = "BFGS") 
b_est <- opt$par # estimated parameter vector

# Check using different starting values
# Simulate vector of starting values close to zero
sv_check <- runif(6, min = -1e-3, max = 1e-3)
# Use same optimization setup as before
opt_check <- optim(sv_check, fn = ff, gr = gg, y = dat_train$wage, 
                   x = x_train, method = "BFGS") # 

# Print "original" and "check" parameter estimates
cbind(b_est, opt_check$par)


## Compute marginal effects for each observation
me <- exp(x_train %*% b_est)*b_est[3]
# Print summary statistics
summary(me)
# Print observation with highest me
x_train[which.max(me),]

## Estimate linear model for log wage
fit1 <- lm(lwage~female+education+region2+region3+region4,
           data = dat_train)
# Compare parameter estimates to prior model
cbind(coefficients(fit1), b_est)
# Compute naive prediction (ignoring non-linearity)
pred1 <- predict(fit1, newdata = dat_test) %>% exp
# Compute mean squared error
mse1 <- ((dat_test$wage-pred1)^2) %>% mean
# Compute predictions based on assumption that log wage is normally distributed
pred1b <- (predict(fit1, newdata = dat_test) + .5*mean(fit1$residuals^2)) %>%
  exp
# Compute mean squared error
mse1b <- ((dat_test$wage-pred1b)^2) %>% mean

# Compute predictions
# Define X matrix for test sample (again select variables via v_sel)
x_test <- cbind(1, as.matrix(dat_test[, v_sel])) 
# Compute predictions
pred2 <- exp(x_test %*% b_est)
# Compute mean squared error
mse2 <- mean( (dat_test$wage - pred2)^2 )

# Compare predictions to outcomes
pred_all <- data.frame(pred1, pred1b, pred2, wage = dat_test$wage)
round(c(mse1, mse1b, mse2), 2)
