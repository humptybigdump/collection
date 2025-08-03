# Exercise 6

# packages
library(np)
library(KernSmooth)

# functions (adapted from Exercise 5)
pred <- function(new_X = x, model_x, model_y) {
  new_X <- sort(new_X)
  X_temp <- model_x
  # remove NAs by values not picked up by which.min
  # X_temp[is.na(model_y)] <- ifelse(median(model_y) > 0, -10^6, 10^6) 
  temp <- numeric(length = length(new_X))
  
  for (i in 1:length(temp)) {
    index_temp <- which.min(abs(new_X[i] - X_temp))
    temp[i] <- model_y[index_temp]
  }
  return(temp)
}

# function to be optimized over, similar to one in g
opt_h_cv <- function(h, nsplits = 10, X_init = data_sim$x, Y_init = data_sim$y1, loss_fun = MSE, pol_ord = 1) {
  # split data randomly and divide into n subgroups
  set.seed(1234)
  index <- 1:length(X_init)
  ran_sample <- sample(index, size = length(index))
  groups <- split(ran_sample, sort(ran_sample%%nsplits))
  
  CV_MSE <- numeric(length = nsplits)
  
  for(i in 1:nsplits) {
    X_cv <- X_init[-groups[[i]]]
    X_test <- X_init[groups[[i]]]
    Y_cv <- Y_init[-groups[[i]]]
    Y_test <- Y_init[groups[[i]]]
    
    loc_pol_temp <- locpoly(X_cv, Y_cv,degree = pol_ord,bandwidth = h)
    y_hat <- pred(new_X = X_test, loc_pol_temp$x, loc_pol_temp$y)
    CV_MSE[i] <- mean((Y_test-y_hat)^2)
  }
  return(mean(CV_MSE))
}

opt_all <- function(lower, upper, max_degree = 5,...) {
  Models <- matrix(nrow = max_degree + 1, ncol = 2, dimnames = list(as.character(c("Constant", "Linear", 2:max_degree)), c("MSE","h")))
  
  for(p in 0:max_degree) {
    temp <- optimize(opt_h_cv, interval = c(lower,upper), pol_ord = p,...)
    Models[p+1,2] <- temp$minimum
    Models[p+1,1] <- temp$objective
  }
  return(Models)
}

####################
# Problem 1
# a - nonpar. reg.
####################

# load data
data_sim <- read.table("data_sim.txt")

# use nonparametric regressions
non_para_regression <- function(x = data_sim$x, y = data_sim$y1, ...) {
  reg_lc_cv <- npreg(y~x, regtype = "lc", bwmethod = "cv.ls", resid = TRUE, ...)
  reg_ll_cv <- npreg(y~x, regtype = "ll", bwmethod = "cv.ls", resid = TRUE, ...)
  
  reg_lc_aic <- npreg(y~x, regtype = "lc", bwmethod = "cv.aic", resid = TRUE, ...)
  reg_ll_aic <- npreg(y~x, regtype = "ll", bwmethod = "cv.aic", resid = TRUE, ...)
  
  MSE <- cbind(c("reg_lc_cv", "reg_ll_cv", "reg_lc_aic", "reg_ll_aic"),
               c(reg_lc_cv$MSE, reg_ll_cv$MSE, reg_lc_aic$MSE, reg_ll_aic$MSE))
  
  return(list(MSE = MSE,
              Regressions = list(lc_cv = reg_lc_cv,
                                 ll_cv = reg_ll_cv,
                                 lc_aic = reg_lc_aic,
                                 ll_aic = reg_ll_aic)))
}

set.seed(1213)
reg_1_ck1 <- non_para_regression(data_sim$x, data_sim$y1)
reg_2_ck1 <- non_para_regression(data_sim$x, data_sim$y2)

reg_locpol_1 <- opt_all(0, 10, nsplits = 10)
reg_locpol_2 <- opt_all(0, 10, Y_init = data_sim$y2)

# check MSE values
reg_1_ck1$MSE
reg_2_ck1$MSE
reg_locpol_1
reg_locpol_2

# for y1, local linear with cross-validation is best
# for y2, local constant is best according to locpol, local linear with AIC with npreg

plot(reg_1_ck1$Regressions$ll_cv, ylim = c(-2,4))
points(data_sim$x, data_sim$y1)
lines(sort(reg_1_ck1$Regressions$lc_cv$eval[,1]),
      reg_1_ck1$Regressions$lc_cv$mean[order(reg_1_ck1$Regressions$ll_cv$eval[,1])],
      col = "blue")

plot(reg_2_ck1$Regressions$ll_aic, ylim = c(-2,4))
points(data_sim$x,data_sim$y1)
lines(sort(reg_2_ck1$Regressions$lc_cv$eval[,1]),
      reg_2_ck1$Regressions$lc_cv$mean[order(reg_2_ck1$Regressions$lc_cv$eval[,1])], col = "blue")

####################
# b - find par. model
####################

# Could be linear? Quadratic? Third Order?
# fit least squares models using lm and nls
?nls
n <- length(data_sim$x)
linear_1 <- lm(y1~x, data = data_sim)
summary(linear_1)
# y1 = 0.912 - 0.86*x
mse_linear_1 <- sum(linear_1$residuals^2)/n
reg_1_ck1$MSE
mse_linear_1

# third_order_1 <- nls(y1~a*x+b*x^2+c*x^3, data = data_sim, start = c(a = 1, b = 1, c = 1))
x_2 <- data_sim$x^2
x_3 <- data_sim$x^3
# predict(third_order_1, sort(data_sim$x))
linear_1_cubic <- lm(data_sim$y1~poly(data_sim$x, degree = 3))
mse_cubic <- sum(linear_1_cubic$residuals^2)/n
summary(linear_1_cubic)
# y1 = 0.49 - 5.55*x + 0.83*x^2 - 1.90*x^3

plot(reg_1_ck1$Regressions$ll_cv, ylim = c(0,2))
points(data_sim$x, data_sim$y1)
lines(sort(data_sim$x), linear_1_cubic$fitted.values[order(data_sim$x)], col = "blue")
# lines(sort(data_sim$x), predict(third_order_1, data_sim$x)[order(data_sim$x)], col = "red")
abline(linear_1, col = "red")

# looks a bit like density --> normal density function? 
# But not straight, so maybe including linear part?

plot(reg_2_ck1$Regressions$lc_cv, ylim = c(-2,4))
points(data_sim$x, data_sim$y1)

# add linear function a+b*x
# add exponential part exp(-c(x-d)^2) that looks similar to normal distribution
# the more variables, the harder to optimize: leave out normalizing part of distribution

# pick smart starting values! Very important:
# a: starting point for linear part, close to 1 --> a=1
# b: slope of linear part, maybe 0.5?
# c & d: tricky, you want to simulate kink: 
# d: similar to mean of normal, so maybe 0.5
# c: standard deviation looks rather small, maybe between 0 and 0.5? --> choose c = 50
exp_order_2 <- nls(y2~a+b*x + exp(-c*(x-d)^2), data = data_sim, start = c(a = 1, b = -0.5, c = 50, d = 0.5), algorithm = "port")
exp_order_2
# y2 = 0.90 - 1.00*x + exp(-124.70*(x - 0.51)^2) 

lines(sort(data_sim$x), predict(exp_order_2, data_sim$x)[order(data_sim$x)], col="red")
exp_order_2
