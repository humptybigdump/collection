# Exercise_5_R_Code

# packages
library(np)
library(KernSmooth)

# Problem 1
####################
# a - read in data
####################
setwd("/Users/lotta/Desktop/Nicht- und Semiparametrik")

# copy file in your working directory or insert location here 
labor_data <- read.table("eps71.txt", header = TRUE)
n <- dim(labor_data)[1] # number of rows
X <- labor_data$age
y <- labor_data$logwage

summary(labor_data)

####################
# b - plot data
####################
plot(labor_data$age, labor_data$logwage, ylab = "Y: Log-Einkommen", xlab = "X: Alter")

####################
# c - linear model
####################
lab_lm <- lm(logwage~age,data = labor_data) # linear model
abline(lab_lm, col = "black")

####################
# d - LCR
####################
?npregbw
# get cross-validated bandwidth
bw_nw <- npregbw(logwage~age, data = labor_data, regtype = "lc") # local constant (Nadaraya-Watson)

# use Silverman's bandwidth
silv_bw <- 1.06*sd(labor_data$age)*n^(-1/5)
lab_lcr_lscv <- npreg(bw_nw) # local constant regression
lab_lcr_silv <- npreg(logwage~age, data = labor_data, bws = silv_bw, bandwidth_compute = FALSE) # local constant w/ Silverman's bandwidth
lab_lcr_silv_locpoly <- locpoly(X, y, degree = 0, kernel = "normal", bandwidth = silv_bw) # local constant w/ Silverman's bandwidth

# plot fitted values
# x - ...$eval; y_hat - ...$mean
lines(t(lab_lcr_lscv$eval), lab_lcr_lscv$mean, col = "blue", lty = 2, lwd = 2)
lines(t(lab_lcr_silv$eval), lab_lcr_silv$mean, col = "red", lty = 1, lwd = 2)
lines(lab_lcr_silv_locpoly$x, lab_lcr_silv_locpoly$y, col = "green", lty = 2, lwd = 2)

# compare results
lcr_silv <- c(lab_lcr_silv$R2, lab_lcr_silv$MSE, lab_lcr_silv$MAE)
names(lcr_silv) <- c("R2","MSE","MAE")
lcr_lscv <- c(lab_lcr_lscv$R2, lab_lcr_lscv$MSE, lab_lcr_lscv$MAE)
names(lcr_lscv) <- c("R2", "MSE", "MAE")

cp_lcr <- cbind(lcr_silv, lcr_lscv)
colnames(cp_lcr) <- c("Local Constant Silverman BW", "Local Constant Cross-Validated BW")
cp_lcr

# compare with manual coding
k_gauss <- function(x) { exp(-x^2 /2)/sqrt(2*pi) }
exp_nw <- function(x, X_vec = X, Y = y, h = silv_bw) {
  sum(Y*k_gauss((X_vec-x)/h))/sum(k_gauss((X_vec-x)/h))
}
y_vec_nw <- sapply(X, exp_nw) # local constant regression

plot(labor_data$age, labor_data$logwage, ylab = "Y: Log-Einkommen", xlab = "X: Alter")
lines(X, y_vec_nw)
lines(t(lab_lcr_silv$eval), lab_lcr_silv$mean, col = "red", lty = 1, lwd = 2)
# they are the same

####################
# e - LLR & LPR
####################
# compute local polynomials using locpoly function from KernSmooth Package
# different implementation, uses bin approximations for performance reasons
# does not give back estimates for original data,
# but pools data in equally spaced bins and gives back estimation for those

# compute estimates using Silverman's bandwidth
bw_ll <- npregbw(logwage~age, data = labor_data, regtype = "ll") # local linear
lab_llr_lscv <- npreg(bw_ll) # local linear regression
lines(t(lab_llr_lscv$eval), lab_llr_lscv$mean, col = "violet", lty = 2, lwd = 2)

lcr <- locpoly(X, y, degree = 0, kernel = "normal", bandwidth = silv_bw)
llr <- locpoly(X, y, degree = 1, kernel = "normal", bandwidth = silv_bw)
lpr2 <- locpoly(X, y, degree = 2, kernel = "normal", bandwidth = silv_bw)
lpr3 <- locpoly(X, y, degree = 3, kernel = "normal", bandwidth = silv_bw)
lpr4 <- locpoly(X, y, degree = 4, kernel = "normal", bandwidth = silv_bw)
lpr5 <- locpoly(X, y, degree = 5, kernel = "normal", bandwidth = silv_bw)

# new plot
plot(labor_data$age, labor_data$logwage, ylab = "Y: Log-Einkommen", xlab = "X: Alter")
lines(lcr$x, lcr$y, lwd = 2, col = "black")
lines(llr$x, llr$y, lwd = 2, col = "red")
lines(lpr2$x, lpr2$y, lwd = 2, col = "blue")
lines(lpr3$x, lpr3$y, lwd = 2, col = "green")
lines(lpr4$x, lpr4$y, lwd = 2, col = "violet")
lines(lpr5$x, lpr5$y, lwd = 2, col = "orange")

# function to obtain values closest to X
pred <- function(new_X = X, model_x, model_y)
{
  X_temp <- model_x
  # remove NAs by values not picked up by which.min
  # X_temp[is.na(model_y)] <- ifelse(median(model_y) > 0, -10^6, 10^6) 
  temp <- numeric(length = length(new_X))
  for (i in 1:length(temp))
  {
    index_temp <- which.min(abs(new_X[i] - X_temp))
    temp[i] <- model_y[index_temp]
  }
  return(temp)
}

# predict values
lcr_y <- pred(X, lcr$x, lcr$y)
llr_y <- pred(X, llr$x, llr$y)
lpr2_y <- pred(X, lpr2$x, lpr2$y)
lpr3_y <- pred(X, lpr3$x, lpr3$y)
lpr4_y <- pred(X, lpr4$x, lpr4$y)
lpr5_y <- pred(X, lpr5$x, lpr5$y)

# compare MSE (here equal to squared residuals)
mse_lcr <- mean((lcr_y-y)^2)
mse_llr <- mean((llr_y-y)^2)
mse_lpr2 <- mean((lpr2_y-y)^2)
mse_lpr3 <- mean((lpr3_y-y)^2)
mse_lpr4 <- mean((lpr4_y-y)^2)
mse_lpr5 <- mean((lpr5_y-y)^2)

mse_sum <- c(mse_lcr, mse_llr, mse_lpr2, mse_lpr3, mse_lpr4, mse_lpr5)
names(mse_sum) <- c("0", "1", "2", "3", "4", "5")
mse_sum

# local polynomials are more flexible and thus better in approximating data
# this comes at a cost of higher variance
# here difference in estimators is larger when fewer points are available

# compute estimates for h = 80
llr_large <- locpoly(X, y, degree = 1, kernel = "normal", bandwidth = 80)
lpr2_large <- locpoly(X, y, degree = 2, kernel = "normal", bandwidth = 80)
lpr3_large <- locpoly(X, y, degree = 3, kernel = "normal", bandwidth = 80)
lpr4_large <- locpoly(X, y, degree = 4, kernel = "normal", bandwidth = 80)
lpr5_large <- locpoly(X, y, degree = 5, kernel = "normal", bandwidth = 80)

plot(labor_data$age, labor_data$logwage, ylab = "Y: Log-Einkommen", xlab = "X: Alter")
lines(llr_large$x, llr_large$y, lwd = 2, col = "red")
lines(lpr2_large$x, lpr2_large$y, lwd = 2, col = "blue")
lines(lpr3_large$x, lpr3_large$y, lwd = 2, col = "green")
lines(lpr4_large$x, lpr4_large$y, lwd = 2, col = "violet")
lines(lpr5_large$x, lpr5_large$y, lwd = 2, col = "orange")
# just one function for approximation

####################
# f - MSE for h-grid
####################
# hist(y) # -> data non-normal!
h_test <- seq(0.1, 20, by = 0.1)

# input: degree, vector of possible bandwidths, Y,X
# output: MSE for each bandwidth
mse_plot <- function(pol_degree, bandwidths = h_test, Y_orig = y, X_orig = X)
{
  y_h <- numeric(length = length(bandwidths))
  for (i in 1:length(y_h))
  {
    loc_poly_model <- locpoly(X_orig, Y_orig, degree = pol_degree, kernel = "normal", bandwidth = bandwidths[i])
    y_h[i] <- mean((pred(new_X = X_orig, model_x = loc_poly_model$x, model_y = loc_poly_model$y) - Y_orig)^2)
  }
  return(y_h)
}

# plot results for d = 0, 1, 2, 3
plot(h_test, mse_plot(0), xlab = "bandwidths", ylab = "MSE", ylim = c(0.2,0.4), type = "l", xlim = c(0, max(h_test)))
lines(h_test, mse_plot(1), col = "red")
lines(h_test, mse_plot(2), col = "blue")
lines(h_test, mse_plot(3), col = "green")
lines(h_test, mse_plot(4), col = "violet")
lines(h_test, mse_plot(5), col = "orange")

# depending on polynomial degree, we have different MSE minimal bandwidths
# we see that we can use larger bandwidths for local polynomial estimators
# since they use smooth functions that can adapt to the data much better

####################
# g - 10-fold CV
####################

# so far, we only used in-sample measures
# now, use cross-validation

# Cross Validation Function for polynomial np regression
cross_valid <- function(nsplits = 10, X_init = X, Y_init = y, loss_fun = MSE, pol_max = 5, h = silv_bw) {
  # split data randomly and divide into n subgroups
  set.seed(1234) # set.seed(1234)
  index <- 1:length(X_init)
  ran_sample <- sample(index, size = length(index))
  groups <- split(ran_sample, sort(ran_sample%%nsplits)) # %% modulo
  
  CV_MSE <- matrix(nrow = nsplits, ncol = pol_max + 2)
  if(pol_max == 5)
  {
    colnames(CV_MSE)[1:7] <- c("OLS", "Local Constant", "Local Linear", "Local Polynomial 2", "Local Polynomial 3", "Local Polynomial 4", "Local Polynomial 5")
  }
  for(i in 1:nsplits)
  {
    X_cv <- X_init[-groups[[i]]]
    X_test <- X_init[groups[[i]]]
    Y_cv <- Y_init[-groups[[i]]]
    Y_test <- Y_init[groups[[i]]]
    
    lm_temp <- lm(Y_cv~X_cv)
    y_hat <- predict(lm_temp, data.frame(X_cv = X_test))
    CV_MSE[i,1] <- loss_fun(Y_test, y_hat)
    
    for(p in 0:pol_max)
    {
      loc_pol_temp <- locpoly(X_cv, Y_cv, degree = p, bandwidth = h)
      y_hat <- pred(new_X = X_test, loc_pol_temp$x, loc_pol_temp$y)
      CV_MSE[i,p+2] <- loss_fun(Y_test, y_hat)
    }
    
  }
  return(colMeans(CV_MSE))
}


MSE <- function(y_orig, y_pred) {
  mean((y_orig - y_pred)^2)
}

opt_silv <- cross_valid(); opt_silv

####################
# h - double optim.
####################
# now optimize over h for each polynomial degree

# function to be optimized over, similar to one in g
opt_h_cv <- function(h, nsplits = 10, X_init = X, Y_init = y, loss_fun = MSE, pol_ord = 1) {
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
  
    loc_pol_temp <- locpoly(X_cv, Y_cv, degree = pol_ord, bandwidth = h)
    y_hat <- pred(new_X = X_test, loc_pol_temp$x, loc_pol_temp$y)
    CV_MSE[i] <- mean((Y_test-y_hat)^2)
  }
  return(mean(CV_MSE))
}

opt_all <- function(lower, upper, max_degree = 5,...) {
  Models <- matrix(nrow = max_degree + 1, ncol = 2, dimnames = list(as.character(c("Local Constant", "Local Linear", paste("Local Polynomial", 2:max_degree))), c("MSE", "h")))
  
  for(p in 0:max_degree) {
    temp <- optimize(opt_h_cv, interval = c(lower,upper), pol_ord = p,...)
    Models[p+1,2] <- temp$minimum
    Models[p+1,1] <- temp$objective
  }
  return(Models)
}

opt_h <- opt_all(0, 60); opt_h
opt_h[which.min(opt_h[,1]),]
# polynomial order 3 is already smooth enough to approximate functions over full support
# smaller bandwidth than 50 will likely only increase MSE here

# alternative: kernel = "epanech"
lcr_opt <- locpoly(X, y, degree = 0, kernel = "normal", bandwidth = opt_h[1,2])
llr_opt <- locpoly(X, y, degree = 1, kernel = "normal", bandwidth = opt_h[2,2])
lpr2_opt <- locpoly(X, y, degree = 2, kernel = "normal", bandwidth = opt_h[3,2])
lpr3_opt <- locpoly(X, y, degree = 3, kernel = "normal", bandwidth = opt_h[4,2])
lpr4_opt <- locpoly(X, y, degree = 4, kernel = "normal", bandwidth = opt_h[5,2])
lpr5_opt <- locpoly(X, y, degree = 5, kernel = "normal", bandwidth = opt_h[6,2])

plot(labor_data$age, labor_data$logwage, ylab = "Y: Log-Einkommen", xlab = "X: Alter")
lines(lcr_opt$x, lcr_opt$y, lwd = 2, col = "black")
lines(llr_opt$x, llr_opt$y, lwd = 2, col="red")
lines(lpr2_opt$x, lpr2_opt$y, lwd = 2, col = "blue")
lines(lpr3_opt$x, lpr3_opt$y, lwd = 2, col = "green")
lines(lpr4_opt$x, lpr4_opt$y, lwd = 2, col = "violet")
lines(lpr5_opt$x, lpr5_opt$y, lwd = 2, col = "orange")



# Problem 2
####################
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
  
  MSE <- c(reg_lc_cv$MSE, reg_ll_cv$MSE, reg_lc_aic$MSE, reg_ll_aic$MSE)
  names(MSE) <- c("reg_lc_cv", "reg_ll_cv", "reg_lc_aic", "reg_ll_aic")
  
  return(list(MSE = MSE,
              Regressions = list(lc_cv = reg_lc_cv,
                                 ll_cv = reg_ll_cv,
                                 lc_aic = reg_lc_aic,
                                 ll_aic = reg_ll_aic)))
}

set.seed(1213)
reg_1_ck1 <- non_para_regression(data_sim$x, data_sim$y1)
reg_2_ck1 <- non_para_regression(data_sim$x, data_sim$y2)

reg_locpol_1 <- opt_all(0, 10, X_init = data_sim$x, Y_init = data_sim$y1)
reg_locpol_2 <- opt_all(0, 10, X_init = data_sim$x, Y_init = data_sim$y2)

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


