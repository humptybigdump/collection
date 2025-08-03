rm(list = ls())

# helper function for triangular kernel 
# inputs: vector x of regressor data; point x0 at which to compute E(Y|X=x0); bandwidth h
# output: weight of each observation x
triangular_kernel <- function(x, x0, h){
  # standardized distance between x and x0
  u <- (x-x0)/h
  # initialize weights at zero
  w <- rep(0, length(x))
  # observations that receive weight > 0
  sel <- abs(u) <= 1
  # weight for these observations (others remain at zero)
  w[sel] <- 1-abs(u[sel])
  # return weight vector
  return(w)
}

# function to draw scatter plot with nonparametric regression curve
# inputs: y, x (data vectors for response, regressor),
# h (scalar, bandwidth parameter), n_grid = number of grid points for x0,
# f_true = function to draw for comparison purposes,
# ... additional inputs passed to plot function (optional)
# output: create scatter plot + nonparametric regression curve
plot_local_linear <- function(x, y, h, n_grid = 1e3, f_true = NULL, ...){
  
  # define a grid of values x0 at which to compute regression estimate
  x_grid <- seq(from = min(x), to = max(x), length.out = n_grid)
  
  # set up vector of regression estimates (initially filled with NAs)
  est <- rep(NA, n_grid)
  
  # loop through grid values
  for (jj in 1:n_grid){
    # define observation weights for current value of x0 (= x_grid[jj])
    w_tmp <- triangular_kernel(x, x_grid[jj], h)
    # fit regression using these weights
    fit_tmp <- lm(y~x, weights = w_tmp)
    # estimate = predicted value of y at current value of x0
    est[jj] <- predict(fit_tmp, newdata = data.frame(x = x_grid[jj]))
  }
  
  # make scatter plot
  plot(x, y, pch = 20, bty = "n", col = grey(.5, .5), 
       xlab = "X", ylab = "Y", cex = 1.5, ...)
  # add nonparametric regression curve
  points(x = x_grid, y = est, type = "l", lwd = 2)
  # optionally: add true curve for comparison
  if (!is.null(f_true)){
    points(x = x_grid, y = f_true(x_grid), type = "l", 
           lwd = 2.5, col = "green4")
  }
}

# simulation example
# set random seed
set.seed(20230126)
# sample size
n <- 500
# simulate N(0,1) regressor
x <- rnorm(n)
# define true regression function for simulation (quadratic function)
f_true <- function(z) (z-.08*(z^2))
# simulate y (= f(x) + N(0,1) error)
y <- f_true(x) + rnorm(n)
# run function for different bandwidth choices
plot_local_linear(x = x, y = y, h = .3, f_true = f_true, xlim = c(-3, 3))
plot_local_linear(x = x, y = y, h = .7, f_true = f_true, xlim = c(-3, 3))
plot_local_linear(x = x, y = y, h = 1.2, f_true = f_true, xlim = c(-3, 3))
plot_local_linear(x = x, y = y, h = 20, f_true = f_true, xlim = c(-3, 3))
