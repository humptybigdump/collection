# Non- and Semiparametrics
# Exercise 1
# Fall 2023

# 1
###########

set.seed(1337) # for reproducible results

n <- 100
lambda <- 0.5

z_i <- numeric()

for (i in 1:n) {
  if (runif(1) < lambda) {
    z_i[i] <- rnorm(1, 165, sqrt(6))
  } else {
    z_i[i] <- rnorm(1, 180, sqrt(7))
  }
}


# 2
###########

F_n <- function(z) {
  1 / n * sum(z_i <= z)
}

values <- sapply(sort(z_i), F_n)

plot(sort(z_i), values, type = "s", 
     xlab = "z", ylab = expression("F"["n"]*"(z)"))

# use R-Function
F_n_2 <- ecdf(z_i)
plot(F_n_2)


# 3
###########

f_hat <- function(z, h) {
  (F_n(z + h) - F_n(z - h)) / (2 * h)
}

# define points where to evaluate the function
grid <- seq(min(z_i), max(z_i), length.out = 1000)

values2 <- sapply(X = grid, FUN = f_hat, h = 1)
plot(grid, values2, type = "l", ylab = "Density", xlab = "z")

# This is equivalent to KDE with a uniform kernel!

# 4
###########

k_gauss <- function(z) {
  1 / sqrt(2 * pi) * exp(-0.5 * z^2)
}
# you can also use dnorm to get a gauss kernel, i.e. k_gauss<-function(z){dnorm(z)}


# 4a
###########

z_test <- 177

values_gauss <- sapply((z_i - z_test) / 1, k_gauss)
plot(z_i, values_gauss, ylab = "K((Z_i - z)/ h)")

# sum over all i and divide by n*h
sum(values_gauss) / (n * 1)

# use f_hat_gauss
f_hat_gauss <- function(z, h) {
  1 / (n * h) * sum(k_gauss((z_i - z) / h))
}

f_hat_gauss(177, 1)

f_hat_gauss(165, 1)


# 4b
###########

# recall the plot from 3:
grid <- seq(min(z_i), max(z_i), length.out = 1000)
values2 <- sapply(X = grid, FUN = f_hat, h = 1)
plot(grid, values2, type = "l", ylab = "Density", xlab = "z")

# add estimated density
lines(grid, sapply(grid, f_hat_gauss, h = 1), col = "blue", lwd = 2)

# add true density (not available in real data sets)
f_z <- function(z) {
  0.5 * dnorm(z, 180, sqrt(7)) + 0.5 * dnorm(z, 165, sqrt(6))
}

lines(grid, sapply(grid, f_z), col = "red", lwd = 2)

legend("top", c("Uniform", "Gaussian", "True density"), col = c("black", "blue", "red"), lty = 1, lwd = 1)


# 5
###########

h_plot <- c(0.025, 0.25, 0.5, 1, 2, 4, 8)
grid <- seq(min(z_i), max(z_i), length.out = 1000)

for (h in h_plot) {
  par(mfrow = c(2, 1))
  
  f_true <- 0.5 * dnorm(grid, 180, sqrt(7)) + 0.5 * dnorm(grid, 165, sqrt(6))
  f_uniform <- sapply(X = grid, FUN = f_hat, h = h)
  f_gauss <- sapply(grid, f_hat_gauss, h = h)
  
  y_lims <- range(f_true, f_uniform, f_gauss)
  
  plot(grid, f_true, type = "l", ylab = "Density", xlab = "Z", main = paste("Uniform, h=", h), ylim = y_lims)
  lines(grid, f_uniform, type = "l", col = "blue")
  
  plot(grid, f_true, type = "l", ylab = "Density", xlab = "Z", main = paste("Gaussian, h=", h), ylim = y_lims)
  lines(grid, f_gauss, col = "blue")
  
  Sys.sleep(0.7) # short pause after each plot to view development
}


# 6
###########
graphics.off()
# a
###########

# use grid search (problem not necessarily convex):
# + easy to implement
# - dependent on prior knowledge/choice of grid

grid_opt <- seq(0, 4, length.out = 1000)

h_err <- numeric()

# calculate l2 error for each h in grid and save in vector
for (i in 1:length(grid_opt)) {
  h_temp <- grid_opt[i]
  h_err[i] <- sqrt(sum((sapply(z_i, f_hat, h = h_temp) - sapply(z_i, f_z))^2))
}

# look at plot of errors
plot(grid_opt, h_err, type = "l", ylab = "Error", xlab = "h")

# choose better window
plot(grid_opt, h_err, type = "l", ylim = c(0, 0.4), ylab = "Error", xlab = "h")

h_star <- grid_opt[which.min(h_err)]
h_star

abline(v = h_star, col = "green", lty = 2)


h_err_gauss <- numeric()

# calculate l2 error for each h in grid and save in vector
for (i in 1:length(grid_opt)) {
  h_temp <- grid_opt[i]
  h_err_gauss[i] <- sqrt(sum((sapply(z_i, f_hat_gauss, h = h_temp) - sapply(z_i, f_z))^2))
}

h_gauss_star <- grid_opt[which.min(h_err_gauss)]
h_gauss_star

# b
###########

plot(grid, sapply(grid, f_z), type = "l", ylab = "Density", xlab = "Z", main = , ylim = c(0, 0.15))
lines(grid, sapply(grid, f_hat, h = h_star), col = "blue")
lines(grid, sapply(grid, f_hat_gauss, h = h_gauss_star), col = "red")
legend("topright", c("True density", "Uniform", "Gaussian"), col = c("black", "blue", "red"), lty = 1, lwd = 1)


# c
###########

# Squared Bias: bias at point z

# Integrated squared bias: for whole sample, global criterion


# d
###########


## i
###########
grid <- seq(min(z_i), max(z_i), length.out = 100)

# use uniform kernel from -1 to 1 for f_hat
k_unif <- function(z) {
  dunif(z, -1, 1)
}

h_grid <- 0.1 * (1:150)
j <- 110 # to obtain Expectation, we need to draw several samples j

# each column contains a sample with n observations
Z_i <- matrix(NA, n, j)

set.seed(1337)
for (k in 1:j) {
  for (i in 1:n) {
    if (runif(1) < lambda) {
      Z_i[i, k] <- rnorm(1, 165, sqrt(6))
    } else {
      Z_i[i, k] <- rnorm(1, 180, sqrt(7))
    }
  }
}

exp_f_hat_gauss <- function(Z, z, h) {
  f_temp <- numeric()
  for (i in 1:ncol(Z)) {
    f_temp[i] <- 1 / (n * h) * sum(k_gauss((Z[, i] - z) / h))
  }
  return(mean(f_temp))
}

squared_bias_gauss <- function(z, h) {
  (exp_f_hat_gauss(Z_i, z, h) - f_z(z))^2
}

sq_bias_gauss <- numeric()
for (i in 1:length(h_grid)) {
  sq_bias_gauss[i] <- integrate(squared_bias_gauss, min(grid), max(grid), h = h_grid[i], subdivisions = 2000)$value
}

exp_f_hat <- function(Z, z, h) {
  f_temp <- numeric()
  for (i in 1:ncol(Z)) {
    f_temp[i] <- 1 / (n * h) * sum(k_unif((Z[, i] - z) / h))
  }
  return(mean(f_temp))
}

squared_bias_hat <- function(z, h) {
  (exp_f_hat(Z_i, z, h) - f_z(z))^2
}

sq_bias_hat <- numeric()
for (i in 1:length(h_grid)) {
  sq_bias_hat[i] <- integrate(squared_bias_hat, min(grid), max(grid), h = h_grid[i], subdivisions = 2000)$value
}

plot(h_grid, sq_bias_hat, type = "l", ylim = range(sq_bias_gauss, sq_bias_hat), 
     xlab = "h", ylab = "Estimated integrated squared bias")
lines(h_grid, sq_bias_gauss, col = "blue")
legend("top", c("Uniform", "Gaussian"), col = c("black", "blue"), lty = 1, lwd = 1)


# Integrated squared bias goes up when h increases 
# Goes down in the beginning --> depends on density structure
# results (low h ->low bias) are valid asymptotically (i.e. for  n->Inf),
# integration is computationally expensive, but you can try out n>>100


## ii
###########

# calculate variance for each z and h

# uniform kernel
var_hat <- function(z, h) {
  f_temp <- numeric()
  for (i in 1:ncol(Z_i)) {
    f_temp[i] <- 1 / (n * h) * sum(k_unif((Z_i[, i] - z) / h))
  }
  return(mean((f_temp - exp_f_hat(Z_i, z, h))^2))
}

# Gaussian kernel
var_gauss <- function(z, h) {
  f_temp <- numeric()
  for (i in 1:ncol(Z_i)) {
    f_temp[i] <- 1 / (n * h) * sum(k_gauss((Z_i[, i] - z) / h))
  }
  return(mean((f_temp - exp_f_hat_gauss(Z_i, z, h))^2))
}

# for each h integrate across all z
int_var_hat <- numeric() # Uniform
int_var_gauss <- numeric() # Gaussian

# takes long, we only plot up to h=7.5
for (i in 1:75) {
  int_var_hat[i] <- integrate(Vectorize(var_hat), min(grid), max(grid), h = h_grid[i], 
                              subdivisions = 2000, stop.on.error = FALSE)$value
  
  int_var_gauss[i] <- integrate(Vectorize(var_gauss), min(grid), max(grid), h = h_grid[i], 
                               subdivisions = 2000)$value
}


plot(h_grid[1:75], int_var_hat, type = "l", xlab = "h", ylab = "Estimated integrated variance", ylim = range(int_var_hat, int_var_gauss))
lines(h_grid[1:75], int_var_gauss, col = "blue")
legend("topright", c("Uniform", "Gaussian"), col = c("black", "blue"), lty = 1, lwd = 1)


# Integrated variance goes down when h goes up


## iii
###########

par(mfrow = c(1, 2))
plot(grid, sapply(grid, f_z), type = "l", col = "black", lty = 1, ylim = c(0, 0.15), lwd = 3, 
     ylab = "Density", main = "Uniform")
lines(grid, sapply(X = grid, FUN = f_hat, h = h_star), type = "l", col = "springgreen3", lwd = 2)
lines(grid, sapply(X = grid, FUN = f_hat, h = 0.5 * h_star), type = "l", col = "blue", lwd = 2)
lines(grid, sapply(X = grid, FUN = f_hat, h = 2 * h_star), type = "l", col = "red", lwd = 2)
legend("topright", c("h_star", "0.5*h_star", "2*h_star", "true density"), col = c("springgreen3", "blue", "red", "black"), lty = 1, lwd = 2)


plot(grid, sapply(grid, f_z), type = "l", col = "black", lty = 1, ylim = c(0, 0.15), lwd = 3, 
     ylab = "Density", main = "Gaussian")
lines(grid, sapply(X = grid, FUN = f_hat_gauss, h = h_gauss_star), type = "l", col = "springgreen3", lwd = 2)
lines(grid, sapply(X = grid, FUN = f_hat_gauss, h = 0.5 * h_gauss_star), type = "l", col = "blue", lwd = 2)
lines(grid, sapply(X = grid, FUN = f_hat_gauss, h = 2 * h_gauss_star), type = "l", col = "red", lwd = 2)
legend("topright", c("h_star", "0.5*h_star", "2*h_star", "true density"), col = c("springgreen3", "blue", "red", "black"), lty = 1, lwd = 2)


# e
###########
# for low h: variance high, bias low (undersmoothing)
# for high h: variance low, bias high (oversmoothing)
# Bias Variance Tradeoff


# 7
###########
graphics.off()

# In practical applications, the true density is not given and thus, we do not know its second derivative:
# use f_hat as approximation

# only depends on f_z through second derivative
# at point where f''(z) is positive, we have an overestimation, i.e. how fast is f'(z) changing and in which direction (curvature/Krümmung)

k_2_gauss <- function(z) {
  1 / (2 * pi) * exp(-0.5 * z^2) * (z^2 - 1)
}

kappa_temp <- function(z) {
  z^2 * k_gauss(z)
}

kappa_2 <- integrate(kappa_temp, -Inf, +Inf)$value

bias_f_n_gauss <- function(z, h) {
  1 / (2 * n * h) * sum(k_2_gauss((z_i - z) / h)) * kappa_2
}

sd_zi <- sd(z_i)
h_init <- 1.06 * sd_zi * n^(-1 / 5)

# At points of high curvature, bias is higher:
# f''(z)>0 --> Bias positive (at minimum here)
# f''(z)<0 --> Bias negative (at peaks here)


plot(grid, sapply(grid, bias_f_n_gauss, h = h_init), type = "l", ylab = "Bias")
abline(h = 0, col = "red", lty = 2)

# Compare this result using theoretical derivative, which can be obtained with normal kernel:
# f_norm''(x)=1/sigma^2 *f_norm(x)*((x-mu)^2 /sigma^2 -1)
# use true f_z instead for second derivative:
# f''_z(z)=0.5*(f_x''(z)+f_y''(z)), where x and y are the two mixtures of 1

# define general function for second derivative of gaussian
f_norm_2 <- function(z, mu, sigma_2) {
  dnorm(z, mu, sqrt(sigma_2)) / sigma_2 * (((z - mu)^2) / sigma_2 - 1)
}

# get mixture distribution in our case
k_2_true <- function(z) {
  0.5 * (f_norm_2(z, 165, 6) + f_norm_2(z, 180, 7))
}

# calculate bias
bias_f_n_gauss_new <- function(z, h) {
  0.5 * h^2 * k_2_true(z) * kappa_2
}


# compare both

plot(grid, sapply(grid, bias_f_n_gauss_new, h = h_init), type = "l", ylab = "Bias", main = "Theoretical Bias of Gaussian Kernel Density Estimator")
abline(h = 0, col = "red", lty = 2)
lines(grid, sapply(grid, bias_f_n_gauss, h = h_init), col = "blue")
legend("bottomright", c("Estimation with true density", "Estimation with Silverman's bandwidth"), col = c("black", "blue"), lty = 1, lwd = 1)

# using KDE with Silverman's bandwidth gives us way smoother version of bias (with smaller values)
# too optimistic in this case
