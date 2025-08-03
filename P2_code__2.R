# Set the sample size and simulate all variables
# according to the assumptions
n <- 1e6
Y0 <- rnorm(n)
tau <- -0.5 + rnorm(n)
Y1 <- Y0 + tau
Z <- (tau > 0)
Y <- Z * Y1 + (1-Z) * Y0
# Calculate the empirical difference in means
mean(Y[Z==1]) - mean(Y[Z==0])
# Calculate the theoretical difference in means
dnorm(0.5)/(1 - pnorm(0.5)) - 0.5
