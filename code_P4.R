# (a)
# Write function that returns all divisors of a given number x except x itself
divisors <- function(x){
  # Vector of numbers to test against
  y <- seq_len(x)
  # If remainder is 0 that number is a divisor of x so return it
  y[x%%y == 0 & x != y]
}

# Apply function for 200
divis <- divisors(200)
divis

# (b) + (c)
set.seed(0)
# Set sample sizes
n <- 400
n1 <- 0.5*n; n0 <- n - n1
# Set covariate values and simulate potential outcomes according to problem text
X <- rep(seq(-2, 2, length.out = max(divis)), each = n/max(divis))
Y0 <- 10*X - 5 + rnorm(n, 50, 10)
Y1 <- Y0 + 10 + rnorm(n, 0, 5)

# Inizialize vector to hold the variance of the difference-in-means fo each K
vars <- rep(NA, length(divis))
# Loop over the divisors
for(d in 1:length(divis)){
  # Set K to be the current divisor
  K <- divis[d]
  # Set the sample sizes in each stratum accordingly
  nk <- n/K; nk1 <- n1/K; nk0 <- n0/K
  # Set number of MC iterations
  n_MC <- 10^4
  # Initialize difference-in-means vector
  tau_hat <- rep(NA, n_MC)
  for(i in 1:n_MC){
    # Initialize treatment assignment vector
    Z_SRE <- rep(NA, n)
    # For all units in one stratum, randomly assign half to the treatment
    # group and the other half to the control group
    for(j in 1:K){
      Z_SRE[(1 + (j-1)*nk):(j*nk)] <- sample(rep(c(0, 1), c(nk0, nk1)))
    }
    # Since the propensity score is the same in all strata, the stratified
    # difference-in-means is equal to the ordinary difference-in-means
    # and can therefore be calculated like this
    tau_hat[i] <- mean(Y1[Z_SRE == 1]) - mean(Y0[Z_SRE == 0])
  }
  # Calculate the empirical variance of the difference-in-means for the current
  # number of strata
  vars[d] <- var(tau_hat)
}

# Set up just one plot panel
par(mfrow = c(1, 1))
# Plot the variance values equidistantly in x-direction without axes
plot(1:length(vars), vars, type = 'b', lwd = 2, pch = 16, axes = FALSE,
     xlab = "K", ylab = "Empirical variance of stratified difference-in-means")
# Add y-axis as usual
axis(2)
# Add x-axis with numbers of strata
axis(1, at = 1:length(vars), labels = divis)
# Add box around plot
box()
