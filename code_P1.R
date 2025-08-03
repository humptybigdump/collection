# Load necessary packages
library(dplyr)
library(Matching)
data(lalonde)
detach("package:Matching", unload = TRUE)
dataset <- lalonde %>% dplyr::select('re78', 'treat', 'black')
# Attach dataset (this means that the variables within the dataset can be
# called by e.g. just writing 'treat' instead of 'dataset$treat')
attach(dataset)

# Get all sample sizes from the dataset (bl is for black, nbl is for not black)
n <- nrow(dataset)
n1 <- sum(treat); n0 <- n - n1
n_bl <- sum(black); n_nbl <- n - n_bl
n1_bl <- sum(treat * black); n0_bl <- n_bl - n1_bl
n1_nbl <- n1 - n1_bl; n0_nbl <- n_nbl - n1_nbl
pi_k <- c(n_nbl/n, n_bl/n)

# (a)
# Calculate the non-stratified difference-in-means
tau_hat <- mean(re78[treat == 1]) - mean(re78[treat == 0])
# Calculate the stratum-specific d-i-m and then the stratified d-i-m
tau_hat_k <- rep(NA, 2)
for(k in 1:2){
  tau_hat_k[k] <- mean(re78[treat == 1 & black == k-1]) -
    mean(re78[treat == 0 & black == k-1])
}
tau_hat_S <- sum(tau_hat_k * pi_k)
# Print both estimates
tau_hat
tau_hat_S


# (b)
# Get a contingency table of black for treat
bl_tr <- as.matrix(table(black, treat))
fisher.test(bl_tr)
# Within the sample, there is a very small positive effect of 'black' on 'treat'
# Within the entire population, this is not significant

# Do a linear regression of re78 on black and treat
summary(lm(re78 ~ black + treat))
# There is a negative large effect of black on re78, even if we control for treat

# Overall, the effect of treat on re78 via black is very small, thus
# almost no difference between tau_hat and tau_hat_S

# (c)
# Set random seed for MC
set.seed(0)
# Set number of MC iterations
n_MC <- 1e4
# Initialize vectors for tau_hat values for false treatment assignments
alt_tau_hat <- rep(NA, n_MC)
alt_tau_hat_S <- rep(NA, n_MC)
for(i in 1:n_MC){
  # Simulate false CRE treatment assignment
  Z_CRE <- sample(treat)
  # Calculate according d-i-m
  alt_tau_hat[i] <- mean(re78[Z_CRE == 1]) - mean(re78[Z_CRE == 0])
  # Initialize vector for statum-specific d-i-m's
  alt_tau_hat_k <- rep(NA, 2)
  # Simulate false SRE treatment assignment
  Z_SRE <- rep(NA, n)
  Z_SRE[black == 0] <- sample(treat[black == 0])
  Z_SRE[black == 1] <- sample(treat[black == 1])
  # Calculate both stratum-specific d-i-m's
  for(k in 1:2){
    alt_tau_hat_k[k] <- mean(re78[black == k-1 & Z_SRE == 1]) -
      mean(re78[black == k-1 & Z_SRE == 0])
  }
  # Calculate false stratified d-i-m
  alt_tau_hat_S[i] <- sum(alt_tau_hat_k * pi_k)
}
# Calculate and print both FRT p-values
p_CRE <- mean(alt_tau_hat >= tau_hat)
p_CRE
p_SRE <- mean(alt_tau_hat_S >= tau_hat_S)
p_SRE

# Visualize p-values by plotting true d-i-m's into histograms of false d-i-m's
limits <- quantile(c(alt_tau_hat, alt_tau_hat_S,
                     tau_hat, tau_hat_S), c(0.001, 0.999))
par(mfrow = c(2, 1))
hist(alt_tau_hat, xlim = limits)
abline(v = tau_hat, col = "red", lwd = 2)
hist(alt_tau_hat_S, xlim = limits)
abline(v = tau_hat_S, col = "red", lwd = 2)

# (d)
# Calculate estimates of variances over outcomes under treatment / control
# over entire sample
S1 <- (sum(treat) - 1)^(-1) *
  sum((re78[treat == 1] - mean(re78[treat == 1]))^2)
S0 <- (sum(1 - treat) - 1)^(-1) *
  sum((re78[treat == 0] - mean(re78[treat == 0]))^2)
# Combine them to get conservative variance estimate for tau_hat
V_hat <- S1/n1 + S0/n0
# Use it to calculate conservative CI for the true ACE
ci_CRE <- tau_hat + c(-1, 1) * sqrt(V_hat) * qnorm(0.975)

# Calculate stratum-specific variance estimators
V_hat_k <- rep(NA, 2)
for(k in 1:2){
  S1 <- (sum(treat * (black == k-1)) - 1)^(-1) *
    sum((re78[treat == 1 & black == k-1] -
           mean(re78[treat == 1 & black == k-1]))^2)
  S0 <- (sum((1 - treat) * (black == k-1)) - 1)^(-1) *
    sum((re78[treat == 0 & black == k-1] -
           mean(re78[treat == 0 & black == k-1]))^2)
  V_hat_k[k] <- S1/(c(n1_nbl, n1_bl)[k]) + S0/(c(n0_nbl, n0_bl)[k])
}
# Get conservative variance estimator for stratified d-i-m
V_hat_S <- sum(pi_k^2 * V_hat_k)
# Calculate corresponding conservative CI for true ACE
ci_SRE <- tau_hat_S + c(-1, 1) * sqrt(V_hat_S) * qnorm(0.975)

# Print both CI's
print(rbind(ci_CRE, ci_SRE))

# Detach dataset
detach(dataset)
