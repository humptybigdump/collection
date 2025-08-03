# Load relevant packages and dataset
library(dplyr)
library(Matching)
data(lalonde)
# Restrict to relevant variables
dataset <- lalonde %>% dplyr::select(treat, re78)

# (a)
# Get the observed outcomes under treatment and calculate their mean
Y1 <- dataset$re78[dataset$treat == 1]
Y1_bar <- mean(Y1)
# Get the observed outcomes under control and calculate their mean
Y0 <- dataset$re78[dataset$treat == 0]
Y0_bar <- mean(Y0)
# Calculate and print the difference-in-means
tau_hat <- Y1_bar - Y0_bar
tau_hat

# (b)
# Get the sample size and the number of treated/untreated units
n <- nrow(dataset)
n1 <- sum(dataset$treat)
n0 <- n - n1
# Calculate the two variance terms S^2(1) and S^2(0)
S1 <- (n1 - 1)^(-1) * sum((Y1 - Y1_bar)^2)
S0 <- (n0 - 1)^(-1) * sum((Y0 - Y0_bar)^2)
# Calculate the conservative variance estimator for the difference-in-means
V_hat <- S1/n1 + S0/n0
# Calculate and print both bounds of the conservative 95%-confidence
# interval for the ACE
CI <- tau_hat + c(-1, 1) * qnorm(0.975) * sqrt(V_hat)
CI

# (c)
# Print the test decision based on whether zero is contained in the CI
if(0 >= CI[1] & 0 <= CI[2]){
  print("The hypothesis ACE=0 is not rejected.")
}else{
  print("The hypothesis ACE=0 is rejected.")
}
