# Function returning the MLE for the geometric distribution
mle_geometric <- function(y){
  1/(mean(y) + 1)
}
# Function returning the MLE for the exponential distribution
mle_exponential <- function(y){
  1/mean(y)
}

# Set random seed 
set.seed(1)
# Nr of MC iterations
n_mc <- 1e3
# Sample size (in each iteration)
n <- 200
# initialize vectors of results
res.geom <- rep(NA, n_mc)
res.exp <- rep(NA, n_mc)

# set true parameter (for geom. dist.)
theta <- .4
# Loop over MC iterations
for (jj in 1:n_mc){
  y <- rgeom(n, theta)
  res.geom[jj] <- mle_geometric(y)
}

# Results for for the geom. dist.
hist(res.geom, main = "Histogram of MLE results", xlab = "Estimate")
abline(v = theta, col = "green4", lwd = 2)

# set true parameter (for exp. dist.)
lambda <- 2
# Loop over MC iterations
for (jj in 1:n_mc){
  y <- rexp(n, lambda)
  res.exp[jj] <- mle_exponential(y)
}

# Results for for the exp. dist.
hist(res.exp, main = "Histogram of MLE results", xlab = "Estimate")
abline(v = lambda, col = "green4", lwd = 2)