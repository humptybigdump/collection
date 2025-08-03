# set random seed
set.seed(343)
# sample size
n <- 200
# nr of Monte Carlo iterations
n_mc <- 1000

# true parameter
lambda <- 2
# parameter estimates to be stored here
lambda_est <- rep(NA, n_mc)
# variance estimates to be stored here
var_est <- data.frame(V2 = rep(NA, n_mc), V3 = rep(NA, n_mc), V4 = rep(NA, n_mc))

# loop over MC iterations
for (jj in 1:n_mc){
  y <- rexp(n, rate = lambda)  
  lambda_hat <- 1/mean(y)
  lambda_est[jj] <- lambda_hat
  # variance estimate based on inverse of negative Hessian
  var_est[jj, 1] <- (lambda_hat^2)/n
  # ... based on inverse of outer product of score
  var_est[jj, 2] <- 1/sum((1/lambda_hat-y)^2)
  # ... sandwich
  var_est[jj, 3] <- var_est[jj, 1] * (1/var_est[jj, 2]) * var_est[jj, 1]
}

# Summarize variance estimates
boxplot(var_est)
# Add actual ('true') variance across MC iterations
abline(h = var(lambda_est), col = "green4", lwd = 2)
# Add theoretical variance
abline(h = n^2*lambda^2/((n-1)^2*(n-2)), col = "red", lwd = 2)