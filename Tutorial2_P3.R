# Function to plot the log-likelohoof functions
ll_plot <- function(theta = .4, n = 20){
  # Simulate 10 datasets of sample size n
  # from a geom. dist. with paremeter theta
  dat <- matrix(rgeom(n*10, theta), n, 10)
  # Set a grid of evaluation points for the function
  theta_grid <- seq(from = .001, to = .999, by = .001)
  # Initialize a matrix for the function values
  # and a vector for the MLE's
  ll_mat <- matrix(NA, length(theta_grid), 10)
  mle <- rep(NA, 10)
  # Loop through the 10 datasets
  for (jj in 1:10){
    # Loop through the evaluation points
    for (kk in seq_along(theta_grid)){
      # Calculate the function value
      ll_mat[kk,jj] <- sum(dgeom(dat[,jj],
                                  theta_grid[kk], log = TRUE))
    }
    # Calculate the MLE
    mle[jj] <- theta_grid[which.max(ll_mat[,jj])]
  }
  # Plot all the log-likelihood functions for all 10 datasets simultaneously
  matplot(theta_grid, 
          ll_mat, type = "l", bty = "n", 
          col = grey(.3, .3), lty = 1,
          xlab = expression(theta), ylab = "Log Likelihood", 
          cex.axis = 1.4, cex.lab = 1.4)
  # Draw vertical lines for the MLE's
  for (jj in 1:10){
    abline(v = mle[jj])
  }
  # Draw a green vertical line for the "true" parameter value
  abline(v = theta, col = "green")
}

set.seed(2)
ll_plot()
ll_plot(n = 1000)
