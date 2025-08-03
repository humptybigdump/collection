library(dplyr)
# (c) and (d)
# 1.
# Draw a random treatment assignment mechanism
z_true <- sample(rep(c(0, 1), each = 3), 6)
z_true

# 2.
# Get an observed outcome vector by changing two random entries in z_true
Y <- z_true
Y
false_indices <- sample(6, 2)
Y[false_indices] <- abs(Y[false_indices] - 1)
Y

# 3.
# Define a function that returns all possible allocations of n1 treatments
# onto n units in a matrix (one permutation per row)
permutation_matrix = function(n, n1){
  # Define M as the binomial coefficient 'n choose n1'
  M <- choose(n, n1)
  # Get the matrix of all possible 3-combinations from 5 different individuals,
  # these are the treated indices
  treat_index <- t(combn(n, n1))
  # Initialize the return matrix
  Z <- matrix(data = rep(0, n*M), nrow = M)
  # Loop over all possible allocations
  for(m in 1:M){
    # Get the treated indices for the current allocation
    treat <- treat_index[m, ]
    # Set the corresponding matrix entries to 1,
    # the rest was already initialized to 0
    Z[m, treat] <- 1
  }
  # Return the final matrix
  return(Z)
}
# Call the function for n=6 and n1=3
allocations <- permutation_matrix(6, 3)
allocations

# Add column names for the 5 units
colnames(allocations) <- paste0('cup', 1:6)
# Turn the matrix into a data frame and add a columns specifying which
# allocation is the true one
allocations <- as.data.frame(allocations) %>%
  mutate(true_alloc = as.logical(rowSums(allocations != 
                        matrix(rep(z_true, nrow(allocations)),
                               byrow = TRUE, nrow = nrow(allocations))) == 0))
allocations

# Define a function that calculates the difference-in-means for the
# treatment assignment in the i-th row of 'allocations'
d_i_m_func <- function(i){
  # Calculate the mean for the units in the treatment and the control group,
  # respectively
  treat_mean <- mean(Y[as.logical(allocations[i, 1:6])])
  control_mean <- mean(Y[as.logical(1-allocations[i, 1:6])])
  # Return their difference
  return(treat_mean - control_mean)
}
# Add the difference-in-means' for the corresponding treatment assignments
# as an additional column to the allocations-data-frame and print it
allocations <- allocations %>%
  mutate(diff_in_means = sapply(1:nrow(allocations), d_i_m_func))
allocations

# 4.
# Calculate the p-value
p_FRT <- mean(allocations$diff_in_means >=
                allocations$diff_in_means[allocations$true_alloc])
p_FRT

plot(x = allocations$diff_in_means, y = jitter(rep(0, nrow(allocations))),
     pch = 16)
abline(v = allocations$diff_in_means[allocations$true_alloc],
       col = "red", lwd = 2)
