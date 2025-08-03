rm(list = ls())
library(dplyr)
library(nycflights13)
library(ggplot2)
library(reshape2)

########################
# Functions
########################

# Function for k nearest neighbor regression
KNN_prediction <- function(x, y, x0, k){
  
  # Nr of predictions
  n0 <- length(x0)
  
  # Data frame to store predictions
  pred <- data.frame(x0 = x0, y0 = NA)
  
  # Loop through x0 values
  for (jj in 1:n0){
    # Squared deviations from x0[jj]
    aux <- (x - x0[jj])^2
    
    # Find nearest neighbors
    # First rank squared deviations
    rk <- rank(aux, ties.method = "random")
    
    # Find indexes of nearest neighbors
    ind_nn <- which(rk <= k)
    
    # Compute mean y value for neighbors
    pred$y0[jj] <- mean(y[ind_nn])
    
  }
  
  return(pred)
  
}

# Function for k nearest neighbor regression (alternative version)
# Avoids using a for loop, however computes the outer product of x and x0 which can be
# quite memory intensive 
KNN_prediction2 <- function(x, y, x0, k){
  # Squared differences between elements of x and x0
  # In the columns, list distance of each point in x0 to each point in x
  d <- outer(x, x0, function(a, b) (a-b)^2)
  # For each x0 value: Rank x values (closest first) (column-wise applied)
  r <- apply(d, 2, function(x) rank(x, ties.method = "random")) 
  # For each x0 value: Mean over k nearest neighbors (column-wise applied)
  y0 <- apply(r, 2, function(z) mean(y[z <= k]))  
  # Return data frame with x0 and y0
  return(data.frame(x0 = x0, y0 = y0))
}

##############################
# Predictions for flight data
##############################

# (3a) Load flights data 
data("flights") 
# Filter observations where both dep_delay and arr_delay are available
# Pipe operator example: https://statistikguru.de/r/pipes-in-r.html
flights3 <- flights %>% select(dep_delay, arr_delay) %>% na.omit
#another option
flights3_b <- na.omit(flights[,c("dep_delay", "arr_delay")])
#yet another option, also keeps other columns
flights3_c <- flights %>% filter(!is.na(dep_delay) & !is.na(arr_delay))


# (3b) Design points x0
x0 <- seq(from = -10, to = 50, length.out = 100)
# Initialize empty data frame to store predictions
pred_all <- data.frame()
# Choices of k
k_choices <- c(1, 20, 200)
# Loop over these choices
for (kk in k_choices){
  # Fit data for present choice of k
  fit_tmp <- KNN_prediction(x = flights3$dep_delay, 
                            y = flights3$arr_delay, 
                            x0 = x0, k = kk)
  # Add results to data frame
  pred_all <- rbind(pred_all, 
                    data.frame(fit_tmp, k = factor(kk)))
}

# Check whether dimension of pred_all makes sense
check <- nrow(pred_all) == length(x0)*length(k_choices)
if (!check) stop("Dimension of pred_all makes no sense")


##############################
# Plotting
##############################

# (3c) Plot results via basic R
pred_all2 <- dcast(x0~k, value.var = "y0", data = pred_all)
matplot(pred_all2[,1], pred_all2[,-1], type = "l", bty = "n", 
        xlab = "x0", ylab = "y0", lwd = 1.5, col = 1:3,
        lty = 1)
legend("topleft", c("k = 1", "k = 20", "k = 100"), lwd = 1.5,
       col = 1:3, bty = "n")

# Plot results via ggplot
# ggplot documentation: https://ggplot2.tidyverse.org/reference/
ggplot(pred_all, aes(x = x0, y = y0, col = k)) + geom_line() + 
  scale_color_viridis_d() + theme_minimal()
 