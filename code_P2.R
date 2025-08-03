# Needed below
library(ggplot2)

# Settings
n <- 5000 # sample size
G <- .4 # coefficient Gamma (reg. of x2 on x1)
b1 <- 1 # coefficients beta1, beta2
b2 <- 1
theta <- 1 # heteroscedasticity coefficient
set.seed(202012) # random seed

# Simulate from simple model, put into data frame
x1 <- rnorm(n)
x2 <- rnorm(n, mean = G*x1, sd = sqrt((theta*x1)^2 + 1)) 
df <- data.frame(x1, x2)

# Plot x1 and x2
ggplot(df, aes(x = x1, y = x2)) + geom_point(col = grey(.3, .3)) + 
  theme_minimal(base_size = 15) + xlab(expression(X[1])) + 
  ylab(expression(X[2]))

# Look at projection on x1
y <- b1*x1 + b2*x2 + rnorm(n)
proj1 <- (b1 + G*b2)*x1

# Projection error
u <- y - proj1

# Add squared value to data frame
df$u2 <- u^2

# Plot u^2 against X1, add CEF estimate
ggplot(df, aes(x = x1, y = u2)) + geom_point(col = grey(.3, .3)) + 
  theme_minimal(base_size = 15) + xlab(expression(X[1])) + 
  ylab(expression(u^2)) + geom_smooth()