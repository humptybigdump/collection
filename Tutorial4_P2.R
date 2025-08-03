# log likelihood function for model
ff <- function(theta, y){
  p <- pnorm(theta[1]) # pnorm to ensure in [0,1]
  lambda <- exp(theta[2]) # exp to ensure > 0
  sel0 <- (y == 0) # dummy for whether y equals 0
  ll <- rep(NA, length(y)) # initialize vector of log-likelihood values
  # entries for zero observations
  ll[sel0] <- log(p + (1-p)*exp(-lambda))
  # entries for nonzero observations
  ll[!sel0] <- log(1-p) + y[!sel0]*log(lambda) - lambda - log(factorial(y[!sel0]))
  # return minus sum of log likelihoods
  -sum(ll)
}

# check ML for simulated data
# sample size 
n <- 5000
# true parameters
p <- .3
lambda <- 2
# simulate data
aux <- runif(n) <= p 
y <- rep(0, n)
y[!aux] <- rpois(sum(!aux), lambda)
# compute MLE
opt <- optim(c(0, 0), ff, y = y)
# print parameter estimates
round(c(pnorm(opt$par[1]), exp(opt$par[2])), 3)

# now apply to empirical data
library(COUNT)
data(badhealth)
y_emp <- badhealth$numvisit
opt_emp <- optim(c(0, 0), ff, y = y_emp)
# print parameter estimates
p_emp <- pnorm(opt_emp$par[1])
lambda_emp <- exp(opt_emp$par[2])
round(c(p_emp, lambda_emp), 3)
# compare to estimate for standard Poisson
round(mean(y_emp), 3)

# Load necessary packages
library(ggplot2)
library(dplyr)

#Compare the fits
# Define grid for horizontal axis
y <- 0:15
# Get Poisson and empirical probabilities
poisson_prob <- dpois(y, lambda = mean(y_emp))
zip_prob <- (1-p_emp)*dpois(y, lambda = lambda_emp) +
  c(p_emp, rep(0, 15))
emp_prob <- table(y_emp)[1:16]/nrow(badhealth)

# Make data frame that contains Poisson probabilities for 
# two different choices of lambda
df <- data.frame(y = y, p = poisson_prob, 
                 name = "Poisson") %>%
  rbind(data.frame(y = y, 
                   p = zip_prob, 
                   name = "ZIP")) %>%
  rbind(data.frame(y = y, 
                   p = as.numeric(emp_prob), 
                   name = "Empirical"))

# Make plot
ggplot(df, aes(x = y, y = p, fill = name)) + 
  # bar plot
  geom_bar(stat = "identity", position = "dodge") + 
  # edit plot layout
  theme_minimal() + 
  theme(legend.position = "top") + 
  scale_fill_discrete(name = "Model") + 
  xlab("y") + ylab("Probability") + 
  scale_x_continuous(breaks = y)