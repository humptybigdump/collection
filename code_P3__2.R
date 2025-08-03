# First, we need the functions to compute the standard errors from
# the lecture

# Function to compute robust standard errors 
# fit is a fitted model (via lm)
compute_HCs <- function(fit){
  # Get data and dimensions
  X <- model.matrix(fit)
  k <- ncol(X)
  n <- nrow(X)
  # Inverse
  XXi <- solve(t(X) %*% X)
  e <- residuals(fit)
  # Matrix of ones (needed below)
  omat <- matrix(1, 1, k)
  u1 <- X * (e %*% omat)
  HC0_ <- XXi %*% (t(u1) %*% u1) %*% XXi
  HC1_ <- n/(n-k) * HC0_
  # h_{ii} values needed for HC2 and HC3
  lev <- hatvalues(fit) 
  u2 <- X * (e/sqrt(1-lev) %*% omat)
  HC2_ <- XXi %*% (t(u2) %*% u2) %*% XXi
  u3 <- X * (e/(1-lev) %*% omat)
  HC3_ <- XXi %*% (t(u3) %*% u3) %*% XXi
  # Return list with four estimators
  list(HC0_, HC1_, HC2_, HC3_)
}

# Function to compute clustered standard errors
# fit is a fitted model (via lm)
# cluster_var is a vector containing the categorical variable used for clustering
compute_cluster_VCV <- function(fit, cluster_var){
  # Get data and dimensions
  X <- model.matrix(fit)
  k <- ncol(X)
  n <- nrow(X)
  e <- residuals(fit)
  y <- predict(fit) + e
  XXi <- solve(t(X) %*% X)
  # X times e (n*k matrix)
  Xe <- X * (e %*% matrix(1, 1, k))
  # Sum across clusters
  Xe_sum <- rowsum(Xe, cluster_var)
  # Number of clusters
  G <- nrow(Xe_sum)
  # double-check
  if (G != length(unique(cluster_var))) 
    stop("cluster_var seems wrong")
  omega <- t(Xe_sum) %*% Xe_sum
  # Scaling factor
  scl <- (G/(G-1))*((n-1)/(n-k))
  V_clustered <- scl* XXi %*% omega %*% XXi
  se_clustered <- sqrt(diag(V_clustered))
  # Return list of coefficients, standard errors and VCV matrix
  list(beta = coefficients(fit), se = se_clustered, V = V_clustered)
}

# Load dependencies
library(dplyr)
library(readxl)
# Load full data set
orig.data <- read_excel("C:/Users/Andreas Eberl/Documents/Lehrveranstaltungen/applied_econometrics_2023_24/Tutorials/5/Tutorial5_Solutions/DDK2011.xlsx")
# Modify data set as specified in the problem text
new.data <- orig.data %>%
  mutate(std.testscore = (totalscore-mean(totalscore))/sd(totalscore)) %>%
  select(std.testscore, tracking, agetest, girl, etpteacher, percentile, schoolid) %>%
  na.omit
# Fit small linear model
simple.fit <- lm(std.testscore ~ tracking, data = new.data)
# Fit full linear model
full.fit <- lm(std.testscore ~ . - schoolid, data = new.data)

#a)
# Robust standard errors
sqrt(diag(compute_HCs(full.fit)[[4]]))

# Clustered standard errors
compute_cluster_VCV(full.fit, new.data$schoolid)$se

# All se's are larger for the clustered sample
# Likely reason: Effective sample size is number of schools instead of number
# of students and is therefore smaller
# Largest difference for regressor 'tracking': here all of the variation
# is between the schools and none within the schools


#b)
# Coefficients with tracking as sole variable in the model
simple.fit$coefficients
# (Why different from coefficients in the book?)
# May tracking-variable only seem relevant because effects of other, ommited
# regressors are falsely assigned to 'tracking'?

# Coefficients with all variables in the model
full.fit$coefficients
# tracking-coefficient is even larger than for the simple model, even with other
# regressors in the model. Answer to question above seems to be 'no'.
