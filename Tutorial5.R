rm(list = ls())

library(dplyr)
library(knitr)

# Function for logistic transformation, i.e. sigmoid
logit <- function(x, b){
  1/(1+exp(-x %*% b))
}

# function to fit logistic regression via iteratively reweighted least squares
fit_logit_irls <- function(y, x, b_start = NULL, n_iter = 20){
  k <- ncol(x)
  if (is.null(b_start)){
    b_start <- rep(0, k)
  }
  b_all <- matrix(0, k, n_iter)
  b_all[,1] <- b_start
  for (jj in 2:n_iter){
    # vector of probs
    p <- logit(x, b_all[,jj-1])
    # vector of weights
    w <- p*(1-p) 
    # response z for auxiliary regression

    z <- x %*% b_all[,jj-1] + (y - p)/w
    # weighted regression
    b_all[,jj] <- lm(z~x-1, weights = w) %>% coefficients #-1 to get rid of intercept
  }
  b_all
}

# Simulate data
n <- 1e5
x <- cbind(1, rnorm(n))
y <- runif(n) <= logit(x, c(1, 2)) #logit(x, c(1, 1)) gets "true" logits for given values x and betas = (1,1)
b_irls <- fit_logit_irls(y, x, n_iter=1000)
matplot(t(b_irls), type = "l")

# Titanic data
library(titanic)
data(titanic_train)
titanic_train <- titanic_train %>% mutate(Pclass = factor(Pclass))

titanic_train$Sexmale = 1 - titanic_train$Sexmale

y <- titanic_train$Survived
x <- model.matrix(Survived~Sex, data = titanic_train)

# Compare irls, glm and brute force
b_all <- fit_logit_irls(y, x, n_iter=100)
fit <- glm(y~x[,-1], family = "binomial")
#system.time(bf <- fit_logit_brute_force(y, x))
df <- data.frame(irls = b_all[,ncol(b_all)], glm = coefficients(fit))
                 #brute_force = bf)
kable(df, digits = 3)

#check for female=1
x <- data.frame(x)
x$Sexmale = 1- x$Sexmale
fit <- glm(y~x[,-1], family = "binomial")
coefficients(fit)

