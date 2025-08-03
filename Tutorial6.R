library(dplyr)
library(murphydiagram)
library(sandwich)
library(lmtest)
# Function to test joint H0 that MZ intercept is zero and MZ slope is one
# lm_mz is a fitted lm object containing the regression
# vhat_mz is the estimated VCV matrix of the parameters
wald_test <- function(lm_mz, vhat_mz){
  # Parameter values under H0
  r <- c(0, 1)
  # Estimated coefficient vector
  theta <- lm_mz %>% coefficients %>% unname
  # Remove names from VCV
  vhat <- vhat_mz %>% unname
  # Wald test stat
  wald <- t(theta - r) %*% solve(vhat) %*% (theta - r)
  # P-value
  (1 - pchisq(wald, df = 2))
}

data("recession_probability")
mz_probit <- lm(recession~probit, 
                data = recession_probability)
# Coefficient CIs
coefci(mz_probit, vcov. = NeweyWest)
# Wald p-value
wald_test(mz_probit, NeweyWest(mz_probit))

mz_spf <- lm(recession~spf, 
             data = recession_probability)
# Coefficient CIs
coefci(mz_spf, vcov. = NeweyWest)
# Wald p-value
wald_test(mz_spf, NeweyWest(mz_spf))
