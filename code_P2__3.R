library(sandwich)
library(lmtest)
library(dplyr)
library(knitr)

experimental <- read.csv("C:/Users/Andreas Eberl/Documents/Lehrveranstaltungen/applied_econometrics_2023_24/Tutorials/6/experimental.csv")
observational <- read.csv("C:/Users/Andreas Eberl/Documents/Lehrveranstaltungen/applied_econometrics_2023_24/Tutorials/6/observational.csv")

# part a): Assess covariate balance

# initialize empty data frame for results
df_results <- data.frame()

# vector of regressor names
names_regressors <- setdiff(names(experimental), 
                            c("treatment", "RE78"))
# loop over regressors
for (nn in names_regressors){
  # loop over data sets
  for (data_set in 1:2){
    if (data_set == 1){
      dat <- experimental
      dat_name <- "experimental"
    } else {
      dat <- observational
      dat_name <- "observational"
    }
    # construct formula
    fml <- formula(paste(nn, "~ treatment"))
    # assess mean difference (regression on treatment dummy)
    fit_tmp <- lm(fml, data = dat)
    # get 95% confidence interval for mean difference
    # (lower and upper limit stored in second row of coefci output)
    ci_tmp <- coefci(fit_tmp, vcov. = vcovHC, type = "HC3")[2,] %>%
      unname
    # collect results
    df_results <- rbind(df_results, 
                        data.frame(data_set = dat_name, 
                                   regressor = nn, 
                                   lower = ci_tmp[1],
                                   upper = ci_tmp[2]))
    # in addition: make boxplot for numerical variables
    if (length(unique(dat[,nn])) > 2){
      boxplot(fml, data = dat, main = paste0(nn, ", ", dat_name, " data"))    
    }
  }
}

# print table with results
kable(df_results, digits = 2)

# part b): ATE Estimation
# For experimental data: Simple mean difference is enough
dat0 <- experimental %>% mutate(Y = RE78, D = treatment)
fit_experimental <- lm(Y~D, data = dat0)
coefci(fit_experimental, vcov. = vcovHC, type = "HC3") %>% kable(digits = 2)
# Print point estimate of ATE
coefficients(fit_experimental)["D"]

# For observational data: Need regression adjustment
# Can estimate ATE if selection on observables assumption holds
# Fit model with interaction terms (see Lecture 9)
dat1 <- observational %>% mutate(D = treatment, Y = RE78) %>% 
  select(Y, D, age, education, nodegree, married, Black, Hispanic, 
         RE74, RE75)
aux <- dat1[, !(names(dat1) %in% c("D", "Y"))]
intrct <- dat1$D * scale(aux, scale = FALSE)
colnames(intrct) <- paste0("D_", names(aux))
dat2 <- cbind(dat1, intrct)
fit_observational <- lm(Y~., data = dat2)
coefci(fit_observational, vcov. = vcovHC, type = "HC3") %>% kable(digits = 2)
# Print point estimate of ATE
coefficients(fit_observational)["D"]
