## ----message=FALSE------------------------------------------------------------------------------------
# install.packages("AmesHousing")
library(AmesHousing)
library(dplyr)
dat <- make_ames() # makes `clean' version of data
dim(dat) # dimension of data


## ----message=FALSE, echo = FALSE----------------------------------------------------------------------
names(dat) # variable names


## ----echo = FALSE, message = FALSE--------------------------------------------------------------------
library(ggplot2)
dat %>% ggplot(aes(x = Sale_Price)) + geom_histogram() + 
  theme_minimal(base_size = 14) + scale_color_viridis_d() + 
  geom_vline(xintercept = median(dat$Sale_Price)) + xlab("Sale Price")



## -----------------------------------------------------------------------------------------------------
# Drop variables that are typically not available for prediction
dat2 <- dat %>% select(-Sale_Type, -Sale_Condition) 
# Make predictor matrix (delete intercept in first column)
x <- model.matrix(Sale_Price~., data = dat2)[,-1]
y <- dat2$Sale_Price
# Dimension of x
dim(x)


## -----------------------------------------------------------------------------------------------------
# table of condition variable (categorical)
table(dat2$Overall_Cond)
# vector that shows whether each predictor is constant
ind <- which(apply(x, 2, sd) == 0)
# names of constant predictors
colnames(x)[ind]
# drop constant predictors
x <- x[,-ind]


## -----------------------------------------------------------------------------------------------------
# Correlation of each predictor with y
cors <- cor(x, y) 
# Put into data frame
df_cors <- data.frame(name = row.names(cors), correlation = cors) %>% 
  arrange(abs(correlation))
# Show five largest correlations (in abs. terms)
tail(df_cors, 5)


## ----message = FALSE----------------------------------------------------------------------------------
library(glmnet)
# Standardize x matrix
xs <- scale(x)
# Run cross-validation
# alpha = 0 is for Ridge, alpha = 1 for LASSO
cv <- cv.glmnet(xs, y, alpha = 1)


## -----------------------------------------------------------------------------------------------------
plot(cv)


## -----------------------------------------------------------------------------------------------------
b <- coef(cv, s = "lambda.1se") #lambda.1se most regularized model such that it's error is within one standard error of minimum
b_df <- data.frame(name = row.names(b), value = as.numeric(b)) %>%
  arrange(value)
tail(b_df, 5)

#
ols <- lm(y ~ xs)
summary(ols)
coef(ols)

