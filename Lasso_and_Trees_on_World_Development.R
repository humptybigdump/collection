library(ISLR)
library(dplyr)
library(rpart)
library(rpart.plot)
library(glmnet)

# Import dataset - you can also download the most recent dataset (or just your preferred indicators) from https://databank.worldbank.org/source/world-development-indicators 
# If you are interested in how the HDI is actually calculated check out: https://hdr.undp.org/sites/default/files/2023-24_HDR/hdr2023-24_technical_notes.pdf 
world_development <- read.csv("~/world_development.csv")

##### LASSO #####
# Divide data into test and training dataset, create model matrix for each of them
test <- model.matrix(~.,data = world_development[81:109,3:236])
train <- model.matrix(~.,data = world_development[1:80,3:236])
# Use Lasso-Method from glmnet (alpha = 1 leads to Lasso, alpha = 0 leads to Ridge-Regression), automatically uses 100 different values for lambda
lasso <- glmnet(train , world_development[1:80,2], alpha = 1)
# Plot coefficients depending on the size of lambda (the number on top is the number of nonzero coefficients)
plot(lasso, xvar = "lambda")

# For a given (index of a) lambda, this returns the indicators with nonzero coefficients (replace 2 with the index of the lambda of interest)
which(lasso$beta[,2]!=0)

# Make predictions (for every lambda) for the test data
pred_lasso <- predict(lasso, test) 
# Calculate MSE in test dataset (for every lambda)
mse_test <- c()
for (i in 1:100) {
  mse_test <- c(mse_test, mean((pred_lasso[,i] -world_development[81:109,2])**2) )
}
# Plot MSE (for every lambda)
plot(lasso$lambda, mse_test)
# Return lambda with minimal MSE
lasso$lambda[which.min(mse_test)]
mse_lasso <- min(mse_test)


# How to choose lambda based on training-set (we chose it based on the performance on the test-dataset -> we had fewer data for training)? -> Use Cross Validation!
cvfit_10 <- cv.glmnet(train ,world_development[1:80,2], alpha = 1, nfold = 10)
cvfit_10$lambda.min
plot(cvfit_10)
cvfit_3 <- cv.glmnet(train ,world_development[1:80,2], alpha = 1, nfold = 3)
cvfit_3$lambda.min
plot(cvfit_3)
cvfit_80 <- cv.glmnet(train ,world_development[1:80,2], alpha = 1, nfold = 80)
cvfit_80$lambda.min
plot(cvfit_80)

##### REGRESSION TREES #####
# Regression Tree - Blackbox Method
tree <-  rpart(world_development[1:80,2]~., data=world_development[1:80,3:236], method = "anova")
plot(tree)
text(tree)

pred_blackbox_tree <- predict(tree, newdata=world_development[81:109,3:236])
mse_blackbox_tree <- mean((pred_blackbox_tree  - world_development[81:109,2])**2)

# Look at subset of the regressors (otherwise implementing a histogram-estimator for 236 indicators without any package is pain) -> the first three chosen with the blackbox-method
world_development_small <- data.frame(cbind(world_development[1:80,2], world_development$GDP..PPP..constant.2017.international...[1:80], world_development$Agriculture..forestry..and.fishing..value.added....of.GDP.[1:80], world_development$Mortality.rate..under.5..per.1.000.live.births.[1:80]))
world_development_small_test <- data.frame(cbind(world_development[81:109,2], world_development$GDP..PPP..constant.2017.international...[81:109], world_development$Agriculture..forestry..and.fishing..value.added....of.GDP.[81:109], world_development$Mortality.rate..under.5..per.1.000.live.births.[81:109]))

# Histogram - this might not work so well here (especially the first indicator)... Could also be a realization of a purely random tree :D
which(world_development_small[2] > (max(world_development_small[2])+min(world_development_small[2]))/2)
which(world_development_small[3] > (max(world_development_small[3])+min(world_development_small[3]))/2)
which(world_development_small[4] > (max(world_development_small[4])+min(world_development_small[4]))/2)

# Implement function (there are way cleaner ways to implement this)
pred_histrogram <- function(x){
  index <- 1:80
  if(x[1] > (max(world_development_small[2])+min(world_development_small[2]))/2){
    index_loc <- c(which(world_development_small[2] > (max(world_development_small[2])+min(world_development_small[2]))/2))
    index <- intersect(index, index_loc)
  }else{
    index_loc <- c(which(world_development_small[2] <= (max(world_development_small[2])+min(world_development_small[2]))/2))
    index <- intersect(index, index_loc)
  }
  if(x[2] > (max(world_development_small[3])+min(world_development_small[3]))/2){
    index_loc <- c(which(world_development_small[3] > (max(world_development_small[3])+min(world_development_small[3]))/2))
    index <- intersect(index, index_loc)
  }else{
    index_loc <- c(which(world_development_small[3] <= (max(world_development_small[3])+min(world_development_small[3]))/2))
    index <- intersect(index, index_loc)  
  }
  if(x[3] > (max(world_development_small[4])+min(world_development_small[4]))/2){
    inde_loc <- c(which(world_development_small[4] > (max(world_development_small[4])+min(world_development_small[4]))/2))
    index <- intersect(index, index_loc)
  }else{
    index_loc <- c(which(world_development_small[4] <= (max(world_development_small[4])+min(world_development_small[4]))/2))
    index <- intersect(index, index_loc)
  }
  return(mean(world_development_small[index, 1]))
}

# Make predictions (much less sophisticated method - unsurprisingly the results are worse than using the methods above)
pred <- c()
for (i in 1:dim(world_development_small_test)[1]) {
  pred <- c(pred, pred_histrogram(world_development_small_test [i,2:4]))
}
mse_histogram <- mean((pred - world_development_small_test [,1])**2)

