library(glmnet) # for logistic regression
library(readr) # to read data
library(matlib)
library(e1071) # for SVM

# Problem 1: Cleveland Heart Disease
# The logistic regression and the SVM include optimization problems - how to solve them is an interesting question, but this is not the focus of the lecture. 
# In both cases, we are going to use packages. In case of the LDA and kNN, we implement the entire function by hand, packages that do the calculations for you
# are noted in the comments. As plotting is usually time consuming, the plots are already implemented. 

# Load data (diagnosis heart disease is in column 14, "1" means that the person of the observation sufferes from a heart disease)
heart_disease_dataset <- read_csv("2_SS23/Statistical Learning/heart_disease_dataset.csv")

# Divide dataset into test and training data (75% of the data should be used as training data).
# You can either simply use the first 75% of observations for training data or you choose randomly (e.g using createDataPartition). 

train <-   # Contains the indices of the training data
test <-    # Contains the indices of the test data


###### Logistic regression ###### 

# Use the function glm from the glmnet package to choose two continuous explanatory variables ( glm(formula = Y ~ X_1 + ... + X_n, family = binomial(link = "logit")) )
full_log_reg <- 

# Use glm to estimate the vector beta based on the two continuous explanatory variables you chose. 

betaHat<- 


###### Plot your results ###### 
# Example if you choose Resting_Blood_Pressure and Max_Heart_Rate_Achieved 
ill <-which(heart_disease_dataset$Diagnosis_Heart_Disease>0)
healthy <-which(heart_disease_dataset$Diagnosis_Heart_Disease==0)
plot(heart_disease_dataset$Resting_Blood_Pressure[ill],heart_disease_dataset$Max_Heart_Rate_Achieved[ill],col="darkorchid4",ylim=c(min(as.numeric(heart_disease_dataset$Max_Heart_Rate_Achieved)),max(as.numeric(heart_disease_dataset$Max_Heart_Rate_Achieved))),pch=18,xlab="Resting blood pressure",ylab="Maximum heart rate" )
points(heart_disease_dataset$Resting_Blood_Pressure[healthy],heart_disease_dataset$Max_Heart_Rate_Achieved[healthy],col="chartreuse4", pch=17, cex=0.9 )
# plot line of classification based on your logistic regression using abline() 


###### Evaluate your results with the test data ###### 
# Calculate number of falsely classified observations in the test dataset
XTest<-              # Modelmatrix 
pred_log_reg <-      # Predicitions for the test dataset using the logistic regression you did with the training dataset
error_log_reg<-      # Number of falsely classified data


###### LDA ###### 
# This will be done manually in the excercise class. Alternatively you can use the package "MASS" and its function "lda". In this case you can also try "qda".

# Calculate n_k and hat(mu_k) for k = 0 (no heart disease) and k = 1 (heart disease) and Sigma_X (and its inverse)


# Implement functions for discriminant and lda classifier (as in Definition 3.25)
discriminant <- function(x_1,x_2) {
  
}
lda_classifier <- function(x_1, x_2){
  
}
# Make predictions for the test dataset and evaluate error
pred_lda <- 
error_lda<-


###### kNN ###### 
# This will be done manually in the excercise class. Alternatively, you can use the package "class" with its function "knn"

# Implement function that returns indices of k nearest neighbours
find_nearest_neighbour <- function(x_1,x_2,k) {
 
} 
# Implement function that returns predicted class using find_nearest_neighbour
knn_classifier <- function(x_1,x_2,k){
  
}


# Make predictions for every possible k and calculate the respective classification error

# Choose the best k and use this k for error_knn 
best_k <- which(evaluate_k == min(evaluate_k))
error_knn<- evaluate_k[best_k[1]]


###### SVM ###### 
# Use the function svm from the e1071 package  
# This function has lots of different opportunities for use - we want to use it for classification and choose "type = C-classification" and use a factor as response vector y
# You have to choose a kernel, parameters for this kernel (gamma and in case of polynomial kernel the constant) and the penalty term lambda (cost in the package) 
# svm(x=cbind( ... ),y=as.factor( ... ), kernel = "... " , gamma = , cost = ,type="C-classification") )

# In the excercise class we use the two examples from 3.40, polynomial and Gaussian kernel (radial kernel), but the package also provides a sigmoid kernel
# You can find the best combination of given sets of parameters using tune.svm( ... ) by 10 fold cross validation - you can also implement that for fun, but we stick to this automated version 
# For the tuning, you pass a vector of the parameters you want to optimize over (see the manual of e1071, page 41, https://cran.r-project.org/web/packages/e1071/e1071.pdf)

# Gaussial kernel
svm_radial <-              # Use tune.svm to find optimal choice within range of parameters of your choice using a radial kernel
svm_radial_optimal <-      # Use the optimal parameters for your final SVM using a radial kernel
  
# polynomial kernel (we stick to a cubic polynomial due to computation time)
svm_polynomial <-          # Use tune.svm to find optimal choice within range of parameters of your choice using a polynomial kernel of degree 3
svm_polynomial_optimal <-  # Use the optimal parameters for your final SVM using a polynomial kernel of degree 3

# Make predictions for the test dataset and evaluate error for both kernels
pred_radial <- 
error_svm_radial<-

pred_polynomial <- 
error_svm_polynomial<-


