library(readr)

mnist_train <- read_csv("mnist_train.csv") # Adapt path if necessary 
mnist_test <- read_csv("mnist_test.csv") # Adapt path if necessary 

# Calculate A_hat
calculate_A <- function(number){
  indices <- which(mnist_train[,1] == number)
  X_loc <- mnist_train[indices,2:length(mnist_train)]
  mean_loc <- as.numeric(colMeans(X_loc))
  X_loc_tilde <- X_loc
  for (i in 1:dim(X_loc)[1]){
    print(i)
    X_loc_tilde[i,] <- X_loc[i,] - mean_loc
  }
  eig_X_loc <-  eigen(t(as.matrix(X_loc_tilde))%*%as.matrix(X_loc_tilde))
  A_loc <- eig_X_loc
  return(A_loc)
}
# Save all eigenvectors -> much less computation time
A_0 <- calculate_A(0)
A_1 <- calculate_A(1)
A_2 <- calculate_A(2)
A_3 <- calculate_A(3)
A_4 <- calculate_A(4)
A_5 <- calculate_A(5)
A_6 <- calculate_A(6)
A_7 <- calculate_A(7)
A_8 <- calculate_A(8)
A_9 <- calculate_A(9)

classify_obs <- function(obs,q){
  k_0 <- calc_k(obs, as.numeric(colMeans(mnist_train[which(mnist_train[,1] == 0),2:length(mnist_train)])), A_0$vectors[,1:q])
  k_1 <- calc_k(obs, as.numeric(colMeans(mnist_train[which(mnist_train[,1] == 1),2:length(mnist_train)])), A_0$vectors[,1:q])
  k_2 <- calc_k(obs, as.numeric(colMeans(mnist_train[which(mnist_train[,1] == 2),2:length(mnist_train)])), A_2$vectors[,1:q])
  k_3 <- calc_k(obs, as.numeric(colMeans(mnist_train[which(mnist_train[,1] == 3),2:length(mnist_train)])), A_3$vectors[,1:q])
  k_4 <- calc_k(obs, as.numeric(colMeans(mnist_train[which(mnist_train[,1] == 4),2:length(mnist_train)])), A_4$vectors[,1:q])
  k_5 <- calc_k(obs, as.numeric(colMeans(mnist_train[which(mnist_train[,1] == 5),2:length(mnist_train)])), A_5$vectors[,1:q])
  k_6 <- calc_k(obs, as.numeric(colMeans(mnist_train[which(mnist_train[,1] == 6),2:length(mnist_train)])), A_6$vectors[,1:q])
  k_7 <- calc_k(obs, as.numeric(colMeans(mnist_train[which(mnist_train[,1] == 7),2:length(mnist_train)])), A_7$vectors[,1:q])
  k_8 <- calc_k(obs, as.numeric(colMeans(mnist_train[which(mnist_train[,1] == 8),2:length(mnist_train)])), A_8$vectors[,1:q])
  k_9 <- calc_k(obs, as.numeric(colMeans(mnist_train[which(mnist_train[,1] == 9),2:length(mnist_train)])), A_9$vectors[,1:q])
  values <- c(k_0, k_1,k_2,k_3,k_4,k_5,k_6,k_7,k_8,k_9)
  return(which.min(values)-1) # R indices
  }

calc_k <- function(obs, mu, A){
  return(norm(obs - mu - A %*% (t(A) %*% (obs - mu)), type="2"))
}

# predict for q

predict_q <- function(q){
  pred_loc <- c() 
  for (i in 1:dim(mnist_test)[1]) {
    print(i)
    pred_loc <- c(pred_loc, classify_obs(as.numeric(mnist_test[i,2:dim(mnist_test)[2]]) , q))
  }
  return(pred_loc)
}
pred_4 <- predict_q(4)
error_4 <- sum(abs(sign(mnist_test[,1] - pred_4))) # 1044

pred_1 <- predict_q(1)
error_1 <- sum(abs(sign(mnist_test[,1] - pred_1))) # 1331

pred_2 <- predict_q(2)
error_2 <- sum(abs(sign(mnist_test[,1] - pred_2))) # 1234

# Calculate error for q = 1,...,6 for the first 1000 obs. of the test dataset 
predict_fast <- function(q){
  pred_loc <- c() 
  for (i in 1:1000) {
    print(i)
    pred_loc <- c(pred_loc, classify_obs(as.numeric(mnist_test[i,2:dim(mnist_test)[2]]) , q))
  }
  return(sum(abs(sign(mnist_test[1:1000,1] - pred_loc))) )
}

error <- c()
for (i in 1:6) {
  loc <- predict_fast(i)
  error <- c(error, loc)
}

plot(error)


# If you want to use a package instead, you could use prcomp from the package stats. 
# You can also look into visualization like fviz_eig from the package factoextra, this is nice for choosing the dimension d based on the training dataset
# This will take some time on the training set too - try a small subset of the data first


