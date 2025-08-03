# initialization
n <- 10000

a1 <- rnorm(n,0,1)
a2 <- rnorm(n,0,1)
a3 <- rnorm(n,0,1)
a4 <- rnorm(n,0,1)

x0 = pnorm(a1+a2+a3+a4)
x1 = pnorm((a1+a2+a3)/sqrt(2))
x2 = pnorm((a1+a2)/sqrt(3))
x3 = pnorm(a1/sqrt(4))
x <- list(x0,x1,x2,x3)

y <- rbinom(n, size = 1, prob = x0)

colors <- c("#E69F00", "#0072B2", "#D55E00", "#CC79A7")

# Murphy curve

score <- function(theta, x, y){
  if (x > theta && y==0){
    return(2*theta)
  }
  if (x < theta && y==1){
    return(2*(1-theta))
  }
  if (x==theta){
    return(2*theta*(1-theta))
  }
  return(0)
}

murphy_curve <- function(x_,y_){
  curve <- function(theta) {
    # returns the mean score
    scores <- sapply(1:length(x_), function(i) score(theta, x_[i], y_[i]))
    return(mean(scores))
  }
  return(curve)
}

# plot Murphy curve

par(pin = c(2.9, 2.9)) # to ensure that the plot is a square
plot(NA, xlim = c(0,1), ylim = c(0,.4),
     xlab = expression(theta), ylab = expression(bar(S)[theta]), lwd = 3)
theta_vec <- seq(0, 1, length.out = 100)
for(i in 1:length(x)){
  lines(theta_vec, sapply(theta_vec, murphy_curve(x[[i]],y)), col = colors[i])
}

legend("topright",
       legend = c(expression(X[0]),expression(X[1]), 
                  expression(X[2]),expression(X[3])),
       lty = 1, col = colors, lwd = 3)

# ROC curve

hit_rate <- function(x,y,t){
  num_ones <- sum(y == 1)
  num_true_pos <- sum(y == 1 & x > t)
  if (num_ones > 0){
    return(num_true_pos/num_ones)
  }
  else{
    return(1)
  }
}

false_alarm_rate <- function(x,y,t){
  num_zeros <- sum(y == 0)
  num_false_pos <- sum(y == 0 & x > t)
  if (num_zeros > 0){
    return(num_false_pos/num_zeros)
  }
  else{
    return(0)
  }
}

# plot ROC
par(pin = c(2.9, 2.9)) # to ensure that the plot is a square
plot(NA, xlim = c(0,1), ylim = c(0,1),
     xlab = "False alarm rate", ylab = "Hit rate", lwd = 3)
for(i in 1:length(x)){
  t_vec <- seq(0, 1, length.out = 100)
  far <- sapply(t_vec, function(t) false_alarm_rate(x[[i]], y, t))
  hr <- sapply(t_vec, function(t) hit_rate(x[[i]], y, t))
  lines(far, hr, col = colors[i])
}
legend("bottomright",
       legend = c(expression(X[0]),expression(X[1]), 
                  expression(X[2]),expression(X[3])),
       lty = 1, col = colors, lwd = 3)

