# Exercise 3 - Problem 2

# a)
set.seed(1337)

# simulate data
sim_t <- rt(n=500, df=15)


# b)
### CV FUNCTIONS
# build kernel function
k_gauss <- function(x){exp(-x^2/2)/sqrt(2*pi)}

# build convolution of kernel function (see exercise notes)
k_bar_gauss <- function(x){exp(-x^2/4)/sqrt(4*pi)}

# build function that generates matrix with (Xi-Xj)/h from vector X and runs kernel function on it 
# afterwards return matrix with zero diagonal  or not (i.e. j \neq i)
double_sum <- function(X=sim_t, h, diag_Null=TRUE, kernel_fun,...)
{
  len <- length(X)
  temp_mat <- matrix(NA, ncol=len, nrow=len)
  for (i in 1:len)
  {
    for (j in 1:len)
    {
      temp_mat[i,j] <- kernel_fun((X[i]-X[j])/h,...)
    }
  }
  if(diag_Null){diag(temp_mat) <- 0}
  return(temp_mat)
}

# important for R: If you break lines in Code, usually R continues if it expects the statement to continue
# here: indicated by round brackets at beginning and end of the statement, without them, only second part would be returned

# leave-one-out CV
loo_cv <- function(h, n=length(sim_t), X_vec=sim_t)
{
  ( 1/(n^2*h) * sum(double_sum(X_vec, h, diag_Null=FALSE, kernel_fun=k_bar_gauss))
   - (2/(n*(n-1)*h) * sum(double_sum(X_vec, h, kernel_fun=k_gauss)) ) )
}

# first plot empirical density / distribution and look at it
hist(sim_t, freq = FALSE) # label with relative, not absolute number of observations
lines(density(sim_t), col="red")

# naive optimization: Compute LOO_CV for sequence of potential bandwidths & choose h with minimal LOO_CV
seq_h <- seq(10^(-5), 1, length.out=100) # sequence of initial (potential) bandwidths
loo_cv_seq_h <- sapply(seq_h, FUN=loo_cv, n=length(sim_t), X_vec=sim_t)
plot(seq_h, loo_cv_seq_h, ylim=c(min(loo_cv_seq_h), -0.15))
opt_0 <- seq_h[which.min(loo_cv_seq_h)]

### FIND OPTIMAL BANDWIDTH
# optimize h using numerical algorithms
# install.packages("np")
library(np) # Nonparametric Kernel Smoothing Methods for Mixed Data Types

# first argument as starting value, start high (see plot before)
opt_1 <- optim(0.2, loo_cv, n=length(sim_t), X_vec=sim_t) # uses Nelder-Mead method
opt_2 <- optimize(loo_cv, c(0,1)) # combination of golden section search and successive parabolic interpolation, and was designed for use with continuous functions
opt_3 <- npudensbw(sim_t, bwmethod = "cv.ls") # computes a bandwidth object for a p-variate kernel unconditional density estimator defined over mixed continuous and discrete (unordered, ordered) data using least-squares cross validation using the method of Li and Racine (2003).
dens_1 <- npudens(opt_3) # computes kernel unconditional density estimates on evaluation data

# kernel density estimator
f_gauss <- function(x, h=0.5){1/(length(sim_t)*h) * sum(k_gauss((sim_t-x)/h))}

# write function to generate vector of values
plot_fun <- function(interval=c(-4,4), h)
{
  int_temp <- seq(interval[1], interval[2], length.out=150)
  y_val <- sapply(int_temp, f_gauss, h=h)
  x_val <- int_temp
  return(cbind(x_val, y_val))
}

# true density
true_dens <- function(x){dt(x,df=15)}

curve(true_dens, -4, 4, col="black")
lines(plot_fun(h=opt_0)[,1], plot_fun(h=opt_0)[,2], col="blue")
lines(plot_fun(h=opt_1$par)[,1], plot_fun(h=opt_1$par)[,2], col="darkblue")
lines(plot_fun(h=opt_2$minimum)[,1], plot_fun(h=opt_2$minimum)[,2], col="green")
lines(plot_fun(h=opt_3$bw)[,1], plot_fun(h=opt_3$bw)[,2], col="red")
lines(density(sim_t), col="blue")

lines(plot_fun(h=0.1)[,1], plot_fun(h=0.1)[,2], col="orange") # undersmoothed
lines(plot_fun(h=0.7)[,1], plot_fun(h=0.7)[,2], col="purple") # oversmoothed

# try out with different simulated data (less normal)
true_dens_2 <- function(x){dt(x, df=1)}

set.seed(1338)

sim_t1 <- rt(n=500, df=1)

# first plot it and look at it
hist(sim_t1, freq=FALSE)
lines(density(sim_t1), col="red")

loo_cv_seq_h_t1 <- sapply(seq_h, FUN=loo_cv, n=length(sim_t1), X_vec=sim_t1)
plot(seq_h, loo_cv_seq_h_t1, ylim=c(-0.15,-0.14))

opt_t1_1 <- optim(0.3, loo_cv, n=length(sim_t1), X_vec=sim_t1)
opt_t1_2 <- optimize(loo_cv, c(0,1), n=length(sim_t1), X_vec=sim_t1)
opt_t1_3 <- npudensbw(sim_t1, bwmethod="cv.ls")
dens_t1_1 <- npudens(opt_t1_3)

interv <- c(-10,10)
curve(true_dens_2, interv[1], interv[2], col="black")
lines(plot_fun(interv ,h=opt_t1_1$par)[,1], plot_fun(interv, h=opt_t1_1$par)[,2], col="darkblue")
lines(plot_fun(interv, h=opt_t1_2$minimum)[,1], plot_fun(interv, h=opt_t1_2$minimum)[,2], col="green")
lines(plot_fun(interv, h=opt_t1_3$bw)[,1], plot_fun(interv, h=opt_t1_3$bw)[,2], col="red")

lines(plot_fun(interv, h=0.1)[,1], plot_fun(interv, h=0.1)[,2], col="orange") # undersmoothed
lines(plot_fun(interv, h=0.7)[,1], plot_fun(interv, h=0.7)[,2], col="purple") # oversmoothed

lines(density(sim_t1), col="blue")

