############################################
# Random Walk Model with Noise: Simulation #
############################################

# rm( list=ls() )
# source("RandomWalkNoise")

# pdf()

n <- 100
V <- ts(rnorm(n,0,1), start=1)
W <- ts(rnorm(n,0,2), start=1)

X <- seq(n)
Y <- seq(n)
X[1] <- V[1]
for (t in 1:(n-1))  X[t+1] <- X[t] + V[t]
for (t in 0:n) Y[t] <- X[t] + W[t] 
X <- ts(X, start=1)  
Y <- ts(Y, start=1)

plot(X, ylim=c(min(c(X,Y)),max(c(X,Y))), main="Random Walk with Noise")
points(Y, pch=5)

# dev.off()
