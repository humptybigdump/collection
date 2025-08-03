#Financial Econometrics PS0
#Intro to R

# Problem 1

x1 <- c(1,4,19)
x2 <- c( 14,-6,0.3)
x3 = x1+x2

M1 = cbind(x1,x2) # or matrix(c(x1,x2), ncol=2)

? runif # for checking uniform distribution
? rnorm # for checking normal distribution
set.seed(42)
x4<- runif(100, min=-2, max=2)
x5<- rnorm(100, mean= 4, sd = 2)

plot(x4,x5)
model1<- lm(x5~x4)
abline(coefficients(model1))

#######################################################

# Problem 2

A = cbind( c(3,-3, 1), c(-3, 2,0) , c(1,0,4))
B = matrix(c(2,0,0,0,5,-1,0,0,1), nrow = 3, ncol=3)
X = cbind(rep(1,6),c(0,2,8,6,2,7))
y = c(1,3,2,5,1,0)

t(A)
t(B)

A+B
A-B

C1 <- A*A # elementwise multiplication
C2 <- A%*%A # matrix multiplication

D<- 2* A


A%*%B
B%*%A

solve(A)
solve(B)


library(Matrix)# use ?? rank to find the appropriate function for the rank
rankMatrix(X)
rankMatrix(t(X)%*%X)
rankMatrix(X%*%t(X))

#dimensions with base R command dim
dim(X)
dim(t(X)%*%X)
dim(X%*%t(X))
