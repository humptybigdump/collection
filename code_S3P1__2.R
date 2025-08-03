# a) Graph CRPS
plot(function(x) pnorm(x),xlim = c(-3,3),ylab = expression(F(x)),col = "blue",)
abline(h = 0,lty = 2,col = "grey")
abline(h = 1,lty = 2,col = "grey")
plot(function(x) ifelse(x >= 1,1,0),add=TRUE,xlim = c(-3,3),col = "red")

# e) Comparison of scores for standard normal prediction
crps_snorm = function(y) y*(2*pnorm(y) - 1) - 1/sqrt(pi) + 2*dnorm(y)
logs_snorm = function(y) -log(dnorm(y))
qs_snorm = function(y) 1/2/sqrt(pi) - 2*dnorm(y)
sphs_snorm = function(y) -dnorm(y)*sqrt(2*sqrt(pi))

plot(crps_snorm,xlim = c(-3,3),ylim = c(-1,5),lty = 1, xlab = "y",ylab = "S(F,y)")
plot(logs_snorm,xlim = c(-3,3),ylim = c(-1,5),lty = 2,add = TRUE)
plot(qs_snorm,xlim = c(-3,3),ylim = c(-1,5),lty = 3,add = TRUE)
plot(sphs_snorm,xlim = c(-3,3),ylim = c(-1,5),lty = 4,add = TRUE)

legend("top",legend = c("CRPS","LogS","QS","SphS"),lty = 1:4)


# f) Simulated time series data
n = 1000

x = ts(rnorm(n+1))
y = x + lag(x,1)
z = ts(ifelse(runif(n,-1,1)<0,-1,1))
x = x[1:n]

par(mfrow = c(2,1))
plot(y)
acf(y)


# Forecast CDFs at all time steps
F.perf <- function(q) pnorm(q - x)
F.clim <- function(q) pnorm(q/sqrt(2))
F.unfo <- function(q) 0.5*(pnorm(q - x) + pnorm(q - x - z))
F.sign <- function(q) pnorm(q + x)


# PIT-Histograms
par(mfrow = c(2,2))
hist(F.perf(y))
hist(F.clim(y))
hist(F.unfo(y))
hist(F.sign(y))


# CRPS
# You can use the functions crps_norm()
# and crps_mixnorm() from the scoringRules package.

# install.packages("scoringRules)
library(scoringRules)

crps.perf = mean(crps_norm(y,x,sd = 1))
crps.clim = mean(crps_norm(y,0,sd = sqrt(2)))
crps.unfo = mean(crps_mixnorm(y,cbind(x,x+z),cbind(rep(1,n),rep(1,n))))
crps.sign = mean(crps_norm(y,-x,sd = 1))


# Log score
# Use logs_norm(), logs_mixnorm() from scoringRules package
logs.perf = mean(logs_norm(y,x,sd = 1))
logs.clim = mean(logs_norm(y,0,sd = sqrt(2)))
logs.unfo = mean(logs_mixnorm(y,cbind(x,x+z),cbind(rep(1,n),rep(1,n))))
logs.sign = mean(logs_norm(y,-x,sd = 1))


results = matrix(c(crps.perf,crps.clim,crps.unfo,crps.sign,
                   logs.perf,logs.clim,logs.unfo,logs.sign),
                 ncol = 2,
                 dimnames = list(c("Perfect","Climatological","Unfocused","Sign-reversed"),
                                 c("CRPS","LogS")))
round(results,2)
