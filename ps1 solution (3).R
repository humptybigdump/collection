# = = = = = = = = = = = = = = = = = = = = = = = = = #
#                                                   #
#         Financial Econometrics 1                  #
#             Problem Set 1 (3)                     #
#                                                   #
# = = = = = = = = = = = = = = = = = = = = = = = = = #

x <- rnorm(100)
fun.ecdf <- ecdf(x)
my.ecdf <- fun.ecdf(sort(x))

rm(list=ls())

# load packages
library("PerformanceAnalytics")
library("tseries")
library("zoo")
library("sn")

# = = = = = = = = = = = = = = = = = = = = = = = = = =
# Problem 3 ####
# = = = = = = = = = = = = = = = = = = = = = = = = = =
# get prices of MSFT and SP500

MSFT.prices = get.hist.quote(instrument="msft", start="1998-01-01",
                             end="2012-05-31", quote="Adjusted",
                             origin="1970-01-01", provider="yahoo",
                             compression="m", retclass="zoo")

SP500.prices = get.hist.quote(instrument="^gspc", start="1998-01-01",
                              end="2012-05-31", quote="AdjClose",
                              provider="yahoo", origin="1970-01-01",
                              compression="m", retclass="zoo")

# Adjust time series ordering and create one merged zoo object
# Transform prices into continuously compounded returns.

colnames(MSFT.prices) = "MSFT"
colnames(SP500.prices) = "SP500"

merged_df = cbind(MSFT.prices, SP500.prices)
head(merged_df, n=3)

MSFT.ret <- coredata(diff(log(MSFT.prices)))
SP500.ret <- coredata(diff(log(SP500.prices)))

merged_df$MSFT.ret <- c(NA, MSFT.ret)
merged_df$SP500.ret <- c(NA, SP500.ret)

merged_df <- coredata(merged_df)
merged_df <- na.omit(merged_df)

# = = = = = = = = = = = = = = = = = = = = = = = = = =
## i ####
# = = = = = = = = = = = = = = = = = = = = = = = = = =

# Empirical quantiles and summary statistics

# empirical quantiles for MSFT
quantile(MSFT.ret)
quantile(MSFT.ret, probs=c(0.01,0.05))

# compare to normal quantiles
qnorm(p=c(0.01, 0.05), mean=mean(MSFT.ret), sd=sd(MSFT.ret))

# empirical and normal quantiles for SP500
quantile(SP500.ret)
quantile(SP500.ret, probs=c(0.01,0.05))
qnorm(p=c(0.01,0.05), mean=mean(SP500.ret), sd=sd(SP500.ret))

# NOTE: 
# empirical quantiles are useful for computing historical simulated value-at-risk (VaR)

# = = = = = = = = = = = = = = = = = = = = = = = = = =
## ii ####
# = = = = = = = = = = = = = = = = = = = = = = = = = =

for (i in c(3,4)){
    
    # compute median
    med <- median(merged_df[,i])
    # compute interquartile range IQR
    IMQ <- quantile(merged_df[,i], probs=0.75) - quantile(merged_df[,i], probs=0.25)
    
    print(paste0("for ", names(merged_df)[i]))
    print(paste0("median = ", round(med, 2), ", IMQ = ", round(IMQ, 2)))
    
}

# use apply to get values for both cols
mean.vals = apply(merged_df, 2, mean)
var.vals = apply(merged_df, 2, var)
sd.vals = apply(merged_df, 2, sd)
skew.vals = apply(merged_df, 2, skewness)
kurt.vals = apply(merged_df, 2, kurtosis)

stats.mat = rbind(mean.vals, var.vals, sd.vals, 
                  skew.vals, kurt.vals); stats.mat

# = = = = = = = = = = = = = = = = = = = = = = = = = =
## iii - empirical distribution function of simulated data #### 
# = = = = = = = = = = = = = = = = = = = = = = = = = =

# simulate data (gaussian white noise)
set.seed(123)
gwn = rnorm(length(MSFT.ret), mean = mean(MSFT.ret), sd = sd(MSFT.ret))
gwn.zoo = zoo(gwn, index(MSFT.ret))
n1 = length(gwn)

# compute and plot empirical distribution function for simulated gaussian data
# "s" is for steps

plot(stepfun(sort(gwn), (1:(n1+1))))

plot(sort(gwn), (1:n1)/n1, type="l", ylim=c(0,1), lwd=2,
     main="Empirical CDF of Gaussian data", ylab="#x(i) <= x")

# or use ecdf function directly
plot(ecdf(gwn), lwd=2)

# compare empirical cdf to standard normal cdf for simulated gaussian data
x1 = sort(gwn)
y1 = pnorm(x1, mean=mean(MSFT.ret), sd=sd(MSFT.ret))			
F.hat = 1:n1/n1 # empirical cdf

plot(x1, y1, main="Empirical CDF vs. Normal CDF for Gaussian data",
     type="l", lwd=2, xlab="standardized gwn", ylab="CDF")

points(x1, F.hat, type="s", lty=1, lwd=2, col="orange")

legend(x="topleft", legend=c("Normal CDF","Empirical CDF"),
       lty=c(1,1), lwd=2, col=c("black","orange"))

# = = = = = = = = = = = = = = = = = = = = = = = = = =
## iv - empirical distribution function of returns ####
# = = = = = = = = = = = = = = = = = = = = = = = = = =

# compare empirical cdf to standard normal cdf for MSFT returns
F.hat   = 1:n1/n1               
x1      = sort(coredata(MSFT.ret))		
y1      = pnorm(x1, mean=mean(MSFT.ret), sd=sd(MSFT.ret))

plot(x1, y1, main="Empirical CDF vs. Normal CDF for MSFT returns",
     type="l",lwd=2,xlab="standardized MSFT returns",ylab="CDF")

points(x1, F.hat, type="s", lty=1, lwd=3, col="orange")

# compare empirical cdf to standard normal cdf for SP500 returns
F.hat   = 1:n1/n1	
x1      = sort(coredata(SP500.ret))
y1      = pnorm(x1, mean=mean(SP500.ret), sd=sd(SP500.ret))

plot(x1, y1, main="Empirical CDF vs. Normal CDF for SP500 returns",
     type="l",lwd=2,xlab="standardized SP500 returns",ylab="CDF")

points(x1, F.hat, type="s", lty=1, lwd=3, col="orange")

# = = = = = = = = = = = = = = = = = = = = = = = = = =
## v ####
# = = = = = = = = = = = = = = = = = = = = = = = = = =
# Formal KS-tests

# H0 = "Distributions are equal"
# H1 = "Not equal"

# low power (Pr(H1|H1) in finite samples: If we don't reject, we can't be sure they're equal
# alternative tests for normal dist: Jarque-Bera test, Lilliefors test
# general alternative (also two-sample): Cramer-von Mises test 

ks.test(MSFT.ret, gwn) # no rejection of null ... since we simulated gwn using MSFT mean and sd
ks.test(SP500.ret, gwn) # rejection of null, no normality, looks like fat tails

# = = = = = = = = = = = = = = = = = = = = = = = = = =
## vi - QQ plots ####
# = = = = = = = = = = = = = = = = = = = = = = = = = =

# compare empirical quantiles of returns against 4 distributions
# normal, student-t, log-normal, skewed normal

# function to plot this for returns
plot_qq_returns <- function(returns.mat, df=3, alpha=3) {
  
  n.ret     <- dim(returns.mat)[1]

  qq.norm   <- rnorm(n.ret)
  qq.t      <- rt(n.ret,df=3)
  qq.lnorm  <- rlnorm(n.ret)
  qq.skew   <- rsn(n.ret,alpha=3)
  
  par(mfrow=c(2,2))
  
  #1st
  qqplot(qq.norm, returns.mat)
  qqline(returns.mat, distribution = qnorm)
  
  #2nd
  qqplot(qq.t, returns.mat)
  qqline(returns.mat, distribution = function(q) qt(q, df=df))
  
  #3rd
  qqplot(qq.lnorm, returns.mat)
  qqline(returns.mat, distribution = qlnorm)
  
  #4th
  qqplot(qq.skew, returns.mat)
  qqline(returns.mat, distribution = function(q) qsn(q, alpha =alpha))

  par(mfrow=c(1,1))
  
}

# plot for MSFT
plot_qq_returns(MSFT.ret)

# plot for SP500
plot_qq_returns(SP500.ret)

# = = = = = = = = = = = = = = = = = = = = = = = = = =
## vi - KS-tests reapplied ####
# = = = = = = = = = = = = = = = = = = = = = = = = = =

gdata           = rnorm(100)
tdata           = rt(100, df=3)     # student t: fat tails
lndata          = rlnorm(100)       # log-normal: asymmetric 
skew.norm.data  = rsn(100, alpha=3) # skew normal: asymmetric

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# MSFT
n.msft<-dim(MSFT.ret)[1]
sd.msft<-sd(MSFT.ret)
mean.msft<-mean(MSFT.ret)

ks.test(MSFT.ret, gdata)            # reject ... since we did not specify mean or variance and are simply comparing to N(0,1)
ks.test(MSFT.ret, gwn)              # no rejection since mean and variance are adjusted to MSFT
ks.test(MSFT.ret, tdata)            # not adjusted ...
ks.test(MSFT.ret, lndata)           # not adjusted ...
ks.test(MSFT.ret, skew.norm.data)   # not adjusted ...

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# SP500
gwn.sp500 <- rnorm(length(SP500.ret), mean=mean(SP500.ret), sd=sd(SP500.ret))
ks.test(SP500.ret,pnorm)            # reject ... since we did not specify mean or variance and are simply comparing to N(0,1)
ks.test(SP500.ret, gwn.sp500)       # no rejection since mean and variance are adjusted to SP500
ks.test(MSFT.ret, tdata)            # not adjusted ...
ks.test(MSFT.ret, lndata)           # not adjusted ...
ks.test(MSFT.ret, skew.norm.data)   # not adjusted ...

