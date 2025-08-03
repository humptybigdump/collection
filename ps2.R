##Financial Econometrics Exercise 
##PS2



# Problem 3
##############################################

#	Core R functions used:
#
#	acf				compute sample autocovariances or autocorrelations	
#	apply				apply function to rows or columns of matrix
#	args				determine agruments of a function
#	boxplot			compute boxplot
#	cbind				combine data objects vertically
#	class				determine class of object
#	colIds			get column names from object
#	cor				compute sample correlation matrix
#	density			compute smoothed histogram
#   ecdf      compute empirical CDF
#	end				get end date of time series
#	help				invoke help system
#	hist				compute histogram
#	legend			add legend to graph
#	length			compute column length of matrix
#	library			load R package
#	mean				compute sample mean
#	names				show names of object
#	par				set graphics parameters
#	plot				generic plot function
#	points			add points to a graph
#	qqline			add line to qq-plot
#	qqnorm			qq-plot against normal distribution
#	qt				compute quantiles of student t distribution
#	rlnorm			generate random data from log-normal distribution
#	rt				generate random data from student t distribution
#	scale				standardize a vector of data
#	pnorm				compute normal CDF
#	seq				generate sequence of numbers
#	sort				sort data
#	start				get start date of time series
#	stdev				compute sample standard deviation
#	ts.plot			time series plot
#	var				compute sample variance or covariance matrix
#	?				invoke help system
#
# R packages used
# PerformanceAnalytics
#   skewness    compute sample skewness
#   kurtosis    compute excess kurtosis
#   tseries			Time series and computational finance
#	get.hist.quote	load data from Yahoo!
#	zoo
#	plot.zoo		plot zoo object
#   sn  
#   rsn       simulate from skew-normal distribution
#   dsn       compute pdf of skew-normal distribution

# set options
options(digits=4, width=70)


# load packages
library(PerformanceAnalytics)
library("tseries")
library("zoo")
library(sn)


## i
###############################
# get monthly adjusted closing price data on MSFT and SP500 from Yahoo
# using the tseries function get.hist.quote. Set sample to Jan 1998 through
# May 2012. Note: if you are not careful with the start and end dates
# or if you set the retclass to "ts" then results might look weird
#
MSFT.prices = get.hist.quote(instrument="msft", start="1998-01-01",
                             end="2012-05-31", quote="AdjClose",
                             provider="yahoo", origin="1970-01-01",
                             compression="m", retclass="zoo")
class(MSFT.prices)
colnames(MSFT.prices)
start(MSFT.prices)
end(MSFT.prices)
head(MSFT.prices)

# change date index class to yearmon
# index(MSFT.prices) = as.yearmon(index(MSFT.prices))
# note: x-axis on graphs do not look good if this is done

SP500.prices = get.hist.quote(instrument="^gspc", start="1998-01-01",
                             end="2012-05-31", quote="AdjClose",
                             provider="yahoo", origin="1970-01-01",
                             compression="m", retclass="zoo")
# change date index class to yearmon
# index(SP500.prices) = as.yearmon(index(SP500.prices))

# add column names 
colnames(MSFT.prices) = "MSFT"
colnames(SP500.prices) = "SP500"

# create zoo object with both prices
MSFTSP500.prices = merge(MSFT.prices, SP500.prices)
head(MSFTSP500.prices, n=3)

#
# compute cc returns 
#

MSFT.ret = diff(log(MSFT.prices))
SP500.ret = diff(log(SP500.prices))
MSFTSP500.ret = merge(MSFT.ret,SP500.ret)
colnames(MSFTSP500.ret)
head(MSFTSP500.ret, n=3)

class(MSFT.ret)
class(SP500.ret)
class(MSFTSP500.ret)

# convert zoo data to matrix data for non time series analysis
# many R functions do not have methods implemented for zoo objects
# and results may be unpredictable
MSFT.ret.mat = coredata(MSFT.ret)
colnames(MSFT.ret.mat) = "MSFT"
rownames(MSFT.ret.mat) = as.character(index(MSFT.ret))

SP500.ret.mat = coredata(SP500.ret)
colnames(SP500.ret.mat) = "SP500"
rownames(SP500.ret.mat) = as.character(index(SP500.ret))

MSFTSP500.ret.mat = coredata(MSFTSP500.ret)
colnames(MSFTSP500.ret.mat) = c("MSFT","SP500")
rownames(MSFTSP500.ret.mat) = as.character(index(MSFTSP500.ret))


#
# Empirical quantiles and summary statistics
#


# use quantile() function
?quantile
args(quantile)
# empirical quantiles for MSFT
quantile(MSFT.ret.mat)
quantile(MSFT.ret.mat,probs=c(0.01,0.05))
# compare to normal quantiles
qnorm(p=c(0.01,0.05), mean=mean(MSFT.ret.mat), 
      sd=sd(MSFT.ret.mat))
# empirical and normal quantiles for SP500
quantile(SP500.ret.mat)
quantile(SP500.ret.mat,probs=c(0.01,0.05))
qnorm(p=c(0.01,0.05), mean=mean(SP500.ret.mat), 
      sd=sd(SP500.ret.mat))



# note: empirical quantiles are useful for computing historical simulation value-at-risk (VaR)

# what is the maximum loss that we will encounter with a certain probability:
# since we assume randomness in prices, we can not limit our losses for certain,
# but given distributional assumptions, we can calculate how bad it can go with 
# a certain probability, so 95% historical-VaR of X roughly tells us:
# what's the worst that can happen (where is the limit of our losses) with 95%
# probability (based on historical assumptions)

#
# historical VaR
#

#log-returns are monotone transformation, works too
q.01 = quantile(MSFT.ret.mat, probs=0.01)
q.05 = quantile(MSFT.ret.mat, probs=0.05) 
q.01
q.05
VaR.01 = 100000*(exp(q.01) - 1) #if we invested 100.000
VaR.05 = 100000*(exp(q.05) - 1) #if we invested 100.000
VaR.01
VaR.05

W = 100000
q.r.msft = quantile(MSFT.ret.mat, probs=c(0.01, 0.05))
q.r.sp500 = quantile(SP500.ret.mat, probs=c(0.01, 0.05))
VaR.msft = W*(exp(q.r.msft) - 1)
VaR.sp500 = W*(exp(q.r.sp500) - 1)
VaR.msft
VaR.sp500

###############################
## ii
###############################
# compute median
median(MSFT.ret.mat)


# compute interquartile range IQR
quantile(MSFT.ret.mat,probs=0.75) - quantile(MSFT.ret.mat,probs=0.25)

# use mean, var, sd, skewness and kurtosis functions
mean(MSFT.ret.mat)
var(MSFT.ret.mat)
sd(MSFT.ret.mat)
skewness(MSFT.ret.mat)
kurtosis(MSFT.ret.mat)
# kurtosis function actually computes excess kurtosis
kurtosis(MSFT.ret) + 3
# note: summary is a generic function with several methods
summary(MSFT.ret.mat)

# Use apply to compute statistics for columns
mean.vals = apply(MSFTSP500.ret.mat, 2, mean)
var.vals = apply(MSFTSP500.ret.mat, 2, var)
sd.vals = apply(MSFTSP500.ret.mat, 2, sd)
skew.vals = apply(MSFTSP500.ret.mat, 2, skewness)
kurt.vals = apply(MSFTSP500.ret.mat, 2, kurtosis)
stats.mat = rbind(mean.vals,var.vals, sd.vals,skew.vals,kurt.vals )

###############################
## iii
###############################
#
# empirical distribution function

# simulate data
set.seed(123)
gwn = rnorm(length(MSFT.ret),mean=mean(MSFT.ret),sd=sd(MSFT.ret))
gwn.zoo = zoo(gwn, index(MSFT.ret))

#
 
# compute and plot empirical distribution function for simulated gaussian data
# "s" is for steps
n1 = length(gwn)
plot(sort(gwn),(1:n1)/n1,type="l",ylim=c(0,1), col="slateblue1", lwd=2,
     main="Empirical CDF of Gaussian data", ylab="#x(i) <= x")

#or use ecdf function
plot(ecdf(gwn),col="slateblue1",lwd=2)

# compare empirical cdf to standard normal cdf for simulated gaussian data
z1 = scale(gwn)			# standardize to have mean zero and sd 1
n1 = length(gwn)
F.hat = 1:n1/n1			# empirical cdf
x1 = sort(z1)				# sort from smallest to largest
y1 = pnorm(x1)			# compute standard normal cdf at x

# The following plot options are used
# type		determine type of plot: "l" is line plot; "s" is step plot
# lty		line type: 1 is solid line; 3 is dot-dashed line
# lwd		line thickness: higher values give thicker lines
# col		line color: 1 is black, 2 is blue etc. 
# For help on plot options, see help(par)

plot(x1,y1,main="Empirical CDF vs. Normal CDF for Gaussian data",
     type="l",lwd=2,xlab="standardized gwn",ylab="CDF")
points(x1,F.hat, type="s", lty=1, lwd=3, col="orange")
legend(x="topleft",legend=c("Normal CDF","Empirical CDF"),
       lty=c(1,1), lwd=2, col=c("black","orange"))

###############################
## iv
###############################

# compare empirical cdf to standard normal cdf for MSFT returns
z1 = scale(MSFT.ret.mat)			# standardize to have mean zero and sd 1
n1 = length(MSFT.ret.mat)
F.hat = 1:n1/n1			# empirical cdf
x1 = sort(z1)				# sort from smallest to largest
y1 = pnorm(x1)			# compute standard normal cdf at x

# or use ecdf()

plot(x1,y1,main="Empirical CDF vs. Normal CDF for MSFT returns",
     type="l",lwd=2,xlab="standardized MSFT returns",ylab="CDF")
points(x1,F.hat, type="s", lty=1, lwd=3, col="orange")
legend(x="topleft",legend=c("Normal CDF","Empirical CDF"),
       lty=c(1,1), lwd=c(2,3), col=c("black","orange"))

# compare empirical cdf to standard normal cdf for SP500 returns
z1 = scale(SP500.ret.mat)			# standardize to have mean zero and sd 1
n1 = length(SP500.ret.mat)
F.hat = 1:n1/n1			# empirical cdf
x1 = sort(z1)				# sort from smallest to largest
y1 = pnorm(x1)			# compute standard normal cdf at x

plot(x1,y1,main="Empirical CDF vs. Normal CDF for SP500 returns",
     type="l",lwd=2,xlab="standardized SP500 returns",ylab="CDF")
points(x1,F.hat, type="s", lty=1, lwd=3, col="orange")
legend(x="topleft",legend=c("Normal CDF","Empirical CDF"),
       lty=c(1,1), lwd=c(2,3), col=c("black","orange"))

###############################
## v
###############################
# Formal KS-tests

#Null: "Distributions are equal"
#Alternative: "Not equal"

#low power (Pr(H1|H1) in finite samples: If we don't reject, we can't be sure they're equal
#alternative for test against normal: Jarque-Bera test, Lilliefors test
#general alternative (also two-sample): Cramér-von Mises test 

ks.test(MSFT.ret.mat,gwn) #no rejection of null
ks.test(SP500.ret.mat,gwn) #rejection of null, looks like fat tails

###############################
## vi
###############################

#
# QQ plots: compare empirical quantiles to those from normal distribution
#

par(mfrow=c(2,2))	# 4 panel layout: 2 rows and 2 columns

#plot normal against simulated gwn
#add add line that passes through 1st and 3rd quantile of gwn and is normal otherwise
	qqnorm(gwn, main="Gaussian White Noise", col="slateblue1")
	qqline(gwn)
	qqnorm(MSFT.ret.mat, main="MSFT Returns", col="slateblue1")
	qqline(MSFT.ret.mat)
	qqnorm(SP500.ret.mat, main="SP500 Returns", col="slateblue1")
	qqline(SP500.ret.mat)
par(mfrow=c(1,1))

# Data for student t with 3 df: tails fatter than normal
set.seed(123)
tdata = rt(100,df=3)		# Student-t with 3 df
gdata = rnorm(100)			# N(0,1) data
xx = seq(from=-5,to=5,length=100)

# data for log-normal: asymmetric distribution
lndata = rlnorm(100)
yy = seq(from=0, to = 5, length=100)

# data for skew normal: asymmetric distribution
skew.norm.data = rsn(100, alpha = 3)
zz = seq(from=-3, to = 3, length=100)


par(mfrow=c(2,3))
	# 1st plot
	plot(xx,dnorm(xx),type="l", lwd=2,
	     main="Normal and Student-t with 3 df", xlab = "z, t", ylab = "pdf")
	points(xx,dt(xx,df=3), type="l", col="orange", lwd=3)
	#legend(x="topright", legend=c("Normal","Student-t"), lty=c(1,1), col=c("black","orange"),
	#       lwd=c(2,3))
	
	# 2nd plot
	plot(yy,dnorm(yy,sd=1),type="l", lwd=2, ylim=c(0,0.7),
	     main="Normal and log-Normal", xlab = "z, log-z", ylab = "pdf")
	points(yy,dlnorm(yy), type="l", lwd=3, col="orange")
	#legend(x="topleft", legend=c("Normal","Skew-Normal"), lty=c(1,1), col=c("black","orange"),
	#      lwd=c(2,3))
	# 3rd plot
	plot(zz,dnorm(zz,sd=1),type="l", lwd=2, ylim=c(0,0.7),
	     main="Normal and Skew-Normal", xlab = "z, skew-z", ylab = "pdf")
	points(zz,dsn(zz, alpha=3), type="l", lwd=3, col="orange")
	#legend(x="topleft", legend=c("Normal","Skew-Normal"), lty=c(1,1), col=c("black","orange"),
	#      lwd=c(2,3))

	# 4th plot
	qqnorm(tdata)
	qqline(tdata)
	# 5th plot
	qqnorm(lndata)
	qqline(lndata)
	# 6th plot
	qqnorm(skew.norm.data)
	qqline(skew.norm.data)
par(mfrow=c(1,1))

##compare Returns against the 4: normal, t, lognormal, skewed normal

#function to plot this for returns
plot_qq_returns<-function(returns.mat,df=3,alpha=3) {
  
  par(mfrow=c(2,2))
  n.ret<-dim(returns.mat)[1]
  
  
  set.seed(1337)
  qq.norm<-rnorm(n.ret)
  qq.t<-rt(n.ret,df=3)
  qq.lnorm<-rlnorm(n.ret)
  qq.skew<-rsn(n.ret,alpha=3)
  
  #1st
  qqplot(qq.norm,returns.mat)
  qqline(returns.mat,distribution = qnorm)
  #2nd
  qqplot(qq.t,returns.mat)
  qqline(returns.mat,distribution = function(q) qt(q,df=df))
  #3rd
  qqplot(qq.lnorm,returns.mat)
  qqline(returns.mat,distribution = qlnorm)
  #4th
  qqplot(qq.skew,returns.mat)
  qqline(returns.mat,distribution = function(q) qsn(q,alpha =alpha))
  
  #
  par(mfrow=c(1,1))
}

#plot for MSFT
plot_qq_returns(MSFT.ret.mat)

#plot for SP500
plot_qq_returns(SP500.ret.mat,df=1)

###############################
## vi
###############################

# Formal KS-tests reapplied

## MSFT
n.msft<-dim(MSFT.ret.mat)[1]
sd.msft<-sd(MSFT.ret.mat)
mean.msft<-mean(MSFT.ret.mat)

ks.test(MSFT.ret.mat,pnorm) #did not specify mean or variance
ks.test(MSFT.ret.mat, gwn) #no rejection

library(nortest)
lillie.test(MSFT.ret.mat) #rejecting normality

#continue with KS-test

ks.test(MSFT.ret.mat, tdata)  #not adjusted
ks.test(MSFT.ret.mat, lndata) #not adjusted
ks.test(MSFT.ret.mat, skew.norm.data) #not adjusted

#look at smoothed density
plot(density(MSFT.ret.mat)) #looks a bit skewed maybe? Try with alpha=2, i.e. right skewed or "positive"
set.seed(1312)
skew.adj<-(rsn(n=n.msft,alpha=2)+mean.msft)*sd.msft #scale simulated values

ks.test(MSFT.ret.mat,skew.adj)

plot(ecdf(MSFT.ret.mat))
lines(ecdf(skew.adj),col="red",lwd=0.5)


## SP500
n.sp500<-dim(SP500.ret.mat)[1]
sd.sp500<-sd(SP500.ret.mat)
mean.sp500<-mean(SP500.ret.mat)
set.seed(123)
gwn.sp500<-rnorm(length(SP500.ret),mean=mean(SP500.ret),sd=sd(SP500.ret))
ks.test(SP500.ret.mat,pnorm) #did not specify mean or variance
ks.test(SP500.ret.mat, gwn.sp500) #no rejection

lillie.test(MSFT.ret.mat) #rejecting normality

#continue with KS-test

ks.test(MSFT.ret.mat, tdata)  #not adjusted
ks.test(MSFT.ret.mat, lndata) #not adjusted
ks.test(MSFT.ret.mat, skew.norm.data) #not adjusted

#look at smoothed density
plot(density(SP500.ret.mat)) #try skewed again
set.seed(1312)
skew.adj<-(rsn(n=n.sp500,alpha=2)+mean.sp500)*sd.sp500 #scale simulated values

ks.test(MSFT.ret.mat,skew.adj)

plot(density(MSFT.ret.mat))
lines(density(skew.adj),col="red",lwd=0.5)

###################################################

#Additional Material:

# 
# 11. outliers
#

# create GWN return data polluted by outlier
gwn.new = gwn
gwn.new[20] = -0.9
par(mfrow=c(2,1))
	ts.plot(gwn.new,main="", lwd=2, col="blue", ylab="")
	abline(h=0)
	hist(gwn.new, main="", col="slateblue1", 
       xlab="GWN with outlier")
par(mfrow=c(1,1))
# compare summary statistic for MSFT returns and returns polluted by outlier
tmp = cbind(gwn, gwn.new)
apply(tmp, 2, mean)
apply(tmp, 2, var)
apply(tmp, 2, sd)
apply(tmp, 2, skewness) #now left tail is "longer", negative skew
apply(tmp, 2, kurtosis) #now distribution is more spread out, higher kurtosis

# outlier robust measures
apply(tmp, 2, median)
apply(tmp, 2, IQR)

#
# 12. boxplots
#
# use boxplot() function
?boxplot
args(boxplot)
boxplot(MSFT.ret.mat,outchar=T,main="Boxplot of monthly cc returns on Microsoft",
        ylab="monthly cc return", col="slateblue1")
boxplot(SP500.ret.mat,outchar=T,main="Boxplot of monthly cc returns on SP500",
        ylab="monthly cc return")
boxplot(gwn,MSFT.ret.mat,SP500.ret.mat,names=c("gwn","MSFT","SP500"),outchar=T, col="slateblue1",
        main="Comparison of return distributions", ylab="monthly cc return")


#
# 13. graphically summarize data
#

par(mfrow=c(2,2))
	hist(MSFT.ret.mat,main="MSFT monthly cc returns",
	     probability=T, ylab="cc return", col="slateblue1")
	boxplot(MSFT.ret.mat,outchar=T, ylab="cc return", col="slateblue1")
	plot(density(MSFT.ret.mat),type="l",xlab="cc return", col="slateblue1", lwd=2,
	     ylab="density estimate", main="Smoothed density")
	qqnorm(MSFT.ret.mat)
	qqline(MSFT.ret.mat)
par(mfrow=c(1,1))

#
# 14. bivariate graphical summaries
#

# bivariate scatterplot
plot(MSFT.ret.mat,SP500.ret.mat,main="Monthly cc returns on MSFT and SP500", 
     lwd=2, pch=16, cex=1.5, col="blue")
abline(h=mean(SP500.ret.mat))	# horizontal line at SP500 mean
abline(v=mean(MSFT.ret.mat))	# vertical line at MSFT mean


# reverse order of variables
plot(SP500.ret.mat,MSFT.ret.mat, 
     main="Monthly cc returns on MSFT and SP500", 
     xlab="S&P500 returns", ylab="MSFT returns",
     lwd=2, pch=16, cex=1.25, col="blue")
abline(v=mean(SP500.ret.mat))  
abline(h=mean(MSFT.ret.mat))	

# all pairwise scatterplots
pairs(cbind(gwn,MSFT.ret.mat,SP500.ret.mat), col="blue",
      pch=16, cex=1.5, cex.axis=1.5)

cov(SP500.ret.mat, MSFT.ret.mat)
cor(SP500.ret.mat, MSFT.ret.mat)

# compute covariance and correlation matrix
var(cbind(gwn,MSFT.ret.mat,SP500.ret.mat))
cor(cbind(gwn,MSFT.ret.mat,SP500.ret.mat))

#
# 15. Time series descriptive statistics
#

# sample autocovariances and autocorrelations

?acf
args(acf)
# autocorrelations for simulated gaussian data and SP500

acf(SP500.ret.mat, lwd=2)
acf(MSFT.ret.mat, lwd=2)






