##Financial Econometrics Exercise 
##PS4

# Problem 6
##############################################


# base R functions used
#
# abline		    draw horizontal line
# cumsum		    compute cumulative sums
# for			      start for loop
# layout		    partition screen into parts
# par			      set graphics defaults
# plot		      create default xy-plot
# rep			      repeat a sequence of values
# rnorm		      generate iid normal random variables
# set.seed		  set random number seed
# ts.plot		    create time series plot
#
# R package functions used
#
# tsdiag
# arima.sim		  simulate from ARIMA model
# arima
# acf/pacf
# ar
# predict       forecast
# Box.test
#
# ARMAacf		    true ACF for ARMA model
#

options(digits=4)
set.seed(143)

if(!require(tseries)){
    install.packages("tseries")   
}
if(!require(PerformanceAnalytics)){
    install.packages("PerformanceAnalytics"); library(PerformanceAnalytics)   
}

##############################################
##i
#Simulate processes
###############################################

sim.ar1<-arima.sim(list(ar=c(0.2)),n=1000)
sim.ar2<-arima.sim(list(ar=c(0.9)),n=1000)

sim.ma1<-arima.sim(list(ma=c(0.1)),n=1000)
sim.ma2<-arima.sim(list(ma=c(0.7)),n=1000)


# ts-plots

par(mfrow=c(2,2))
ts.plot(sim.ar1,main ="AR(1) with phi=0.2", ylim=c(-5,5))
abline(h=0)
ts.plot(sim.ar2,main ="AR(1) with phi=0.9", ylim=c(-5,5))
abline(h=0)
ts.plot(sim.ma1,main ="MA(1) with theta=0.1", ylim=c(-5,5))
abline(h=0)
ts.plot(sim.ma2,main ="MA(1) with theta=0.7", ylim=c(-5,5))
abline(h=0)

##############################################
##ii
#Plot ACF and PACF
###############################################

# corresponding ACFs und PACFS

par(mfrow=c(2,4))
acf(sim.ar1,main="ACF of AR(1) with phi=0.2")
acf(sim.ma1,main="ACF of MA(1) with theta=0.1")
acf(sim.ar2,main="ACF of AR(1) with phi=0.9")
acf(sim.ma2,main="ACF of MA(1) with theta=0.7")

pacf(sim.ar1,main="PACF of AR(1) with phi=0.2")
pacf(sim.ma1,main="PACF of MA(1) with theta=0.1")
pacf(sim.ar2,main="PACF of AR(1) with phi=0.9")
pacf(sim.ma2,main="PACF of MA(1) with theta=0.7")

par(mfrow=c(1,1))
chart.ACFplus(sim.ar1) # puts ACFs und PACFs on the same scale and starts both at 0 but prevents a title

# or with self-written ACF2 
#acf2(sim.ar1) 

##############################################
##iii
#Repeat analysis with negative coefficients
###############################################

sim.ar3<-arima.sim(list(ar=c(-0.2)),n=1000)
sim.ma3<-arima.sim(list(ma=c(-0.1)),n=1000)

sim.ar4<-arima.sim(list(ar=c(-0.9)),n=1000)
sim.ma4<-arima.sim(list(ma=c(-0.7)),n=1000)

# ts-plots

par(mfrow=c(2,2))
ts.plot(sim.ar3,main ="AR(1) with phi= -0.2", ylim=c(-5,5))
abline(h=0)
ts.plot(sim.ar4,main ="AR(1) with phi= -0.9", ylim=c(-5,5))
abline(h=0)
ts.plot(sim.ma3,main ="MA(1) with theta= -0.1", ylim=c(-5,5))
abline(h=0)
ts.plot(sim.ma4,main ="MA(1) with theta= -0.7", ylim=c(-5,5))
abline(h=0)


# corresponding ACFs und PACFS

par(mfrow=c(2,4)) 
acf(sim.ar3,main="ACF of AR(1) with phi= -0.2")
acf(sim.ma3,main="ACF of MA(1) with theta= -0.1")
acf(sim.ar4,main="ACF of AR(1) with phi= -0.9")
acf(sim.ma4,main="ACF of MA(1) with theta= -0.7")

pacf(sim.ar3,main="PACF of AR(1) with phi= -0.2")
pacf(sim.ma3,main="PACF of MA(1) with theta= -0.1")
pacf(sim.ar4,main="PACF of AR(1) with phi= -0.9")
pacf(sim.ma4,main="PACF of MA(1) with theta= -0.7")


##############################################
##iv
#Compare with AR(2) and ARMA(2,2)
###############################################


sim.ar5<-arima.sim(list(ar=c(0.9,-0.2)),n=1000)
sim.arma1<-arima.sim(list(ar=c(0.9,-0.2), ma=c(0.7,-0.1)),n=1000)
par(mfrow=c(2,1))
ts.plot(sim.ar5,main ="AR(2) with phi=(0.9,-0.2)", ylim=c(-5,5))
abline(h=0)
ts.plot(sim.ar4,main ="ARMA(2,2)", ylim=c(-5,5))
abline(h=0)


par(mfrow=c(2,2))
acf.ar5<- acf(sim.ar5,main="ACF of AR(2) process")
acf.arma1<-acf(sim.arma1,main="ACF of ARMA(2,2) process")
pacf(sim.ar5,main="PACF of AR(2) process")
pacf(sim.arma1,main="PACF of ARMA(2) process")

##############################################
##v
#Plot ACF and of ARMA and AR
###############################################

par(mfrow=c(1,1))
plot(acf.ar5$acf, col=1, xlab="lags", ylab="ACF")
points(acf.arma1$acf, col=2)


##########################################################
rm(list=ls(all=TRUE))
#setwd() # setting working direcctory
getwd() # checking working directory

library(tseries)
library(zoo)

#i
##########################################################
# get monthly adjusted closing price data on BMW and DAX from Yahoo
# using the tseries function get.hist.quote. Set sample to Jan 2000 through
# May 2014. 

BMW.prices.d = get.hist.quote(instrument="bmw.de", start="2000-01-01",
                              end="2014-05-31", quote="AdjClose",
                              provider="yahoo", origin="1970-01-01",
                              compression="d", retclass="zoo")

BMW.prices.w = get.hist.quote(instrument="bmw.de", start="2000-01-01",
                              end="2014-05-31", quote="AdjClose",
                              provider="yahoo", origin="1970-01-01",
                              compression="w", retclass="zoo")

BMW.prices.m = get.hist.quote(instrument="bmw.de", start="2000-01-01",
                              end="2014-05-31", quote="AdjClose",
                              provider="yahoo", origin="1970-01-01",
                              compression="m", retclass="zoo")
class(BMW.prices.m)
colnames(BMW.prices.m)
start(BMW.prices.m)
end(BMW.prices.m)
head(BMW.prices.m)


DAX.prices = get.hist.quote(instrument="^gdaxi", start="2000-01-01",
                            end="2014-05-31", quote="AdjClose",
                            provider="yahoo", origin="1970-01-01",
                            compression="m", retclass="zoo")


# add column names 
colnames(BMW.prices.d) = "BMWd"
colnames(BMW.prices.w) = "BMWw"
colnames(BMW.prices.m) = "BMWm"
colnames(DAX.prices) = "DAX"


# compute cc returns 
BMWd.ret = diff(log(BMW.prices.d))
BMWw.ret = diff(log(BMW.prices.w))
BMWm.ret = diff(log(BMW.prices.m))
DAX.ret = diff(log(DAX.prices))
ALL.ret = merge(BMWm.ret, DAX.ret)
ALL.ret2 = merge(BMWm.ret,BMWd.ret)
colnames(ALL.ret)
head(ALL.ret, n=3)

plot(ALL.ret, plot.type="single", col=c("blue","red"), lty=1:2, lwd=2)

legend(x="topleft", legend=c("BMW","DAX"), col=c("blue","red"), lty=1:2)

##(ii): dynamic properties
# convert zoo data to matrix data for non time series analysis
# many R functions do not have methods implemented for zoo objects
# and results may be unpredictable
##########################################################

BMWd.ret.mat = coredata(BMWd.ret)
colnames(BMWd.ret.mat) = "BMWd"
rownames(BMWd.ret.mat) = as.character(index(BMWd.ret))


BMWw.ret.mat = coredata(BMWw.ret)
len_bmww<-dim(BMWw.ret.mat)[1]
BMWw.ret.mat<-as.matrix(BMWw.ret.mat[1:(len_bmww-1)])
              
colnames(BMWw.ret.mat) = "BMWw"
rownames(BMWw.ret.mat) = as.character(index(BMWw.ret))[1:(len_bmww-1)]
#BMWw.ret.mat<-as.matrix(BMWw.ret.mat[1:(dim(BMWw.ret.mat)[1]-1)])

BMWm.ret.mat = coredata(BMWm.ret)
colnames(BMWm.ret.mat) = "BMWm"
rownames(BMWm.ret.mat) = as.character(index(BMWm.ret))

DAX.ret.mat = coredata(DAX.ret)
colnames(DAX.ret.mat) = "DAX"
rownames(DAX.ret.mat) = as.character(index(DAX.ret))

Box.test(BMWd.ret.mat, lag = 1, type = c("Box-Pierce", "Ljung-Box"))
par(mfrow=c(2,1))
acf(BMWd.ret.mat, na.action = na.contiguous)
pacf(BMWd.ret.mat, na.action = na.contiguous)
#alternative option with Performance analytics
library(PerformanceAnalytics)
chart.ACFplus(BMWd.ret.mat) 
#ho:autocorrelation zero
Box.test(BMWw.ret.mat, lag = 1, type = c("Box-Pierce", "Ljung-Box"))
acf(BMWw.ret.mat)
pacf(BMWw.ret.mat)

Box.test(BMWm.ret.mat, lag = 1, type = c("Box-Pierce", "Ljung-Box"))
acf(BMWm.ret.mat)
pacf(BMWm.ret.mat)

Box.test(DAX.ret.mat, lag = 1, type = c("Box-Pierce", "Ljung-Box"))
acf(DAX.ret.mat)
pacf(DAX.ret.mat)



##(iii)
##########################################################
#require(graphics)
#take BMW as example
arima(BMWm.ret.mat,order=c(1,0,0))$aic
BIC(arima(BMWm.ret.mat,order=c(1,0,0)))

jarque.bera.test(BMWm.ret.mat)
set.seed(111)
tdata=rt(171,df=3)
ks.test(BMWm.ret.mat, tdata)#not t-distributed

par(mfrow=c(2,1))
acf(BMWm.ret.mat^2)
pacf(BMWm.ret.mat^2)

BMWfit<-arima(BMWm.ret.mat,order=c(0,0,1))
BMWfit$coef
par(mfrow=c(1,1))
ts.plot(BMWfit$residuals)
abline(h=0)
BMWres<-BMWfit$residuals[-1] # discart first observation

Box.test(BMWres,lag=1)#cannot reject H0
BMWfit$aic

par(mfrow=c(2,1))
acf(BMWres)
pacf(BMWres)
acf(BMWres^2)
pacf(BMWres^2)

par(mfrow=c(1,1))
qqnorm(BMWres) # qq plot for normal distribution
qqline(BMWres, col = "red3", lwd = 3) # adding line for the qq plot

jarque.bera.test(BMWres)#not normal

#DAX
BIC(arima(DAX.ret.mat,order=c(1,0,0)))
DAXfit<-arima(DAX.ret.mat,order=c(0,0,1))
DAXfit$coef
ts.plot(DAXfit$residuals)
DAXres<- DAXfit$residuals[-1] # discart first observation: 0-lag correlation, is always 1...

Box.test(DAXres,lag=1)
DAXfit$aic

par(mfrow=c(2,1))
acf(DAXres)
pacf(DAXres)
acf(DAXres^2)
pacf(DAXres^2)

par(mfrow=c(1,1))
qqnorm(DAXres) # qq plot for normal distribution
qqline(DAXres, col = "red3", lwd = 3) # adding line for the qq plot
jarque.bera.test(DAXres)#not normal

tsdiag(DAXfit)# summarizes main parts of the above

### automatic model selection
# simple AR 
BMWd.ar = ar(na.omit(BMWd.ret.mat), aic = TRUE, order.max = 10)
BMWw.ar = ar(BMWw.ret.mat, aic = TRUE, order.max = 10)
BMWm.ar = ar(BMWm.ret.mat, aic = TRUE, order.max = 10)
DAX.ar = ar(DAX.ret.mat, aic = TRUE, order.max = 10)

# set 
RD<- BMWm.ret.mat
#RD<- DAX.ret.mat
# compare aic and bic for different arma models
min.ic = Inf
my.model = c(0,0)
for(i in 0:3){
    for(j in 0:3){
        res = arima(RD, order = c(i, 0, j), method = "ML")
        if(res$loglik)
            aic = -2 * res$loglik + 2 * (length(res$coef) + 1) # res$aic
        bic = -2 * res$loglik + (length(res$coef) + 1) * log(length(RD))
        if(aic < min.ic){min.ic = aic; my.model = c(i, j)}
        #if(bic < min.ic){min.ic = bic; my.model = c(i, j)}
        print(paste(i, ", ", j, " / (3, 3)", ", ML = ", round(res$loglik, digits = 4), 
                    ", AIC = ", round(aic, digits = 4), 
                    ", BIC = ", round(bic, digits=4), sep = ""))
    }
}


##(IV): forecast comparison
##########################################################
# ARMA estimation
# split sample in two parts
int<- length(RD)-10 
outt<- 10

est.run = arima(RD[1:int], order = c(0, 0, 1), method = "ML") # in-sample model estimation
fore.arima = predict(est.run, n.ahead = outt) # out-of sample forecast

plot(RD, type="l")
lines(fore.arima$pred, col="red")
lines(fore.arima$pred+2*fore.arima$se,col="red",lty=3)
lines(fore.arima$pred-2*fore.arima$se,col="red",lty=3)

# RW estimation
e = rnorm(outt,sd=0.1)
naive = RD[int]+ cumsum(e) # naive benchmark
lines(163:172,naive,col="green")


##(v): forecast evaluation
future= RD[(int+1): length(RD)]
# Root Mean Square Forecast Error (RMSFE)
sqrt(mean((fore.arima$pred - future)^2))
sqrt(mean((naive - future)^2))
# Median Absolute Forecast Error (MDAFE)
median(abs(fore.arima$pred - future))
median(abs(naive - future))
# Mean Absolute Forecast Error (MAFE)
mean(abs(fore.arima$pred - future))
mean(abs(naive - future))

