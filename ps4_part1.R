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
  install.packages("PerformanceAnalytics")   
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
