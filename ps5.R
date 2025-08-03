rm(list=ls(all=TRUE))
setwd("C:/Users/thisisme/Desktop/R") # setting working direcctory
getwd() # checking working directory

install.packages(c("tseries")) # install needed packages
library(tseries)


Y = read.csv("exrates.csv")
head(Y)
X<- Y[,2:6] # only data, no time index
nob<- length(X[,1])
RD <- log(X[2:nob,])-log(X[1:(nob-1), ])# log returns

# Canadian Dollar - first column:

adf.test(RD[,1]) # augmented dickey fuller test: H0 unit root
kpss.test(RD[,1])# KPSS test: H0 stationarity
Box.test(RD[ ,1], lag = 1, type = c("Box-Pierce", "Ljung-Box")) # Portmonteau (Box-Pierce) or Ljung-Box test

pacf(RD[,1]) # partial autocorrelation function
acf(RD[,1])  # autocorrelation function

# model fit 
fit1 = arima(RD[,1], order = c(1, 0, 1)) # ARMA
fit2 = arima(RD[,1], order = c(1, 1, 1)) # ARIMA

# evaluate fit

fit1$aic # in-sample fit
fit2$aic 

resid1 = fit1$residuals[-1]
resid2 = fit2$residuals[-1]

##  ACFs
acf(resid1)
pacf(resid1)

acf(resid2)
pacf(resid2)


#### forecast comparison
# split sample in two parts
int<- length(RD[,1])-10 
outt<- 10

est.run1 = arima(RD[1:int,1], order = c(1, 0, 1), method = "ML") # in-sample model estimation
est.run2 = arima(RD[1:int,1], order = c(1, 1, 1), method = "ML") # in-sample model estimation
fore1.arima = predict(est.run1, n.ahead = outt) # out-of sample forecast
fore2.arima = predict(est.run2, n.ahead = outt) # out-of sample forecast

future= RD[(int+1): length(RD[,1]),1]

# RMSE
sqrt(mean((fore1.arima$pred - future)^2))
sqrt(mean((fore2.arima$pred - future)^2))

##############################################################
# automated forecast evaluation with package forecast

install.packages("forecast")
library(forecast)

est.run3 = auto.arima(RD[1:int,1]) # in-sample automated model fit
est.run3
fore3.arima = predict(est.run3, n.ahead = outt) # out-of sample forecast

accuracy(est.run1) # in-sample one-step ahead forecast accuracy - summary
accuracy(est.run2)
accuracy(est.run3)

accuracy(fore1.arima$pred, future) # out-of-sample one-step ahead forecast accuracy - summary
accuracy(fore2.arima$pred, future)
accuracy(fore3.arima$pred, future)

## Diebold Mariano Test for out of sample fit, H0 is models are equal
eA = fore1.arima$pred - future
eB = fore2.arima$pred - future
eC = fore3.arima$pred - future

dm.test(eA, eB, power = 1)
dm.test(eA, eC, power = 1)
###################################################################

# Variance ratio tests (all for Canadian Dollar)

install.packages("vrtest")
library(vrtest)

kvec <- c(2,5,10)
Lo.Mac(RD[,1], kvec) # VR statistics for different holding times (M1 iid, M2 heteroskedasticity corrected)
Boot.test(RD[,1],kvec,nboot=500,wild="Normal")
VR.plot(RD[,1],kvec)

#Auto.VR(RD[,1]) # A variance ratio test with holding period value chosen by a data dependent procedure

