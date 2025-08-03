# PROBLEM SET 6

# Problem 11

# --------
# Set Working Directory, Load Packages
# --------
rm(list=ls(all=TRUE))

file_path = rstudioapi::getActiveDocumentContext()$path # get file path
setwd(dirname(file_path))

# install.packages(c("tseries", "zoo", "fGarch","timeDate", "forecast")) # install needed packages

library(tseries)
library(zoo)
library(fGarch)
library(timeDate)
library(forecast)

# --------
# Step 1: Prepare Data
# --------
# read-in the data
Y = read.csv("bmw_eur_daily.csv")
head(Y)
class(Y)

td = timeSequence(as.Date("2000/1/3"), as.Date("2014/4/3"), "days") # time series as numerical object including all calender days including weekends and bank holidays

# holidays in the period
years.included <- unique(as.integer(format(x = td, format = "%Y")))
holidays <- holidayLONDON(years.included)  #  (others: holidayNERC, holidayNYSE, holidayTSX & holidayZURICH)

# Subset business days
bd <- td[isBizday(td, holidays)]

price.z <- zoo(x = Y$Adj.close, order.by = bd) # create zoo object of prices
X <- diff(log(price.z)) # compute cc returns (lag = 1, log-return)
X.mat <- coredata(X) # for non-ts-calculations

par(mfrow = c(2, 1))
plot(X, type = "l")
plot(price.z, type = "l")

# --------
# Step 2: Check for Autocorrelation Behaviour
# --------
# H0: data is uncorrelated
Box.test(X, lag = 1, type = c("Ljung-Box"))
Box.test(X, lag = 1, type = c("Box-Pierce"))

# plot autocorrelation and partial autocorrelation function
acf(X.mat, xlim = c(0, 35))
pacf(X.mat, xlim = c(0, 35))

adf.test(X) # augmented dickey fuller test: H0 unit root, H1: stationary time series
kpss.test(X) # KPSS test: H0 stationarity

# --------
# Step 3: Try Simple TS-Models
# --------
fit1 <- arima(X.mat, order = c(1, 0, 1)) # ARMA(1,1)
fit2 <- auto.arima(X.mat) # with forecast package -> ARMA(1,3) here

summary(fit1)
summary(fit2)

fit1$aic # in-sample fit
fit2$aic # in-sample fit

resid1 <- fit1$residuals
resid2 <- fit2$residuals

# plot residuals, acf and pacf of residuals
plot(resid1, type = "l")
plot(resid2, type = "l")

acf(resid1, xlim = c(0, 35))
pacf(resid1, xlim = c(0, 35))

acf(resid2, xlim = c(0, 35))
pacf(resid2, xlim = c(0, 35))

# plot squared resiudals and their acf and pacf
plot(resid1^2, type = "l")
plot(resid2^2, type = "l")

acf(resid1^2)
pacf(resid1^2)

acf(resid2^2)
pacf(resid2^2)

### SQUARED RETURNS PLOTTEN

# squared returns
Box.test(X.mat^2, lag = 1, type = c("Ljung-Box")) # autocorrelation in squared returns!
Box.test(X.mat^2, lag = 1, type = c("Box-Pierce"))

par(mfrow = c(2, 1))
acf(X.mat^2)
pacf(X.mat^2)
par(mfrow = c(1, 1))

# --------
# Step 4: Fit ARMA-GARCH model
# --------
garchfit1 <- garchFit( ~ arma(1, 1) + garch(1, 1), data = X.mat, cond.dist = "norm")
summary(garchfit1)

res1 <- residuals(garchfit1, standardize = TRUE)

Box.test(res1, lag = 1, type = c("Ljung-Box"))
Box.test(res1, lag = 1, type = c("Box-Pierce"))

acf(res1) 							
pacf(res1)

acf(res1^2) 							
pacf(res1^2)

qqnorm(res1)
qqline(res1)

# compare to only ARMA fit -> they have fatter tails
# (volatility clusters not considered)
qqnorm(resid1)
qqline(resid1)

jarque.bera.test(res1)
jarque.bera.test(resid1)

# compare to t-residuals
qqplot(res1, rt(300, df = 4))
qqline(rt(300, df = 4))

# thus fit
garchfit2 <- garchFit( ~ arma(1, 1) + garch(1, 1), data = X.mat, cond.dist = "std")
summary(garchfit2)

res2 <- residuals(garchfit2, standardize = TRUE)

Box.test(res2, lag = 1, type = c("Ljung-Box"))
Box.test(res2, lag = 1, type = c("Box-Pierce"))

acf(res2) 							
pacf(res2)
acf(res2^2) 							
pacf(res2^2)

# par(mfrow=c(1,1))
qqplot(res2, rt(300,df=4))
qqline(rt(300,df=4))