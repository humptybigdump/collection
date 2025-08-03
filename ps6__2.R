rm(list=ls(all=TRUE))
setwd("C:/Users/Schienle/Desktop/R") # setting working direcctory
getwd() # checking working directory

install.packages(c("tseries", "zoo", "fGarch","timeDate")) # install needed packages
library(tseries)
library(zoo)
library(fGarch)
library(timeDate)


Y = read.csv("bmw_eur_daily.csv")
head(Y)
class(Y)

td = timeSequence(as.Date("2000/1/3"), as.Date("2014/4/3"), "days") # time series as numerical object including all calender days including weekends and bank holidays
# holidays in the period
years.included <- unique( as.integer( format( x=td, format="%Y" ) ) );
holidays <- holidayLONDON(years.included)  #  (others: holidayNERC, holidayNYSE, holidayTSX & holidayZURICH)
# Subset business days
bd <- td[isBizday(td, holidays)]

price.z = zoo(x=Y$Adj.close, order.by=bd)		# create zoo object of prices 
X = diff(log(price.z)) 					# compute cc returns
X.mat = coredata(X)					# for non-ts-calculations
plot(X)


Box.test(X, lag = 1, type = c("Box-Pierce", "Ljung-Box"))
par(mfrow=c(2,1))
acf(X.mat) 							
pacf(X.mat)
par(mfrow=c(1,1))

adf.test(X) # augmented dickey fuller test: H0 unit root
kpss.test(X)# KPSS test: H0 stationarity

# model fit in mean
fit1 = arima(X.mat, order = c(1, 0, 1)) # ARMA
fit2 = auto.arima(X.mat) # with forecast package

fit1$aic # in-sample fit
fit2$aic # in-sample fit

resid1 = fit1$residuals
resid2 = fit2$residuals

par(mfrow=c(2,1))
acf(resid1)
pacf(resid1)

acf(resid2)
pacf(resid2)

acf(resid1^2)
pacf(resid1^2)

acf(resid2^2)
pacf(resid2^2)

par(mfrow=c(1,1))

# squared returns
Box.test((X.mat)^2, lag = 1, type = c("Box-Pierce", "Ljung-Box"))
par(mfrow=c(2,1))
acf((X.mat)^2) 							
pacf((X.mat)^2)
par(mfrow=c(1,1))

# GARCH fit
garchfit1 = garchFit(~arma(1,1)+garch(1,1),data=X.mat,cond.dist="norm")
summary(garchfit1)

# check if standardized residuals r- \hat{mu)/\hat{sigma} are iid 

res1= residuals(garchfit1,standardize = TRUE)

Box.test(res1, lag = 1, type = c("Box-Pierce", "Ljung-Box"))

par(mfrow=c(2,1))
acf(res1) 							
pacf(res1)
acf(res1^2) 							
pacf(res1^2)
par(mfrow=c(1,1))

qqnorm(res1)
qqline(res1)

#compare to only ARMA fit:
qqnorm(resid1)
qqline(resid1)
jarque.bera.test(res1)
jarque.bera.test(resid1)

# compare to t-residuals
qqplot(res1, rt(300,df=4))
qqline(rt(300,df=4))
# thus fit
garchfit2 = garchFit(~arma(1,1)+garch(1,1),data=X.mat,cond.dist="std")
summary(garchfit2)

res2= residuals(garchfit2,standardize = TRUE)

Box.test(res2, lag = 1, type = c("Box-Pierce", "Ljung-Box"))

par(mfrow=c(2,1))
acf(res2) 							
pacf(res2)
acf(res2^2) 							
pacf(res2^2)
par(mfrow=c(1,1))

qqplot(res2, rt(300,df=4))
qqline(rt(300,df=4))


#########################################################

### simulations

# ARCH(2) - use default omega (=1e-6) and specify alpha, set beta=0!
spec = garchSpec(model = list(alpha = c(0.2, 0.4), beta = 0))
S1=garchSim(spec, n = 100)

# AR(1)-ARCH(2) - use default mu (=0), omega
spec = garchSpec(model = list(ar = 0.5, alpha = c(0.3, 0.4), beta = 0))
S2=garchSim(spec, n = 100)

# AR([1,5])-GARCH(1,1) - use default garch values (alpha=0.1, beta= 0.8) and subset ar[.]
spec = garchSpec(model = list(mu = 0.001, ar = c(0.5,0,0,0,0.1)))
S3=garchSim(spec, n = 100)

# ARMA(1,2)-GARCH(1,1) - use default garch values
spec = garchSpec(model = list(ar = 0.5, ma = c(0.3, -0.3)))
S4=garchSim(spec, n = 100)

par(mfrow=c(2,4))

acf(S1)
acf(S2)
acf(S3)
acf(S4)

pacf(S1)
pacf(S2)
pacf(S3)
pacf(S4)

acf(S1^2)
acf((arima$residuals(S2, order=c(1,0,0)))^2)
acf((S3-arima(S3, order=c(5,0,0)))^2)
acf((S4-arima(S4, order=c(1,0,2)))^2)

pacf(S1^2)
pacf(S2^2)
pacf(S3^2)
pacf(S4^2)

