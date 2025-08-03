# = = = = = = = = = = = = = = = = = = = = = = = = = #
#                                                   #
#         Financial Econometrics 1                  #
#                   Problem Set 4                   #
#                                                   #
# = = = = = = = = = = = = = = = = = = = = = = = = = #

# = = = = = = = = = = = = = = = = = = = = = = = = = =
# Problem 6 ####
# = = = = = = = = = = = = = = = = = = = = = = = = = =

# base R functions used

# abline        draw horizontal line
# cumsum        compute cumulative sums
# for           start for loop
# layout        partition screen into parts
# par           set graphics defaults
# plot          create default xy-plot
# rep           repeat a sequence of values
# rnorm         generate iid normal random variables
# set.seed      set random number seed
# ts.plot       create time series plot

# R package functions used

# tsdiag
# arima.sim     simulate from ARIMA model
# arima
# acf/pacf
# ar
# predict       forecast
# Box.test

# ARMAacf       true ACF for ARMA model

# = = = = = = = = = = = = = = = = = = = = = = = = = =

rm(list=ls(all=TRUE))
options(digits=4)
set.seed(143)

if(!require(tseries)){
    install.packages("tseries")   
}
if(!require(PerformanceAnalytics)){
    install.packages("PerformanceAnalytics")
}

# = = = = = = = = = = = = = = = = = = = = = = = = = =
## i) simulate processes ####
# = = = = = = = = = = = = = = = = = = = = = = = = = =

n_samples <- 1000

sim.ar1<-arima.sim(list(ar=c(0.2), order=c(1,0,0)), n=n_samples); sim.ar1
sim.ar2<-arima.sim(list(ar=c(0.9)), n=n_samples)

sim.ma1<-arima.sim(list(ma=c(0.1)), n=n_samples)
sim.ma2<-arima.sim(list(ma=c(0.7)), n=n_samples)

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

# = = = = = = = = = = = = = = = = = = = = = = = = = =
## ii) plot ACF and PACF ####
# = = = = = = = = = = = = = = = = = = = = = = = = = =

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

# = = = = = = = = = = = = = = = = = = = = = = = = = =
## iii) negative coefficients ####
# = = = = = = = = = = = = = = = = = = = = = = = = = =#

sim.ar3<-arima.sim(list(ar=c(-0.2)),n=n_samples)
sim.ma3<-arima.sim(list(ma=c(-0.1)),n=n_samples)

sim.ar4<-arima.sim(list(ar=c(-0.9)),n=n_samples)
sim.ma4<-arima.sim(list(ma=c(-0.7)),n=n_samples)

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

# = = = = = = = = = = = = = = = = = = = = = = = = = =
## iv) compare AR(2), ARMA(2,2) ####
# = = = = = = = = = = = = = = = = = = = = = = = = = =#

sim.ar5     <- arima.sim(list(ar=c(0.9,-0.2)),n=n_samples)
sim.arma1   <- arima.sim(list(ar=c(0.9,-0.2), ma=c(0.7,-0.1)),n=n_samples)

par(mfrow=c(2,1))
ts.plot(sim.ar5,main ="AR(2) with phi=(0.9,-0.2)", ylim=c(-5,5))
abline(h=0)
ts.plot(sim.arma1,main ="ARMA(2,2) with phi=(0.9,-0.2) and theta=(0.7,-0.1)", ylim=c(-5,5))
abline(h=0)

par(mfrow=c(2,2))
acf.ar5     <- acf(sim.ar5, main="ACF of AR(2) process")
acf.arma1   <- acf(sim.arma1, main="ACF of ARMA(2,2) process")
pacf(sim.ar5,main="PACF of AR(2) process")
pacf(sim.arma1,main="PACF of ARMA(2) process")

# = = = = = = = = = = = = = = = = = = = = = = = = = =
## v) plot ACF in one plot####
# = = = = = = = = = = = = = = = = = = = = = = = = = =

par(mfrow=c(1,1))
plot(acf.ar5$acf, col=1, xlab="lags", ylab="ACF", pch=1)
points(acf.arma1$acf, col=2, pch=2)
legend("topright", legend=c("AR(2)", "ARMA(2,2)"), col=c(1, 2), pch=c(1, 2))

# = = = = = = = = = = = = = = = = = = = = = = = = = =
# Problem 7 ####
# Box-Jenkins analysis of financial returns
# = = = = = = = = = = = = = = = = = = = = = = = = = =

rm(list=ls(all=TRUE))
library(tseries)
library(zoo)

# = = = = = = = = = = = = = = = = = = = = = = = = = =
## i) get data ####
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

# = = = = = = = = = = = = = = = = = = = = = = = = = =
## iii) dynamic properties ####
# = = = = = = = = = = = = = = = = = = = = = = = = = =
# convert zoo data to matrix data for non time series analysis
# many R functions do not have methods implemented for zoo objects
# and results may be unpredictable

BMWd.ret.mat = coredata(BMWd.ret)
colnames(BMWd.ret.mat) = "BMWd"
rownames(BMWd.ret.mat) = as.character(index(BMWd.ret))

BMWw.ret.mat = coredata(BMWw.ret)
len_bmww<-dim(BMWw.ret.mat)[1]
BMWw.ret.mat<-as.matrix(BMWw.ret.mat[1:(len_bmww-1)])
              
colnames(BMWw.ret.mat) = "BMWw"
rownames(BMWw.ret.mat) = as.character(index(BMWw.ret))[1:(len_bmww-1)]

BMWm.ret.mat = coredata(BMWm.ret)
colnames(BMWm.ret.mat) = "BMWm"
rownames(BMWm.ret.mat) = as.character(index(BMWm.ret))

DAX.ret.mat = coredata(DAX.ret)
colnames(DAX.ret.mat) = "DAX"
rownames(DAX.ret.mat) = as.character(index(DAX.ret))

## = = = = = = = = = = = = 

par(mfrow=c(2,1))

Box.test(BMWd.ret.mat, lag = 1, type = c("Box-Pierce", "Ljung-Box"))
acf(BMWd.ret.mat, na.action = na.contiguous)
pacf(BMWd.ret.mat, na.action = na.contiguous)

Box.test(BMWw.ret.mat, lag = 1, type = c("Box-Pierce", "Ljung-Box"))
acf(BMWw.ret.mat)
pacf(BMWw.ret.mat)

Box.test(BMWm.ret.mat, lag = 1, type = c("Box-Pierce", "Ljung-Box"))
acf(BMWm.ret.mat)
pacf(BMWm.ret.mat)

Box.test(DAX.ret.mat, lag = 1, type = c("Box-Pierce", "Ljung-Box"))
acf(DAX.ret.mat)
pacf(DAX.ret.mat)

# = = = = = = = = = = = = = = = = = = = = = = = = = =
## iv) AIC ####
# take BMW as example
# = = = = = = = = = = = = = = = = = = = = = = = = = =

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
BMWres<-BMWfit$residuals[-1] # discard first observation

Box.test(BMWres,lag=1) #cannot reject H0
BMWfit$aic

par(mfrow=c(2,1))
acf(BMWres)
pacf(BMWres)
acf(BMWres^2)
pacf(BMWres^2)

par(mfrow=c(1,1))
qqnorm(BMWres) # qq plot for normal distribution
qqline(BMWres, col = "red3", lwd = 3) # adding line for the qq plot

jarque.bera.test(BMWres) # not normal

# = = = = = = = = = = = = = = = = = = = = = = = = = =
# DAX

BIC(arima(DAX.ret.mat,order=c(1,0,0)))
DAXfit<-arima(DAX.ret.mat,order=c(0,0,1))
DAXfit$coef
ts.plot(DAXfit$residuals)
DAXres<- DAXfit$residuals[-1] 
# discard first observation: 0-lag correlation, is always 1...

Box.test(DAXres,lag=1)
DAXfit$aic

par(mfrow=c(2,1))
acf(DAXres)
pacf(DAXres)
acf(DAXres^2) # ARCH GARCH Model class
pacf(DAXres^2)

par(mfrow=c(1,1))
qqnorm(DAXres) # qq plot for normal distribution
qqline(DAXres, col = "red3", lwd = 3) # adding line for the qq plot
jarque.bera.test(DAXres) # not normal

tsdiag(DAXfit) # summarizes main parts of the above

# automatic model selection
# simple AR 
BMWd.ar = ar(na.omit(BMWd.ret.mat), aic = TRUE, order.max = 10)
BMWw.ar = ar(BMWw.ret.mat, aic = TRUE, order.max = 10)
BMWm.ar = ar(BMWm.ret.mat, aic = TRUE, order.max = 10)
DAX.ar = ar(DAX.ret.mat, aic = TRUE, order.max = 10)

# set 
RD <- BMWm.ret.mat
RD <- DAX.ret.mat

# compare aic and bic for different arma models
min.ic = Inf
my.model = c(0,0)

ar_max <- 10
ma_max <- 10

for(i in 0:ar_max){
    for(j in 0:ma_max){
        res = arima(RD, order = c(i, 0, j), method = "ML")
        if(res$loglik)
            aic = -2 * res$loglik + 2 * (length(res$coef) + 1) # res$aic
        bic = -2 * res$loglik + (length(res$coef) + 1) * log(length(RD))
        if(aic < min.ic){min.ic = aic; my.model = c(i, j)}
        # if(bic < min.ic){min.ic = bic; my.model = c(i, j)}
        print(paste(i, ", ", j,
                    ", loglik = ",  sprintf("%.4f", round(res$loglik, digits=4)), 
                    ", AIC = ",     sprintf("%.4f", round(aic, digits=4)), 
                    ", BIC = ",     sprintf("%.4f", round(bic, digits=4)), 
                    sep = ""))
    }
}

my.model

# = = = = = = = = = = = = = = = = = = = = = = = = = =
## v) forecast comparison ####
# ARMA estimation
# split sample in two parts
# = = = = = = = = = = = = = = = = = = = = = = = = = =

n_total <- length(RD)
n_test  <- 10
n_train <- n_total-n_test

# in-sample model estimation
est.run = arima(RD[1:n_train], order = c(20,0,20), method="ML")
coef(est.run)

# direct out-of sample forecast
fore.arima = predict(est.run, n.ahead=n_test) 

plot(RD, type="l")
lines(fore.arima$pred, col="red")
lines(fore.arima$pred+2*fore.arima$se,col="red",lty=3)
lines(fore.arima$pred-2*fore.arima$se,col="red",lty=3)

# Plotting in-sample fit
fitted <- RD[1:n_train] - coredata(residuals(est.run))
lines((1:n_train), fitted, col = "blue", lty = 1, lw = 2)

# RW estimation
e = rnorm(n_test, sd=0.1)
naive = RD[n_train]+ cumsum(e) # naive benchmark
lines(163:172,naive,col="green")

# = = = = = = = = = = = = = = = = = = = = = = = = = =

# Rolling forecast

# Create empty vectors to store forecast values
forecast_values <- numeric(n_test)

for (i in 1:n_test) {
    est.run <- arima(RD[1:(n_train+i)], order=c(3, 0, 3), method = "ML")
    cat("Coefficients at time", n_train+i-1, ":", coef(est.run), "\n")
    forecast_values[i] <- predict(est.run, n.ahead=1)$pred
}

plot(RD, type="l")
lines(seq(n_train+1, n_total), forecast_values, col="red")

# = = = = = = = = = = = = = = = = = = = = = = = = = =
## vi) forecast evaluation ####
# = = = = = = = = = = = = = = = = = = = = = = = = = =
future= RD[(n_train+1): n_total]

# Assuming you have the results stored in variables
rmsfe_values <- c(sqrt(mean((fore.arima$pred - future)^2)),
                  sqrt(mean((naive - future)^2)),
                  sqrt(mean((0 - future)^2)))

mdafe_values <- c(median(abs(fore.arima$pred - future)),
                  median(abs(naive - future)),
                  median(abs(0 - future)))

mafe_values <- c(mean(abs(fore.arima$pred - future)),
                 mean(abs(naive - future)),
                 mean(abs(0 - future)))

# Creating a dataframe
results_df <- data.frame(Method = c("ARIMA", "Naive", "Zero"),
                         RMSFE = round(rmsfe_values, 3),
                         MDAFE = round(mdafe_values, 3),
                         MAFE =  round(mafe_values, 3))

results_df
