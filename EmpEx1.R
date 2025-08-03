#Financial Econometrics II Empirical Exercises
rm(list=ls())
#Problem 1: ARCH Processes in R

#install and load important packages

#install.packages("rugarch")
#install.packages("forecast")
#install.packages("tseries")
#install.packages("zoo")
library(forecast)
library(tseries)
library(rugarch)
library(zoo)


############
#i

#Download DAX data from January 2006 to November 2020

#dax price data as zoo object from tseries package
dax.raw <- get.hist.quote(instrument = "^GDAXI",start=as.Date("2006-01-01"),end=as.Date("2020-11-01"),
                          quote="Adjusted")

#nice to plot
plot(dax.raw)

#get cc returns

dax.ret<- diff(log(dax.raw))
summary(dax.ret)
#we see varying variance
plot(dax.ret)

#but when? Covid 19 and Financial Crisis
par(mfrow=c(2,1))
plot(dax.raw)
plot(dax.ret)
par(mfrow=c(1,1))
#graphics.off() #works as well and resets graphics state

#lets see when the most extreme returns occured
#for classic data manipulation,
#it might be easier to transform data into data.frame

#from zoo package
dax.ret.mat<-data.frame(cc_ret=coredata(dax.ret),dates=index(dax.ret))
row.names(dax.ret.mat)<-dax.ret.mat[,2]

#when do we have missing values?
dax.ret.mat[is.na(dax.ret.mat),]
dax.ret.mat<-dax.ret.mat[-which(is.na(dax.ret.mat)),]

#lets look at most extreme points and when they occured
quant<-quantile(dax.ret.mat[,1],c(0.005,0.995),na.rm=TRUE)
dax.ret.mat[dax.ret.mat[,1]>quant[2]|dax.ret.mat[,1]<quant[1],]

#look at squared returns as well
mean(dax.ret.mat[,1]) #almost zero
#they depict variance in this mean zero model
plot(dax.ret.mat[,2],dax.ret.mat[,1]^2,type="l")
#change
############
#ii

acf(dax.ret.mat[,1]) #not very informative
pacf(dax.ret.mat[,1]) #not very informative

#we have to look at squared residuals

#assuming ARMA(0,0) model:
pacf(dax.ret.mat[,1]^2)
#mean correction doesnt change much
pacf((dax.ret.mat[,1]-mean(dax.ret.mat[,1]))^2)

#what if we use ARMA(1,1) model for mean
arma_1.1<-arima(dax.ret.mat[,1],order=c(1,0,1))
resids_1.1<-arma_1.1$residuals
plot(resids_1.1^2)
pacf(resids_1.1^2) #seems not to matter much, why?
coef(arma_1.1) #very small coefficients

#a lot of significant lags, so try out large models,too

acf(resids_1.1^2)
#seems to have an ma component, too --> So maybe GARCH model would be informative

############
#iii

#look at garch specs first

ugarchspec() #should have ARMA(1,1) and normal dist
?ugarchfit
lags<-1:15

model_stats<-list()

for(i in lags)
{
  spec_garch<-ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(i,0)))
  tryCatch({
    temp<-ugarchfit(spec_garch,dax.ret.mat[,1])
    model_stats[[i]]<-list(model=temp,order=i,AIC=infocriteria(temp)[1],BIC=infocriteria(temp)[2])
  },error=function(e){model_stats[[i]]<<-NULL})
  
  print(paste0("Finished with Model ",i))
}

wo_model<-t(sapply(lags,function(x) {if(is.null(model_stats[[x]])){c(x,0,0)} else c(
  model_stats[[x]][[2]],model_stats[[x]][[3]],model_stats[[x]][[4]])}))
colnames(wo_model)<-c("Order","AIC","BIC")
wo_model #very similar

which.min(wo_model[,2]) #AIC order 14
which.min(wo_model[,3]) #BIC order 11

# iv
#look at properties of residuals
#for model 1 first
#model_1<-model_stats[[1]]$model
model_1<-model_stats[[14]]$model

#get standardized residuals
resid_mod1<-model_1@fit$residuals/model_1@fit$sigma

#tails? should be visible looking at 95% quantiles
quantile(resid_mod1,c(0.05,0.5,0.95))
#standardized resids seem to have fat tails
qqnorm(resid_mod1)
qqline(resid_mod1)
#or use specified function
plot(model_1) #number 9

#look at dynamical properties with portmanteau statistics

acf(resid_mod1) # no autocorrelation
pacf(resid_mod1) #no autocorrelation in 14-lag model
#same for squared
acf(resid_mod1^2) 
pacf(resid_mod1^2)
#more autocorrelation in the ARCH part

Box.test(resid_mod1^2,type="Ljung")
Box.test(resid_mod1^2,lag=2,type="Ljung")
Box.test(resid_mod1^2,lag=3,type="Ljung")

#could also use weighted Ljung box test for power increase
#install.packages("WeightedPortTest")
library(WeightedPortTest)
Weighted.Box.test(resid_mod1^2,type="Ljung")

#look at same for residuals
Box.test(resid_mod1,lag=3,type="Ljung")

model_1

############
#iv

#try t distribution

spec_t1<-ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(1,0)),
                   distribution.model = "std")
spec_t2<-ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(14,0)),
                    distribution.model = "std")
t1<-ugarchfit(spec_t1,dax.ret.mat[,1])
t2<-ugarchfit(spec_t2,dax.ret.mat[,1])

resid_t1<-t1@fit$residuals/t1@fit$sigma
resid_t2<-t2@fit$residuals/t2@fit$sigma

#use rugarch plot function
plot(t1)
plot(t2)

Box.test(resid_t1^2,type="Ljung")
Box.test(resid_t2^2,type="Ljung")


############
#v

#do some forecasting

#easy to do with the rugarch package

#check specs again
ugarchspec()

spec_norm1<-ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(1,0)),
                    distribution.model = "norm")
spec_norm14<-ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(14,0)),
                       distribution.model = "norm")

norm1<-ugarchfit(spec_norm1,dax.ret.mat[,1],out.sample = 20,solver = "hybrid")
norm14<-ugarchfit(spec_norm14,dax.ret.mat[,1],out.sample = 20,solver = "hybrid")

fit1_ahead<-ugarchforecast(norm1,n.ahead=20,out.sample =20)
fit14_ahead<-ugarchforecast(norm14,n.ahead=20,out.sample =20)
measures<-function(y_true,y_pred)
{
  if(!length(y_true)==length(y_pred))
  {stop("Inputs do not have same length")}
  return(list(MSE=mean((y_true-y_pred)^2),MAE=mean(abs(y_true-y_pred))))
}
fit1_ahead
plot(fit1_ahead)

#measure fit
len<-length(dax.ret.mat[,1])
measures(dax.ret.mat[(len-19):len,1],fitted(fit1_ahead))
##using ugarch method: careful, out.sample needs to be the same for both objects

#directional accuracy: how good is my forecast in predicting the
#direction (i.e. sign of difference)
#DAC=1/(No. forecasts)*sum(sign(y_t+1-y_t)==sign(y_hat_t+1-y_t))

fpm(fit1_ahead) 
fpm(fit14_ahead) 
#using rolling predicting

fit1_roll<-ugarchroll(spec_norm1,dax.ret.mat[,1],n.ahead=1,forecast.length =20,
                      refit.every = 1)

fit1_roll
fpm(fit1_roll)
#or manual
measures(as.data.frame(fit1_roll)$Realized,as.data.frame(fit1_roll)$Mu)

#for other model
fit14_roll<-ugarchroll(spec_norm14,dax.ret.mat[,1],n.ahead=1,forecast.length =20,
                      refit.every = 1)

fpm(fit14_roll)
fpm(fit1_roll)

