#Financial Econometrics

#Additional Exercise for Estimation of Times Series and (basic) forecasting


#Simulate Model, ARMA 1,1

phi<-0.5
theta<-0.5

#manually:
set.seed(1234)

epsilon<-rnorm(1000)
Y<-rnorm(1) #starting value

for (i in 2:1000)
{
  Y[i]<-phi*Y[i-1]+theta*epsilon[i-1]+epsilon[i]
}
y_true<-Y[501:1000]

#use arima.sim

?arima.sim
#what is a reasonable value?
arima.sim #leave it

set.seed(1234)
arma.1.1<-arima.sim(list(ar=c(phi),ma=(theta)),n=1000) #uses burn in period


fit.0.0<-arima(arma.1.1) #only intercept is fittet

fit.0.0$coef-mean(arma.1.1)
?arima

#estimate model

fit.1.1<-arima(arma.1.1,order=c(1,0,1))

#use ML only: uses 
fit.1.1_ml<-arima(arma.1.1,order=c(1,0,1),method="ML")

#use only conditional sum of squares only:
fit.1.1_css<-arima(arma.1.1,order=c(1,0,1),method="CSS")

#compare with manual simulation
fit.y<-arima(Y,order=c(1,0,1))

#compare:

fit.1.1$coef #Conditional least squares for initial values, then ML
fit.1.1_ml$coef # only ML with initial Values with Kalman Filter
fit.1.1_css$coef #conditions on observations and calculates least square fit


#############################################

#Forecasting, MSE, MAE (not optimal)

#use X percent for estimation and 1-X for forecasting

#compare AR1, MA1 and ARMA11 models with ML

fit_ar<-arima(arma.1.1[1:900],order=c(1,0,0))
fit_ma<-arima(arma.1.1[1:900],order=c(0,0,1))
fit_arma<-arima(arma.1.1[1:900],order=c(1,0,1))

pred_ar<-predict(fit_ar,n.ahead=100)$pred
pred_ma<-predict(fit_ma,n.ahead=100)$pred
pred_arma<-predict(fit_arma,n.ahead=100)$pred

squared_loss<-function(fun,prediction,true)
{
  fun((prediction-true)^2)
}
absolute_loss<-function(fun,prediction,true)
{
  fun(abs(prediction-true))
}
mse_ar<-squared_loss(mean,pred_ar,arma.1.1[901:1000])
mae_ar<-absolute_loss(mean,pred_ar,arma.1.1[901:1000])

mse_ma<-squared_loss(mean,pred_ma,arma.1.1[901:1000])
mae_ma<-absolute_loss(mean,pred_ma,arma.1.1[901:1000])

mse_arma<-squared_loss(mean,pred_arma,arma.1.1[901:1000])
mae_arma<-absolute_loss(mean,pred_arma,arma.1.1[901:1000])


cbind(c(mse_ar,mse_ma,mse_arma),c(mae_ar,mae_ma,mae_arma))

#What could we also do? Use rolling window with 1 step ahead 
#############################################
#play around with parameters
phi=0.5
theta=0.5
set.seed(1234)
arma.1.1<-arima.sim(list(ar=c(phi),ma=(theta)),n=1000) #uses burn in period

#write function yourself
#we will write function that loops through iterations with fixed rolling window
#' get prediction measures for 1 step ahead prediction
#' @param data univariate time series data that is to be fitted
#' @param order order vector with c(ar,I,ma) order
#' @param window_size size of rolling window size, default to 90 percent of observations
#' @param ... additional arguments for arima function
#' @return MSPE and MAE over rolling window
get_prediction_measures<-function(data,order,window_size=NULL,...)
{
 
 if(is.null(window_size))
 {
   window_size<-floor(length(data)*0.9)
 }

 n_forecast<-length(data)-window_size
 error_full<-rep(NA,n_forecast)
 
  for (i in 1:n_forecast)
  {
    fit_temp<-arima(data[i:(window_size+i-1)],order=order,...)
    pred_temp<-predict(fit_temp,n.ahead=1)$pred
    error_temp<-pred_temp-data[window_size+i]
    error_full[i]<-error_temp
  }
 mse<-mean(error_full^2)
 mae<-mean(abs(error_full))
 return(list(measures=c(mse,mae),error=error_full))
}
get_prediction_measures(data=arma.1.1,order=c(1,0,1),window_size = 900)
get_prediction_measures(data=arma.1.1,order=c(1,0,0),window_size = 900)
get_prediction_measures(data=arma.1.1,order=c(0,0,1),window_size = 900)
