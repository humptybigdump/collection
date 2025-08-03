#Financial Econometrics II

#Problem 3: GARCH and stochastic volatility Estimation

#install and load important packages

#install.packages("rugarch")
#install.packages("forecast")
#install.packages("tseries")
#install.packages("zoo")
#install.packages("stochvol")
library(forecast)
library(tseries)
library(rugarch)
library(zoo)


##########
#i

#simulate one heavy tailed garch and one normal garch

n<-5000 #number of simulated points
#specify parameters for model via fixed.pars
spec_normal<-ugarchspec(mean.model =list(armaOrder=c(1,1)),variance.model = 
                          list(model="sGARCH",garchOrder=c(1,1)),
                        distribution.model = "norm",fixed.pars=
                          list(mu=0.001,ar1=0.6,ma1=-0.2,omega=0.0001,
                               alpha1=0.05, beta1=0.90))

#simulate from a specification via ugarchpath, from a fitted model via ugarchsim
sim_norm<-ugarchpath(spec_normal,n.sim=n,n.start= 100,m.sim=1,rseed=1234)

#what is its call? And its plot?
sim_norm
plot(sim_norm)


#same for fat tailed distribution
#use t distribution with 4 df --> i.e fourth moment is infinite
# this poses problems for theoretical convergence, so interesting how
# ths works in practice

spec_t4<-ugarchspec(mean.model =list(armaOrder=c(1,1)),variance.model = 
                          list(model="sGARCH",garchOrder=c(1,1)),
                        distribution.model = "std",fixed.pars=
                          list(mu=0.001,ar1=0.6,ma1=-0.2,omega=0.0001,
                               alpha1=0.05, beta1=0.90,shape=4))

sim_t4<-ugarchpath(spec_t4,n.sim=n,n.start= 100,m.sim=1,rseed=1234)

sim_t4

set.seed(1212)
#take 3 degrees of freedom
spec_t3<-ugarchspec(mean.model =list(armaOrder=c(1,1)),variance.model = 
                      list(model="sGARCH",garchOrder=c(1,1)),
                    distribution.model = "std",fixed.pars=
                      list(mu=0.001,ar1=0.6,ma1=-0.2,omega=0.0001,
                           alpha1=0.05, beta1=0.90,shape=3))
sim_t3<-ugarchpath(spec_t3,n.sim=n,n.start= 100,m.sim=1,rseed=1234)
#plot them next to each other
#see differences between the two
par(mfrow=c(4,2))
for(i in 1:4)
{
  plot(sim_norm,which=i) 
  plot(sim_t4,which=i) 
}
par(mfrow=c(1,1))


##########
#ii
#estimation strategies

#convert data sets into matrix

sim_t4_vec<-fitted(sim_t4)
sim_norm_vec<-fitted(sim_norm)
sim_t3_vec<-fitted(sim_t3)

#use ugarchfit, look closely at solvers
spec_t<-ugarchspec(mean.model =list(armaOrder=c(1,1)),variance.model = 
                     list(model="sGARCH",garchOrder=c(1,1)),
                   distribution.model = "std")
#here we use the solver and solver_control options
#solvers differ in terms of starting point estimation and numerical minimization
#good call is using "hybrid", which tries out several methods if one does not converge
# or using "solnp", the default one being augmented Lagrange solver
#One can also use "gosolnp", a randomized version of solnp (with multiple
# restarts), therefore needs rseed parameter to be fully reproducible
#this could be good when you are worried to be stuck at a local optimum
#we can also set solver parameters manually using the solver.control argument

#try easy, give right distribution
est_t_solnp<-ugarchfit(spec_t,sim_t4_vec,solver="solnp")

infocriteria(est_t_solnp)
coef(est_t_solnp)

#try the same with normal distribution
spec_norm<-ugarchspec(mean.model =list(armaOrder=c(1,1)),variance.model = 
                        list(model="sGARCH",garchOrder=c(1,1)),
                      distribution.model = "norm")

est_norm_solnp<-ugarchfit(spec_norm,sim_norm_vec,solver="solnp")
est_norm_gosolnp<-ugarchfit(spec_norm,sim_t4_vec,solver="gosolnp",solver.control=
                         list(n.starts=5,n.sim=1000,rseed=1234))

infocriteria(est_norm_solnp)
est_norm_solnp@fit$robust.matcoef
est_t_solnp@fit$robust.matcoef



#GARCH parameters are different and significance is higher, makes sense
#true params: mu=0.001,ar1=0.6,ma1=-0.2,omega=0.0001,alpha1=0.05,
#beta1=0.90,shape=4
#try with smaller sample size again yourself

#compare normal distribution


#try the same with only three degrees of freedom
est_norm_hybrid<-ugarchfit(spec_norm,sim_t3_vec,solver="lbfgs") #fails here

#works
est_norm_solnp<-ugarchfit(spec_norm,sim_t3_vec,solver="solnp") 
#should be at least as good, due to multiple starting points
#you can override default parameters and try out many different solvers

est_gosolnp<-ugarchfit(spec_norm,sim_t3_vec,solver="hybrid",solver.control=
                         list(n.starts=500,n.sim=1000,rseed=1234),numderiv.control = list(hess.eps=1e-1))
#use hybrid tool that tries all different models


###################################
#Problem 4: Value at Risk Forecasting and Evaluation

######
#i
#theoretical

######
#ii
library(PerformanceAnalytics)

#start simple with one step ahead forecast
no_fc<-20
len_sim<-length(sim_t3_vec[,1])
window_length<-len_sim-no_fc
sim_calc<-c(sim_t3_vec)


#historical simulation (simple)
#just use the 5% quantile of all training data for the forecast
var_hist<-quantile(sim_calc[1:window_length],0.05)
#use function from Performance analytics --> make sure your definition is right
VaR(sim_calc[1:window_length],0.95,method="historical")
#GARCH
#take a GARCH (1,1) model with std distribution
spec_garch<-ugarchspec(mean.model =list(armaOrder=c(1,1)),variance.model = 
                         list(model="sGARCH",garchOrder=c(1,1)),
                       distribution.model = "std")
fit_garch<-ugarchfit(spec_garch,sim_calc[1:(window_length+1)],out.sample = 1)
forecast_one_ahead<-ugarchforecast(fit_garch,n.ahead = 1)
VaR_garch<-forecast_one_ahead@forecast$seriesFor+
  qdist("std",p=0.05,shape=fit_garch@fit$coef[7])*forecast_one_ahead@forecast$sigmaFor

#make data.frame for forecasts
forecasts_var<-data.frame(hist=rep(NA,no_fc),garch=rep(NA,no_fc))
forecasts_var_fast<-data.frame(hist=rep(NA,no_fc),garch=rep(NA,no_fc))
#very slow!
for(i in 1:no_fc)
{
  hist_temp<-quantile(sim_calc[i:(window_length-1+i)],0.05)
  #naive method for garch not using rugarch package functionality
  garch_fit_temp<-ugarchfit(spec_garch,
                            data= sim_calc[i:(window_length+i)],
                            out.sample = 1)
  fc_temp<-ugarchforecast(garch_fit_temp,n.ahead = 1)
  garch_temp<-fc_temp@forecast$seriesFor+ 
    qdist("std",p=0.05,shape=garch_fit_temp@fit$coef[7])*fc_temp@forecast$sigmaFor
  forecasts_var[i,1]<-hist_temp
  forecasts_var[i,2]<-garch_temp
  print(i/no_fc*100)
 }

#faster to use ugarchroll, why? Can be parallized
for(i in 1:no_fc)
{
  hist_temp<-quantile(sim_calc[i:(window_length-1+i)],0.05)
  forecasts_var_fast[i,1]<-hist_temp
}
#Elapsed: 30.96343 secs
#garch_fit_roll<-ugarchroll(spec_garch,data= sim_calc,n.ahead = 1,
#                           forecast.length = no_fc,refit.every = 1,
#                           refit.window = "moving",VaR.alpha = 0.05)

#use parallel backend
library(parallel)
cl<-makeCluster(detectCores()-1)
#Elapsed: 9.23266 secs
garch_fit_roll_par<-ugarchroll(spec_garch,data= sim_calc,n.ahead = 1,
                           forecast.length = no_fc,refit.every = 1,
                           refit.window = "moving",VaR.alpha = 0.05,
                           cluster=cl)

forecasts_var_fast[,2]<-garch_fit_roll_par@forecast$VaR[,1]

plot(sim_calc[(window_length+1):len_sim],main="0.05 VaR Forecasts",
     xlab="Forecast Horizon",ylab="Log-Returns",)
lines(forecasts_var_fast[,1],col="red",lty=2)
lines(forecasts_var_fast[,2],col="blue",lty=2)

#what makes a VaR forecast correct?
#if it describes the risk accurately
#i.e. if 5% of values for returns fall below 5% VaR

#for hist
sum(sim_calc[(window_length+1):len_sim]<forecasts_var_fast[,1])/no_fc
#for GARCH
sum(sim_calc[(window_length+1):len_sim]<forecasts_var_fast[,2])/no_fc

#should be at 5%, so not that good
######
#iii

#Backtesting 
#install.packages("GAS")
library(GAS)
oos_data<-sim_calc[(window_length+1):len_sim]
?BacktestVaR
#forecasts_var_fast[,1]
fc<-seq(0,-0.02,length.out=20)
hist_scores<-BacktestVaR(oos_data,fc,alpha=0.05)
garch_scores<-BacktestVaR(oos_data,forecasts_var_fast[,2],alpha=0.05)

#DQ Test
#see Engle RF and Manganelli S. (2004). "CAViaR: Conditional Autoregressive Value at Risk by Regression Quantiles." Journal of Business & Economic Statistics,
hist_scores$DQ
garch_scores$DQ
#both very significant, meaning they are not good forecasts

#Kupiec Test
#see Kupiec PH (1995). "Techniques for Verifying the Accuracy of Risk Measurement Models." The Journal of Derivatives,
hist_scores$LRuc
garch_scores$LRuc
#both somewhat significant

#both are the same since Kupiec only looks at exceendances
# while DQ Test takes form/predictability of VaR violations into account via
# regression on lagged values

#Simple Measure:
#Divide number of exceedances by number of expected exceedences (Actual over Excpected)
#with 20 obs, we would expect 1 exceedance (5% of 20)
#ideally, close to 1
sum(sim_calc[(window_length+1):len_sim]<fc)/1
sum(sim_calc[(window_length+1):len_sim]<forecasts_var_fast[,2])/1

#compare with exceedance measure:
hist_scores$AE
garch_scores$AE
