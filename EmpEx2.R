#Financial Econometrics II Empirical Exercises

#Problem 2: GARCH models in R

#install and load important packages

#install.packages("rugarch")
#install.packages("forecast")
#install.packages("tseries")
#install.packages("zoo")
library(forecast)
library(tseries)
library(rugarch)
library(zoo)


#get dax returns continued
############
#i

#Download DAX data from January 2006 to November 2020

#dax price data as zoo object from tseries package
dax.raw <- get.hist.quote(instrument = "^GDAXI",start=as.Date("2006-01-01"),
                          end=as.Date("2020-11-01"),quote="Adjusted")

eur.raw<-get.hist.quote(instrument = "EURUSD=X",start=as.Date("2006-01-01"),
                        end=as.Date("2020-11-01"),quote="Adjusted")
  
#get cc returns

dax.ret<- diff(log(dax.raw))
eur.ret<- 100*diff(log(eur.raw)) #multiply by 100 to avoid numerical instabilities
data(dmbp)
#from zoo package
dax.ret.mat<-data.frame(cc_ret=coredata(dax.ret),dates=index(dax.ret))
row.names(dax.ret.mat)<-dax.ret.mat[,2]

eur.ret.mat<-data.frame(cc_ret=coredata(eur.ret),dates=index(eur.ret))
row.names(eur.ret.mat)<-eur.ret.mat[,2]
#when do we have missing values?
dax.ret.mat[is.na(dax.ret.mat),]
dax.ret.mat<-dax.ret.mat[-which(is.na(dax.ret.mat)),]

eur.ret.mat[is.na(eur.ret.mat),]
eur.ret.mat<-eur.ret.mat[-which(is.na(eur.ret.mat)),]

eur.ret.vec<-eur.ret.mat[,1]
names(eur.ret.vec)<-eur.ret.mat[,2]

plot(eur.raw)
plot(eur.ret)



###################
#i
#garch specs on dax

#just fit 4 orders
ar_part<-1:4
ma_part<-1:4
len<-length(ar_part)*length(ma_part)
init<-rep(0,len)
models_garch<-data.frame(p=init,q=init,AIC=init,BIC=init)
pos<-0
for(p in ar_part)
{
  for(q in ma_part)
  {
    pos<-pos+1
    spec_garch<-ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(p,q)))
    tryCatch({
      temp<-ugarchfit(spec_garch,dax.ret.mat[,1])
      models_garch[pos,]<-c(p,q,infocriteria(temp)[1],infocriteria(temp)[2])
    
    print(paste0("Finished with Model ",p, ",",q))
    })
  
  }
}

models_garch
models_garch[order(models_garch[,3]),] #aic, more weight on likelihood
models_garch[order(models_garch[,4]),] #punishes larger models more (depending on sample size)

#GARCH AIC BIC
# 3 4 -6.043423 -6.025162
# 1 1 -6.042588 -6.032628
# 2 1 -6.043389 -6.031768
#compare to ARCH
#[14,]    14 -6.035375 -6.005493
#[15,]    15 -6.034802 -6.003261

#best model seems to be simple model, could try 2,1 model too
#In comparison to plain ARCH, much simpler models are sufficient
spec_1.1_dax<-ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(1,1)))
dax_1.1<-ugarchfit(spec_1.1_dax,dax.ret.mat[,1])
dax_1.1

#test statistics (more descriptions in rugarch vignette on CRAN)
#########
##Ljung Box tests (weighted for more power)
#looks at more lags too (if they have autocorrelation combined)
##ARCH LM tests
#test the null distribution of an adequatly fitted ARCH model
#check if you should include more effects
##Nyblom Stability
#Tests for the stability of parameters over time, null states they are not changing
#Rejection of null could indicate usage of some asymmetric model
## Sign Bias test
# by Engle  and  Ng  (1993)
#tests for leverage of squared residuals, i.e. how do they react to shocks
#in e_t-1, does that with regression and checks coefficient significance
#significance could indicate apARCH model
##Pearson Goodness of Fit test
#test standardized residuals against theoretical density (e.g. norm)
#here, test is adapted to account for non-iid effect by reordering residuals



####################
#ii
#garch stuff on euro usd

#first look at mean and sd for arima spec
mean(eur.ret.vec) #very small
hist(eur.ret.vec,breaks=100)

#garch terms?
acf(eur.ret.vec^2) #1 cond. variance term
pacf(eur.ret.vec^2) #2 squared innovation terms?

#can take a bit, gives us ARMA(2,0) or ARMA(0,1)
#try 1,1 first
autoarfima(eur.ret.vec,method = "partial",criterion="AIC") 
autoarfima(eur.ret.vec,method = "partial",criterion="BIC")
spec_garch1.1<-ugarchspec(mean.model =list(armaOrder=c(2,0)),variance.model = 
                            list(model="sGARCH",garchOrder=c(1,1)))

garch.fit<-ugarchfit(spec_garch1.1,eur.ret.vec)
garch.fit #what can we conclude here? Auto-correlation?
plot(garch.fit) #12 News impact curve
infocriteria(garch.fit)
#4: Maybe use MA(1) term in ARMA model
#9: qqplot reveals fat tails?
plot(garch.fit, which=12)

#could there be non-stationarity in GARCh part?
garch.fit@fit$persistence #close to 1
std_resid<-garch.fit@fit$residuals /garch.fit@fit$sigma
plot(std_resid^2,type="l") #square of standardized residuals
plot(garch.fit@fit$sigma^2,type="l") #conditonal variance
plot(garch.fit@fit$residuals^2,type="l")

#acf of squared residuals
pacf(garch.fit@fit$residuals^2)

#coming up with a better garch specification
p<-2 #arch order (i.e. squared residuals)
q<-1 #garch order (i.e. squared conditional variance)

#iGARCH fit
spec_igarch<-ugarchspec(mean.model =list(armaOrder=c(2,0)),
                        variance.model = list(model="iGARCH",garchOrder=c(p,q)))
i_garch.fit<-ugarchfit(spec_igarch,eur.ret.vec)
i_garch.fit #what can we see?
plot(i_garch.fit)

# iGarch incorporates the unit root restriction: No estimation of last parameter

#compare with GARCH(1,1)
coef(garch.fit)
coef(i_garch.fit) #pretty similar

infocriteria(garch.fit)
infocriteria(i_garch.fit)

#compare baseline with GARCH 2.1 and arma 2,1
spec_garch.2.1<-ugarchspec(mean.model =list(armaOrder=c(2,1)),variance.model = 
                            list(model="sGARCH",garchOrder=c(2,1)),
                           distribution.model = "std")

garch.2.1.fit<-ugarchfit(spec_garch.2.1,eur.ret.vec)
plot(garch.2.1.fit,which=9)
signbias(garch.2.1.fit)
signbias(garch.fit)
#since the coefficients seem to be changing (Nyblom stability), we might try
# an asymmetric model
#also use MA term that was indicated in plot 4
#EGARCH FIT

spec_egarch<-ugarchspec(mean.model =list(armaOrder=c(2,1)),
                        variance.model = list(model="eGARCH",garchOrder=c(p,q)))
e_garch.fit<-ugarchfit(spec_egarch,eur.ret.vec)
e_garch.fit

nyblom(garch.2.1.fit)
nyblom(e_garch.fit)
#better regarding stability of coefficients
#what about leverage of innovations?
signbias(garch.fit)
signbias(e_garch.fit) #still high leverage of squared innovation/errors
plot(e_garch.fit)

#Try apARCH models (asymmetric Power)
#T-GARCH e.g. GJR-GARCH as special case of this model
#should take squared innovations more  directly into account

spec_gjrgarch<-ugarchspec(mean.model =list(armaOrder=c(2,1)),
                        variance.model = list(model="gjrGARCH",garchOrder=c(p,q)))
gjr_garch.fit<-ugarchfit(spec_gjrgarch,eur.ret.vec)
gjr_garch.fit 
#stability is worse now
nyblom(gjr_garch.fit)
nyblom(e_garch.fit)

#leverage is worse too
signbias(gjr_garch.fit)
signbias(e_garch.fit)

#try other conditional distribution
spec_gjrgarch_ged<-ugarchspec(mean.model =list(armaOrder=c(2,1)),
                          variance.model = list(model="gjrGARCH",garchOrder=c(p,q)),
                          distribution.model = "ged")
gjr_garch.ged.fit<-ugarchfit(spec_gjrgarch_ged,eur.ret.vec)
gjr_garch.ged.fit
plot(gjr_garch.ged.fit)

#apARCH
#finally try an apARCH model to see whether leverage effects persist

spec_aparch_ged<-ugarchspec(mean.model =list(armaOrder=c(2,1)),
                              variance.model = list(model="apARCH",garchOrder=c(p,q)),
                              distribution.model = "sged")
apgarch.ged.fit<-ugarchfit(spec_aparch_ged,eur.ret.vec)
apgarch.ged.fit
plot(apgarch.ged.fit)


####################
#iv

#use the best models for an out-of-sample forecast using t-distribution
#take 80% of obs as your window and refit every 10 observations to obtain better
#computational performance
#garch2.1
#e_garch.2.1
#apARCH 2.1
garch_order<-c(2,1)
distr<-"std"
length_fc<-floor(length(eur.ret.vec)*0.1)
spec_oos_garch<-ugarchspec(mean.model =list(armaOrder=c(2,1)),
                           variance.model = list(model="sGARCH",
                                                 garchOrder=garch_order),
                           distribution.model = distr)
spec_oos_egarch<-ugarchspec(mean.model =list(armaOrder=c(2,1)),
                            variance.model = list(model="eGARCH",
                                                  garchOrder=garch_order),
                            distribution.model = distr)
spec_oos_aparch<-ugarchspec(mean.model =list(armaOrder=c(2,1)),
                            variance.model = list(model="apARCH",
                                                  garchOrder=garch_order),
                            distribution.model = distr)


#rolling prediction, takes some time
#we can also use parallel (works different depending on OS), but way faster
library(parallel)
cl <- makeCluster(detectCores()-1)
garch_roll<-ugarchroll(spec_oos_garch,data=eur.ret.vec,n.ahead = 1,forecast.length =length_fc,
                       refit.every = 30,refit.window = "moving",cluster=cl,
                       solver="hybrid")
egarch_roll<-ugarchroll(spec_oos_egarch,data=eur.ret.vec,n.ahead = 1,forecast.length =length_fc,
                       refit.every = 30,refit.window = "moving",cluster=cl,
                       solver="hybrid")

aparch_roll<-ugarchroll(spec_oos_aparch,data=eur.ret.vec,n.ahead = 1,forecast.length =length_fc,
                        refit.every = 30,refit.window = "moving",cluster=cl,
                        solver="hybrid")
fpm(garch_roll)
fpm(egarch_roll) #egarch best in terms of out of sample forecast
fpm(aparch_roll)

#look at best model a bit closer 
egarch_roll # took 8 seconds to run, 13 refits
plot(egarch_roll,which=2)
plot(egarch_roll,which=3)
plot(egarch_roll,which=4)
plot(egarch_roll,which=5)

par(mfrow=c(1,1))

#do bootstrap forecasts, if method is set to full, this can take very long
#why bootstrap: we are forecasting using the distribution specified for GARCh
# models and drawing new samples
#more details in:
#L. Pascual, J.Romo, and E.Ruiz.
#Bootstrap prediction for returns and volatilities in garchmodels.
#Computational Statistics and Data Analysis, 50(9):2293-2312, 2006

#uncertainty arises:
# - through underlying distribution, i.e. t-distribution here
# - through parameter uncertainty, i.e. our alpha, beta,...
#so with partial, we just bootstrap our standardized residuals to build
# empirical distribution, i.e. for b=1,...,B, draw bootstrap samples from y_t 
# and calculate path iteratively (parameter n.bootpred)

#with full, we additionally refit our parameters n.bootfit times using simulation
# starting from our last observation and using bootstrapped innovations (of size N)
#this will give us n.bootfit different parameters, thus allowing for a parameter
# distribution
#
n_oos<-10
in_samp<-1:(length(eur.ret.vec)-n_oos)
out_samp<-(length(eur.ret.vec)-n_oos+1):length(eur.ret.vec)
egarch_insamp<-ugarchfit(spec_oos_egarch,eur.ret.vec,out.sample = n_oos)
#might take very long
boot_egarch<-ugarchboot(egarch_insamp,method="Full",rseed=c(1:(100+500)),n.ahead = n_oos,
                        n.bootfit=100,n.bootpred = 500,cluster=cl,verbose=TRUE)
boot_egarch
plot(boot_egarch,which=2)
plot(boot_egarch,which=3)
plot(boot_egarch,which=1)
par(mfrow=c(1,1))
#with simple, it's really fast, but no density :(
#try with 10/10 and 250/500
egarch_insamp_large<-ugarchfit(spec_oos_egarch,eur.ret.vec,out.sample = 250)
boot_simple_egarch<-ugarchboot(egarch_insamp_large,method="Partial",n.ahead = 500,
                               cluster=cl,verbose=TRUE)
plot(boot_simple_egarch,which=2)
plot(boot_simple_egarch,which=3)

boot_simple_egarch
boot_egarch
