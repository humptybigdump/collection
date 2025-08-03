#Financial Econometrics II Empirical Exercises

###################################
#Problem 5: Estimation Techniques for Stochastic Volatility Models

#install.packages("stochvol")
library(stochvol) #bayesian analytics

#install.packages("stochvolTMB")
library(stochvolTMB) #frequentist analytics

library(tseries)
######
#i

#get euro USD values again


eur.raw<-get.hist.quote(instrument = "EURUSD=X",start=as.Date("2006-01-01"),
                        end=as.Date("2020-11-01"),quote="Adjusted")

#get cc returns
eur.ret<- 100*diff(log(eur.raw)) #multiply by 100 to avoid numerical instabilities

eur.ret.mat<-data.frame(cc_ret=coredata(eur.ret),dates=index(eur.ret))
row.names(eur.ret.mat)<-eur.ret.mat[,2]
#when do we have missing values?
eur.ret.mat<-eur.ret.mat[-which(is.na(eur.ret.mat)),]
eur.ret.vec<-eur.ret.mat[,1]
names(eur.ret.vec)<-eur.ret.mat[,2]

#split sample into two
n_ins<-floor(length(eur.ret.vec)*0.9)
insamp<-eur.ret.vec[1:n_ins]
outsamp<-eur.ret.vec[(n_ins+1):length(eur.ret.vec)]
#now apply stochastic volatility model
#gaussian distribution and only mean

set.seed(1231)
cl_1=makeCluster(detectCores()-1)
stoch_ar0<-svsample(insamp,designmatrix = "ar0",parallel = "snow",
                    cl=cl_1,print_progress = "automatic")
stopCluster(cl_1)

summary(stoch_ar0)
#here, they use exp instead of log in the model, changes coefficients
#gives posterior distribution for mu (alpha in lecture)
#sigma (sigma_eta in lecture), phi
#and regression coefficients for ARMA model
plot(stoch_ar0)

######
#ii

#use laplace approximation

stochvol_TMB_ar0<-estimate_parameters(insamp, "gaussian")
plot(stochvol_TMB_ar0)
#compare the two
summary(stochvol_TMB_ar0,report="transformed")
stoch_ar0$summary$para

#parameters are very similar, difference is that we estimate them differently

# parameter  estimate   std_error    z_value      p_value        type
# 1:   sigma_y 0.5294255 0.040849240  12.960474 2.049771e-38 transformed
# 2:   sigma_h 0.1691270 0.019204102   8.806815 1.287520e-18 transformed
# 3:       phi 0.9822851 0.004932371 199.150703 0.000000e+00 transformed

# mean          sd          5%         50%         95%       ESS
# phi        0.9807359 0.005382327  0.97139964  0.98114468  0.98880961  162.4502
# sigma      0.1772923 0.020536951  0.14495334  0.17673959  0.21089891  103.9946
# exp(mu/2)  0.5301334 0.042701873  0.46305904  0.52868371  0.60266148 7365.3542


