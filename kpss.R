#kpss.test statistic

#simulate model
t=10000

set.seed(1231)
iid_err<-rnorm(t)

y<-0.5 + iid_err

plot(y,type="l")

#look at level

resid_level<-y-mean(y)
#sum(abs(resid_level-lm(y~1)$residuals))


s_t<-cumsum(resid_level) #the sum our residuals will converge to zero eventually
plot(s_t, type="l")
plot(s_t^2,type="l")
plot(s_t^2 /t^2,type="l")
var_y<-var(y) #should be one

kpss_level<-sum(s_t^2)/var_y/t^2
kpss.test(y,"Level")

#look at trend

resid_trend<-lm(y~c(1:t))$residuals

s_t_trend<-cumsum(resid_trend) #the sum our residuals will converge to zero eventually
plot(s_t_trend, type="l")
kpss_trend<-sum(s_t_trend^2)/var_y/t^2

kpss.test(y,"Trend")
kpss.test(y,"Level")

#what if we had random walk?

#simulate random walk

set.seed(1212)
err<-rnorm(t)
rw<-rep(NA,t)
rw[1]<-err[1]
for( i in 2:t)
{
  rw[i]<-rw[i-1]+err[i]
}
plot(rw,type="l")
lines(y,col="red")
kpss.test(rw)

#what about variance? Calculate Newey-West manually

rw_model<-lm(rw~1)
rw_resid<-rw_model$residuals

##calculate Newey West Variance
#get lag order
lag<-trunc(4 * (t/100)^0.25)

#get first part of variance
err_sq<-sum(rw_resid^2) /t

#get second covariance part
cov_part<-0
for ( j in 1:lag)
{
  temp_1<-0
  for(q in (j+1):t)
  {
    temp_1<-temp_1+rw_resid[q]*rw_resid[q-j]
  }
  w_j=(1-j/(lag+1))
  cov_part<-cov_part+ temp_1*w_j
}
var_long<-2*cov_part/t + err_sq

#calculate test statistic
s_t_rw<- cumsum(rw_resid)

kpss_rw<-sum(s_t_rw^2) / var_long / t^2

#should be similar :)
kpss_rw
kpss.test(rw)
