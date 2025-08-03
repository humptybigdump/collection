# Set your working directory using
#setwd("C:/.../.../...")
# or make sure to put the files containing the data in the default directory
#getwd()

#####################################################################################
# Code from solultion for Problem 2 of Problem Set 3
options(scipen=999) # avoid scientific notation on plot axes

period.weekly = 52
tsd.weekly = ts(scan("weekly.dat"),start = c(2016,1),frequency = period.weekly)

plot(tsd.weekly,xlab = "Year",ylab = "Number of deaths")

# Classical Decomposition Algorithm
decomposition = function(x,d){
  n = length(x)
  
  # Step 1
  x.star = filter(x,c(0.5,rep(1,d-1),0.5)/d) # if d is even, otherwise replace filter with rep(1,d)/d
  
  #Step 2
  z = x-x.star
  
  # par(mfrow = c(2,2))
  # for(t in 1+0:3*d/4) plot(z[t+d*0:(floor(n/d))],ylab = as.expression(bquote(s[d%.%t+.(t)])))
  
  # readline("Press enter to continue.")
  
  s.hat = rep(NA,d)
  for(i in 1:d) s.hat[i] = mean(z[seq(i,n,d)],na.rm=TRUE)
  s.hat = s.hat - mean(s.hat)
  
  # par(mfrow = c(1,1))
  # plot(s.hat)
  
  # here K = 4
  p <- 2*pi*(1:d)/d 
  fit.lm = lm(s.hat ~ cos(p) + sin(p) + cos(2*p) + sin(2*p) + cos(3*p) + sin(3*p) + cos(4*p) + sin(4*p) - 1)  
  # lines(fit.lm$fitted, col="green")  
  
  s = as.vector(fit.lm$fitted - mean(fit.lm$fitted))
  # s <- rep(s,n/d)
  
  # readline("Press enter to continue.")
  
  #Step 3
  
  dt = suppressWarnings(x-s)
  # plot(dt)
  
  t = 1:n
  m.lm = lm(dt ~ t)
  
  # lines(ts(m.lm$fitted,start = start(dt),frequency = d),col = "green")
  
  m = as.vector(m.lm$fitted)
  
  #readline("Press enter to continue.")
  
  #Step 4
  
  y <- suppressWarnings(x - s - m) 
  
  return(list(m.lm,s,y))
}

decomp.weekly = decomposition(ts(tsd.weekly[1:208],start = start(tsd.weekly),frequency = period.weekly),period.weekly)
#decomp.monthly = decomposition(ts(tsd.monthly[1:48],start = start(tsd.monthly),frequency = period.monthly),period.monthly)

# Plot of the components
x = ts(tsd.weekly[1:208],start = start(tsd.weekly),frequency = period.weekly)
decomp = decomp.weekly
period = period.weekly

y = decomp[[3]]
m = as.vector(decomp[[1]]$fitted.values)
s = decomp[[2]]
t = 1:length(y)

plot(x,ylim = c(min(c(s,y)),max(x)))
lines(y,col = "blue")
lines(ts(m,start = start(x),frequency = period),col = "red")
lines(ts(s[(t-1)%%period+1],start = start(x),frequency = period),col = "green")

for(i in 0:floor(length(x)/period)) abline(v = 2016+i,col = "grey",lty = 2)
abline(h = 0,col = "grey",lty = 2)

# Sample ACF of the random component
acf(decomp.weekly[[3]],main="ACF of weekly residuals")

# Estimates for 2020
estimate.weekly = ts(decomp.weekly[[2]][(1:length(tsd.weekly)-1)%%period.weekly+1] 
                     + predict.lm(decomp.weekly[[1]],data.frame(t = 1:length(tsd.weekly))),
                     start = start(tsd.weekly),frequency = period.weekly)
plot(tsd.weekly,ylim = c(min(tsd.weekly,estimate.weekly),max(tsd.weekly,estimate.weekly)))
lines(estimate.weekly,col = "red")

for(i in 0:floor(length(tsd.weekly)/period.weekly)) abline(v = 2016+i+14/52,col = "grey",lty = 2) # marks calendar week 15 of each year
#####################################################################################

# Residuals
y = decomp.weekly[[3]]

par(mfrow = c(3,1))
plot(y)
acf(y)
pacf(y)

ar = ar.yw(y)
# ar.mle(y)
ar

fore = predict(ar,n.ahead=length(tsd.weekly)-208)

m = ts(predict.lm(decomp.weekly[[1]],data.frame(t = 1:length(tsd.weekly))),start = start(tsd.weekly),frequency = period.weekly)
s = ts(decomp.weekly[[2]][(1:length(tsd.weekly)-1)%%period.weekly+1],start = start(tsd.weekly),frequency = period.weekly)

par(mfrow=c(1,1))
ts.plot(tsd.weekly,xlim = c(2019,2020+16/52), main="Weekly Deaths")

abline(v = 2020,col = "grey",lty = 2)

lines(m+s,col="green",lty = 2) # previous estimate using trend and seasonal component
lines(fore$pred + m + s,col="green") # with AR(1)-model

lines(fore$pred+m+s+1.65*fore$se,col="red")
lines(fore$pred+m+s-1.65*fore$se,col="red")





