# Set your working directory using
#setwd("C:/.../.../...")
# or make sure to put the files containing the data in the default directory
#getwd()

options(scipen=999) # avoid scientific notation on plot axes

period.weekly = 52
period.monthly = 12
tsd.weekly = ts(scan("weekly.dat"),start = c(2016,1),frequency = period.weekly)
tsd.monthly = ts(scan("monthly.dat"),start = c(2016,1),frequency = period.monthly)

plot(tsd.weekly,xlab = "Year",ylab = "Number of deaths")
plot(tsd.monthly,xlab = "Year",ylab = "Number of deaths")


# Classical Decomposition Algorithm
decomposition = function(x,d){
  n = length(x)
  
  # Step 1
  x.star = filter(x,c(0.5,rep(1,d-1),0.5)/d) # if d is even, otherwise replace filter with rep(1,d)/d
  
  #Step 2
  z = x-x.star
  
  par(mfrow = c(2,2))
  for(t in 1+0:3*d/4) plot(z[t+d*0:(floor(n/d))],ylab = as.expression(bquote(s[d%.%t+.(t)])))
  
  readline("Press enter to continue.")
  
  s.hat = rep(NA,d)
  for(i in 1:d) s.hat[i] = mean(z[seq(i,n,d)],na.rm=TRUE)
  s.hat = s.hat - mean(s.hat)
  
  par(mfrow = c(1,1))
  plot(s.hat)
  
  # here K = 4
  p <- 2*pi*(1:d)/d 
  fit.lm = lm(s.hat ~ cos(p) + sin(p) + cos(2*p) + sin(2*p) + cos(3*p) + sin(3*p) + cos(4*p) + sin(4*p) - 1)  
  lines(fit.lm$fitted, col="green")  
  
  s = as.vector(fit.lm$fitted - mean(fit.lm$fitted))
  # s <- rep(s,n/d)
  
  readline("Press enter to continue.")
  
  #Step 3
  
  dt = suppressWarnings(x-s)
  plot(dt)
  
  t = 1:n
  m.lm = lm(dt ~ t)
  
  lines(ts(m.lm$fitted,start = start(dt),frequency = d),col = "green")
  
  m = as.vector(m.lm$fitted)
  
  #readline("Press enter to continue.")
  
  #Step 4
  
  y <- suppressWarnings(x - s - m) 
  
  return(list(m.lm,s,y))
}

decomp.weekly = decomposition(ts(tsd.weekly[1:208],start = start(tsd.weekly),frequency = period.weekly),period.weekly)
decomp.monthly = decomposition(ts(tsd.monthly[1:48],start = start(tsd.monthly),frequency = period.monthly),period.monthly)

# Plot of the components ######################################################
# Weekly
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

# Monthly
x = ts(tsd.monthly[1:48],start = start(tsd.monthly),frequency = period.monthly)
decomp = decomp.monthly
period = period.monthly

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
acf(decomp.monthly[[3]],main="ACF of monthly residuals")


# Estimates for 2020
# Weekly
estimate.weekly = ts(decomp.weekly[[2]][(1:length(tsd.weekly)-1)%%period.weekly+1] 
                     + predict.lm(decomp.weekly[[1]],data.frame(t = 1:length(tsd.weekly))),
                     start = start(tsd.weekly),frequency = period.weekly)
plot(tsd.weekly,ylim = c(min(tsd.weekly,estimate.weekly),max(tsd.weekly,estimate.weekly)))
lines(estimate.weekly,col = "red")

for(i in 0:floor(length(tsd.weekly)/period.weekly)) abline(v = 2016+i+14/52,col = "grey",lty = 2) # marks calendar week 15 of each year

# Monthly
estimate.monthly = ts(decomp.monthly[[2]][(1:length(tsd.monthly)-1)%%period.monthly+1] 
                     + predict.lm(decomp.monthly[[1]],data.frame(t = 1:length(tsd.monthly))),
                     start = start(tsd.monthly),frequency = period.monthly)
plot(tsd.monthly,ylim = c(min(tsd.monthly,estimate.monthly),max(tsd.monthly,estimate.monthly)))
lines(estimate.monthly,col = "red")

for(i in 0:floor(length(tsd.monthly)/period.monthly)) abline(v = 2016+i+2/12,col = "grey",lty = 2) # marks March of each year
