#Set your working directory using
#setwd("C:/.../.../...")
#or make sure to put the files containing the data in the default directory
#getwd()


# Tests 

portmanteau.test <- function(ts) { # detects large autocorrelations
  k = 20 #10
  myacf <- acf(ts,plot=FALSE) 
  Q <- signif(length(ts)*cumsum(myacf[[1]][2:(k+1)]^2)[k],3)
  p <- signif(1-pchisq(Q,k),2)
  cat("\t Q =", Q, "  p =", p, "\n")
}

turningpoint.test <- function(ts) { # detects correlations at lag 1
  tp <- (sign((ts-lag(ts,-1))*(ts-lag(ts,1)))+1)/2 
  numtp <- cumsum(tp)[[length(ts)-2]]
  z <- abs( (numtp-2*(length(ts)-2)/3) / sqrt((16*length(ts)-29)/90) )
  p <- signif(2*(1-pnorm(z)),2)
  cat("\t T =", numtp, "  p =", p, "\n")
}

differencesign.test <- function(ts){ # detects trend, can be "fooled" by a strong cyclic component!
  sc <- (sign(ts-lag(ts,k=-1))+1)/2
  S <- cumsum(sc)[[length(ts)-1]]
  z <- abs( (S-(n-1)/2) / sqrt((length(ts)+1)/12) )
  p <- signif(2*(1-pnorm(z)),2) 
  cat("\t S =", S, "  p =", p, "\n")
}

rank.test = function(ts){ # useful for detecting (linear) trend
  n = length(ts)
  rs = unlist(lapply(1:(n-1),function(h) (sign(ts-lag(ts,k=-h))+1)/2))
  R = sum(rs)
  z = abs((R-n*(n-1)/4)/sqrt(n*(n-1)*(2*n+5)/72))
  p <- round(2*(1-pnorm(z)),2) 
  print(paste("R =",R,"p =",p))
}


# Time Series

n <- 200


# Time Series A: MA(1)

# set.seed(100)
# A <- ts(round(arima.sim(n,model=list(ma=.5)),4))

A <- ts(scan("A.dat"))


# Time Series B: IID Poisson(2000)  

# set.seed(300)
# B <- ts(rpois(n,2000))

B <- ts(scan("B.dat"))


# Time Series C:
# Standard and Poor's 500 Index closing values from March 8 to 
# Dezember 16, 1988
# Source: http://lib.stat.cmu.edu/datasets/spdc2693

C <- ts(scan("C.dat"))


# Time Series D: 
# Iterates of the Logistic Function 
# S.M. Ulam and John von Neumann, Bull. Amer. Math Soc. 53, 1120 (1947)

# D <- seq(n)
# D[1] <- 0.4736779
# for (i in 2:200)  D[i] <- 4*D[i-1]*(1-D[i-1])
# D <- ts(round(D,5))  

D <- ts(scan("D.dat"))


# Time Series E: IID normal(20,5) 

# set.seed(400)
# E <- ts(round(rnorm(n,20,5),2))  

E <- ts(scan("E.dat"))


# Time Series F:
# New Jersey Pick-It Lottery Data
# 200 winning 3-digit numbers (from 000 to 999) for drawings starting 
# May 22, 1975. (This was the beginning of the Pick-It lottery.)
# Source: Splus

F <- ts(scan("F.dat"))


# Plots

par(mfrow=c(2,3)) 

ts.plot(A,main="Time Series A")
ts.plot(B,main="Time Series B")
ts.plot(C,main="Time Series C")
ts.plot(D,main="Time Series D")
ts.plot(E,main="Time Series E")
ts.plot(F,main="Time Series F")


# Analysis of the time series

par(mfrow=c(2,3)) 

plot( A, main="Time Series A" )
acf( A, 50, "correlation", main="Sample ACF" )
plot( A, lag(A,1), ylab="lag(A,1)", main = "Lag Plot" )
portmanteau.test(A)        #  Q = 30.2  p = 0.00  for k = 10
turningpoint.test(A)       #  T = 116   p = 0.01
differencesign.test(A)     #  S = 104   p = 0.27
rank.test(A)               #  R = 10007 p = 0.9

ts.plot( B, main="Time Series B" )
acf( B, 50, "correlation", main="Sample ACF" )
plot( B, lag(B,1), ylab="lag(B,1)", main = "Lag Plot" )
portmanteau.test(B)        #  Q = 9.34   p = 0.5  for k = 10
turningpoint.test(B)       #  T = 128    p = 0.5  
differencesign.test(B)     #  S = 101    p = 0.71  
rank.test(B)               #  R = 9827.5 p = 0.8

par(mfrow=c(2,3)) 

ts.plot( C, main="Time Series C" )
acf( C, 50, "correlation", main="Sample ACF" )
plot( C, lag(A,1), ylab="lag(C,1)", main = "Lag Plot" )
portmanteau.test(C)        #  Q = 962   p = 0.00  for k = 10
turningpoint.test(C)       #  T = 104   p = 0.00 
differencesign.test(C)     #  S = 110   p = 0.01 
rank.test(C)               #  R = 13669 p = 0

ts.plot( D, main="Time Series D" )
acf( D, 50, "correlation", main="Sample ACF" )
plot( D, lag(D,1), ylab="lag(D,1)", main = "Lag Plot" )
portmanteau.test(D)        #  Q = 12.6  p = 0.25  for k = 10
turningpoint.test(D)       #  T = 134   p = 0.74
differencesign.test(D)     #  S = 132   p = 0.00
rank.test(D)               #  R = 9944  p = 0.99

par(mfrow=c(2,3)) 

ts.plot( E, main="Time Series E" )
acf( E, 50, "correlation", main="Sample ACF" )
plot( E, lag(E,1), ylab="lag(E,1)", main = "Lag Plot" )
portmanteau.test(E)        #  Q = 6.36    p = 0.78  for k = 10
turningpoint.test(E)       #  T = 125     p = 0.24
differencesign.test(E)     #  S = 100     p = 0.90
rank.test(E)               #  R = 10179.5 p = 0.63

ts.plot( F, main="Time Series F" )
acf( F, 50, "correlation", main="Sample ACF" )
plot( F, lag(F,1), ylab="lag(F,1)", main = "Lag Plot" )
portmanteau.test(F)       #  Q = 14.1  p = 0.17  for k = 10
turningpoint.test(F)      #  T = 124   p = 0.18
differencesign.test(F)    #  S = 95    p = 0.27 
rank.test(F)              #  R = 10318 p = 0.44









