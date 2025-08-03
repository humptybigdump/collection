# Set your working directory using
#setwd("C:/.../.../...")
# or make sure to put the files containing the data in the default directory
#getwd()

#########################################################################
# Time Series G:
# Daily average number of defects per truck found in the final 
# inspection at the end of the assembly line of a truck 
# manufacturing plant 
# Source: Wei, Series W1
#########################################################################

G <- ts(scan("G.dat"))

par(mfrow=c(3,1))
ts.plot(G,main="Time Series G")
acf(G)
acf(G,type="partial")

# arima(G,order = c(0,0,2))

G.ar.yw <- ar.yw(G)
G.ar.yw$x.mean                 # 1.79
G.ar.yw$order
G.ar.yw$aic
G.ar.yw$ar                     # 0.43
G.ar.yw$var                    # 0.22

G.ar.mle <- ar.mle(G)
G.ar.mle$x.mean                # 1.78
G.ar.mle$order 
G.ar.mle$aic
G.ar.mle$ar                    # 0.43  
G.ar.mle$var                   # 0.21

par(mfrow=c(3,1))
plot(G.ar.mle$resid,main="Residuals")
acf(G.ar.mle$resid,na.action=na.pass)
acf(G.ar.mle$resid,type="partial",na.action=na.pass)

G.fore <- predict(G.ar.mle,n.ahead=5)
mean(G)                        # 1.79  
sd(G)                          # 0.52
G.fore
G.fore$pred                    # 1.81 1.79 1.79 1.78 1.78
G.fore$se                      # 0.46 0.50 0.51 0.51 0.51
par(mfrow = c(1,1))
ts.plot(G, xlim = c(0,length(G)+5), ylim = c(0,3.5), main="Time Series G")  
lines(G.fore$pred,col="green")
lines(G.fore$pred+1.65*G.fore$se,col="red")
lines(G.fore$pred-1.65*G.fore$se,col="red")

#########################################################################
# Time Series H: simulated MA(3)
#########################################################################
# H <- arima.sim(model=list(ma=c(1,0.2,1)),n=400)
# not invertible 
# while the estimated model is 

H <- ts(scan("H.dat"))

par(mfrow=c(3,1))
ts.plot(H,main="Time Series H")
acf(H)
acf(H,type="partial")

H.ma3 <- arima(H, order=(c(0,0,3)) )
H.ma3                          # 0.38 0.37 0.59
# H.ma3$model                    
H.ma3$sigma2                   # 1.99
H.ma3$aic                      # 1420.8 

par(mfrow=c(3,1))
plot(H.ma3$resid,main="Residuals")
acf(H.ma3$resid,na.action=na.pass)
acf(H.ma3$resid,type="partial",na.action=na.pass)

H.fore <- predict(H.ma3,n.ahead=10)
mean(H)                        # -0.05 
sd(H)                          #  1.82
# H.fore
H.fore$pred                    #  1.05 -1.19 0.35 -0.05 -0.05
H.fore$se                      #  1.41  1.51 1.60  1.80  1.80

par(mfrow=c(1,1))
ts.plot(H, xlim = c(350,410), ylim=c(-5.5,5.5), main="Time Series H")  
lines(H.fore$pred,col="green")
lines(H.fore$pred+1.65*H.fore$se,col="red")
lines(H.fore$pred-1.65*H.fore$se,col="red")

#########################################################################
# Time Series I: simulated AR(3)
#########################################################################
# I <- ts(arima.sim(model=list(ar=c(1.8,-1.07,0.21)),n=400))

I <- ts(scan("I.dat"))

par(mfrow=c(3,1))
ts.plot(I,main="Time Series I")
acf(I)
acf(I,type="partial")

I.ar.yw <- ar.yw(I)
I.ar.yw
I.ar.yw$aic
I.ar.yw$x.mean               # 1.15
I.ar.yw$ar                   # 1.77 -0.99 0.14
I.ar.yw$var                  # 1.01

I.ar.mle <- ar.mle(I,order.max=3)      
I.ar.mle
I.ar.mle$aic
I.ar.mle$x.mean             # 1.13
I.ar.mle$ar                 # 1.76 -0.98 0.14 
I.ar.mle$var                # 0.99

par(mfrow=c(3,1))
plot(I.ar.mle$resid,main="Residuals")
acf(I.ar.mle$resid,na.action=na.pass)
acf(I.ar.mle$resid,type="partial",na.action=na.pass)

I.fore <- predict(I.ar.mle,n.ahead=20)      
mean(I)                     # 1.15
sd(I)                       # 4.78
# I.fore
I.fore$pred                 # 1.28 0.54 0.12 -0.06 -0.05 
I.fore$se                   # 1.00 2.02 2.93  3.63  4.12

par(mfrow=c(1,1))
ts.plot(I, xlim = c(350,420), ylim=c(-18,18), main="Time Series I")  
lines(I.fore$pred,col="green")
lines(I.fore$pred+1.65*I.fore$se,col="red")
lines(I.fore$pred-1.65*I.fore$se,col="red")
