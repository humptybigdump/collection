#####################################################
# S&P 500 Index from March 8, 1988 to March 1, 1989 #
#####################################################

# pdf()

rm(list=ls())

# training set: March 8 to December 16, 1988

SP500.full <- ts(scan("My-Stat-Files/KIT/time.series/R/SP500.dat"))
SP500 <- ts(SP500.full[1:200])      

# par(mfrow=c(3,1))

ts.plot(SP500, main="S&P 500: March 8 to December 16, 1988")
acf(SP500)
acf(SP500, type="partial", 23)

SP500.ar <- ar.mle(SP500)
SP500.ar$x.mean                 # 268.0
SP500.ar$order                  # 1
SP500.ar$aic                    # 418.8 0.0 1.8 3.7 5.1
SP500.ar$ar                     # 0.936
SP500.ar$var.pred               # 6.14

ts.plot(SP500.ar$resid, main="Residuals from AR(1) MLE fit")
acf(SP500.ar$resid, na.action=na.pass)
acf(SP500.ar$resid, type="partial", na.action=na.pass)

SP500.diff <- diff(SP500)

ts.plot(SP500.diff, main="First Differences of S&P 500: March 8 to December 16, 1988")
mean(SP500.diff)                # 0.0344
sd(SP500.diff)                  # 2.52
acf(SP500.diff)
acf(SP500.diff, type="partial", 23)

# par(mfrow=c(1,1))

SP500.ar.fore <- predict(SP500.ar,n.ahead=50)
ts.plot(SP500, SP500.ar.fore$pred, main="S&P 500: March 8, 1988 to March 1, 1989")
lines(SP500.ar.fore$pred,col="red")
lines(SP500.ar.fore$pred+2*SP500.ar.fore$se,col="red")
lines(SP500.ar.fore$pred-2*SP500.ar.fore$se,col="red")

SP500.rw.fore <- ts( rep(SP500[200],50), start=201) 
pred.se <- sqrt(1:50)*sd(SP500.diff) 

ts.plot(SP500, SP500.rw.fore, ylim=c(241,312), main="S&P 500: March 8, 1988 to March 1, 1989")
lines(SP500.ar.fore$pred,col="red")
lines(SP500.ar.fore$pred+2*SP500.ar.fore$se,col="red")
lines(SP500.ar.fore$pred-2*SP500.ar.fore$se,col="red")
lines(SP500.rw.fore,col="green")
lines(SP500.rw.fore+2*pred.se,col="green")
lines(SP500.rw.fore-2*pred.se,col="green")

# test set: December 17, 1988 to March 1, 1989

ts.plot(SP500, SP500.rw.fore, ylim=c(241,312), main="S&P 500: March 8, 1988 to March 1, 1989")
lines(SP500.ar.fore$pred,col="red")
lines(SP500.ar.fore$pred+2*SP500.ar.fore$se,col="red")
lines(SP500.ar.fore$pred-2*SP500.ar.fore$se,col="red")
lines(SP500.rw.fore,col="green")
lines(SP500.rw.fore+2*pred.se,col="green")
lines(SP500.rw.fore-2*pred.se,col="green")
lines(SP500.full)

# dev.off()






