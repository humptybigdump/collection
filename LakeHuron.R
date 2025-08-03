#########################################################################
# Lake Huron data                                                       #
#                                                                       #
# Level of Lake Huron in feet, 1875 - 1972                              #
#########################################################################

# https://www.google.de/maps/
# https://en.wikipedia.org/wiki/Lake_Huron

# rm( list=ls() )

# pdf()

LH <- LakeHuron

# par(mfrow=c(3,1))

ts.plot(LH, main="Level of Lake Huron in Feet, 1875-1972")
acf(LH)
pacf(LH)

# par(mfrow=c(3,3))

LH.yw <- ar.yw(LH)           # R subtracts the sample mean 
LH.yw$x.mean                 # 579.00  
LH.yw$order                  # 2 
LH.yw$aic                    # 118.7 5.2 0.0 0.3 2.2 3.8 5.8 6.9 8.7 10.7 
                             # smallest value subtracted, so minimum is zero  
LH.yw$ar                     # 1.054 -0.267
LH.yw$var.pred               # 0.508
ts.plot(LH.yw$resid,main="Residuals from AR(2) Yule-Walker Fit")
acf(LH.yw$resid,na.action=na.pass)
pacf(LH.yw$resid,na.action=na.pass)

LH.burg <- ar.burg(LH)       # R subtracts the sample mean 
LH.burg$x.mean               # 579.00  
LH.burg$order                # 2 
LH.burg$aic                  # 121.3 4.1 0.0 0.7 2.3 4.3 6.3 7.7 9.4 11.4 
                             # smallest value subtracted, so minimum is zero  
LH.burg$ar                   # 1.044 -0.246
LH.burg$var.pred             # 0.479
ts.plot(LH.burg$resid,main="Residuals from AR(2) Burg Fit")
acf(LH.burg$resid,na.action=na.pass)
pacf(LH.burg$resid,na.action=na.pass)

LH.mle <- ar.mle(LH)
LH.mle$x.mean                # 579.04  
LH.mle$order                 # 2 
LH.mle$aic                   # 120.0 3.9 0.0 0.6 2.4 4.3 6.3 7.6 9.5 11.5 
LH.mle$ar                    # 1.044 -0.250
LH.mle$var.pred              # 0.479
ts.plot(LH.mle$resid,main="Residuals from AR(2) MLE Fit")
acf(LH.mle$resid,na.action=na.pass)
pacf(LH.mle$resid,na.action=na.pass)

LH.mle3 <- ar.mle(LH,aic=FALSE,order.max=3)
LH.mle3$x.mean               #  9.005
LH.mle3$ar                   #  1.071 -0.352  0.099
LH.mle3$asy.var.coef         #  0.010 -0.010  0.003
                             # -0.010  0.020 -0.010
                             #  0.003 -0.010  0.010
LH.mle3$var                  #  0.472

# par(mfrow=c(1,1))

LH.fore <- predict(LH.mle,n.ahead=10)
LH.fore$pred         # 9.79 9.59 9.43 9.31 9.23 9.17 9.13 9.10 9.08 9.07 
LH.fore$se           #  .69 1.00 1.16 1.23 1.27 1.29 1.29 1.30 1.30 1.30
sqrt(var(LH))        # 1.32
ts.plot(LH, LH.fore$pred, main="Level of Lake Huron in Feet (Reduced by 570)")  
lines(LH.fore$pred,col="green")
lines(LH.fore$pred+2*LH.fore$se,col="red")
lines(LH.fore$pred-2*LH.fore$se,col="red")

# dev.off()