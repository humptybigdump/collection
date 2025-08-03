# Plot of the admissible region
plot(function(x) -x^2/4,xlim = c(-2,2),ylim = c(-1,1),xlab = expression(phi[1]),ylab = expression(phi[2]),lty = 2,main = "Causal region of AR(2)")
lines(x = c(-2,0,2,-2),y = c(-1,1,-1,-1))
abline(h = 0, lty = 2,col = "grey")
abline(v = 0, lty = 2,col = "grey")

# Simulated AR(2) data and (theoretical) ACF
plots.ar2 = function(phi){
  par(mfrow = c(2,1))
  plot(ts(arima.sim(model=list(ar=phi),n=100)),ylim = c(-5,5),ylab = expression(x[t]),main = "Simulated time series")
  abline(h = 0,lty = 2,col = "grey")
  plot(x = 0:10,y = ARMAacf(ar = phi,lag.max = 10),ylim = c(-1,1),type = "h",main = "ACF",xlab = "lag",ylab = "ACF")
  abline(h = 0)
  par(mfrow = c(1,1))
}

plots.ar2(c(0,-0.8))
plots.ar2(c(0,0.8))

plots.ar2(c(0.4,0.4))

plots.ar2(c(-0.4,0.4))

plots.ar2(c(0.4,-0.4))  
plots.ar2(c(0.8,-0.8))  
plots.ar2(c(1.6,-0.8))
plots.ar2(c(0.4,-0.8))

plots.ar2(c(-0.4,-0.4))
plots.ar2(c(-0.8,-0.8))
plots.ar2(c(-1.6,-0.8))
plots.ar2(c(-0.4,-0.8))




