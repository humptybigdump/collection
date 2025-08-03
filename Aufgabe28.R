# Räumliche Stochastik
# WS 2021/22
# Aufgabe 28

#install.packages("spatstat")- einmalige Installation
library(spatstat)



# Aufgabenteil (a)
# Simulation der dargestellten Realisationen

# Beobachtungsfenster (für die Punktprozessklasse ppp von spatstat)
window = owin(c(0,1),c(0,1))

# homogener Poissonprozess mit Intensität 50
Pois = rpoispp(120, win=window, nsim=1)
#plot(Pois, main="Poisson", pch=16)
plot(Pois$x, Pois$y, main="Poisson", xlim=c(0,1), ylim=c(0,1), xlab="", ylab="", pch=16)


pdf(file="Poisson.pdf")   # Plot abspeichern
par(mar=c(0.1,0.1,2.5,0.1), cex.main=1.8, mfrow=c(1,1))
plot(Pois$x,Pois$y, main="Realisation 3", pch=16)
dev.off()


# Hardcore-Strauss-Prozess mit Interaktionsradius R
# (d.h. es treten keine Punktpaare mit Abstand <= R auf)
Strauss = rStrauss(beta=250, gamma=0, R=0.04, W=window, nsim=1)
#plot(Strauss, main="Strauss", pch=16)
plot(Strauss$x, Strauss$y, main="Strauss", xlim=c(0,1), ylim=c(0,1), xlab="", ylab="", pch=16)


pdf(file="Strauss.pdf")   # Plot abspeichern
par(mar=c(0.1,0.1,2.5,0.1), cex.main=1.8, mfrow=c(1,1))
plot(Strauss$x,Strauss$y, main="Realisation 1", pch=16)
dev.off()


# Matern-Cluster-Prozess
MatC = rMatClust(kappa=40,scale=0.1,mu=3, win=window, nsim=1)
#plot(MatC, main="Matern", pch=16)
plot(MatC$x, MatC$y, main="Matérn", xlim=c(0,1), ylim=c(0,1), xlab="", ylab="", pch=16)


pdf(file="MatC.pdf")   # Plot abspeichern
par(mar=c(0.1,0.1,2.5,0.1), cex.main=1.8, mfrow=c(1,1))
plot(MatC$x,MatC$y, main="Realisation 2", pch=16)
dev.off()



# Aufgabenteil (b)

#Poissonprozess mit Intensität intensity im Beobachtungsfenster [low,high]^2
rPoisson = function(intensity,low=0,high=1) {
  volume = (high-low)^2
  tau = rpois(1,lambda=intensity*volume)   # Punktanzahl auswürfeln
  x = runif(tau, min=low, max=high)        # Koordinaten der Punkte
  y = runif(tau, min=low, max=high)        #   gleichverteilt auswürfeln
  window = owin(c(low,high),c(low,high))
  Phi = ppp(x,y,window)                    # Objekt der Klasse ppp erzeugen
  return(Phi)
}

gamma = 50
BinP = rPoisson(gamma)   # Poissonprozess mit Intensität 50 erzeugen
plot(BinP$x, BinP$y, main="Gemischter Binomialprozess", xlim=c(0,1), ylim=c(0,1), xlab="", ylab="", pch=16)

pdf(file="BinP.pdf")
par(mar=c(2,2,2.5,1), cex.main=1.8, mfrow=c(1,1))
plot(BinP$x, BinP$y, main="Gemischter Binomialprozess", xlim=c(0,1), ylim=c(0,1), xlab="", ylab="", pch=16)
dev.off()



# Aufgabenteil (c)
lambda = 0.02
Y = rexp(1,rate=lambda)   # Intensität auswürfeln
Cox = rPoisson(Y)
plot(Cox$x, Cox$y, main="Cox-Prozess", xlim=c(0,1), ylim=c(0,1), xlab="", ylab="", pch=16)
Cox$n     # Punktanzahl des Prozesses

pdf(file="Cox.pdf")
par(mar=c(2,2,2.5,1), cex.main=1.8, mfrow=c(1,1))
plot(Cox$x, Cox$y, main="Cox-Prozess", xlim=c(0,1), ylim=c(0,1), xlab="", ylab="", pch=16)
dev.off()



# Aufgabenteil (d)

# Matérn-Cluster-Prozess im Beobachtungsfenster [low,high]^2
# kappa - Intensität Mutterprozess
# scale - Interaktionsradius der Kindprozesse
# mu    - Intensität Kindprozesse
rMatern = function(kappa,scale,mu,low=0,high=1) {
  Mother = rPoisson(kappa,low-scale,high+scale)  # Mutterprozess in größerem Fenster
  X = c()
  Y = c()
  rate = pi*scale^2*mu          # erwartete Punktanzahl eines Clusters
  for (i in 1:Mother[["n"]]) {
    beta = rpois(1,lambda=rate)   # Punktanzahl des i-ten Clusters
    for (j in 1:beta) {           # Punkte in Polarkoordinaten
      phi = runif(1, min=0, max=2*pi)   # Richtung gleichverteilt
      r = runif(1, min=0, max=scale^2)
      R = sqrt(r)                       # sichert Gleichverteilung im Ball B(0,scale)
      x = R*cos(phi) + Mother$x[i]      # Umrechnung in euklidische Koordinaten
      y = R*sin(phi) + Mother$y[i]
      if ( low<=x & x<=high & low<=y & y<=high ) { # nur Punkte im Beobachtungsfenster übernehmen
        X = c(X,x)
        Y = c(Y,y)
      }
    }
  }
  window = owin(c(low,high),c(low,high))
  MatC = ppp(X,Y,window)
  return( list(MatC=MatC, Mother=Mother) )   # auch Mutterprozess wird zurückgegeben
}

kappa = 10
scale = 0.1
mu = 500 / pi
MatC = rMatern(kappa,scale,mu)

plot(MatC[["MatC"]]$x, MatC[["MatC"]]$y, main="Matérn-Cluster-Prozess", xlim=c(0,1), ylim=c(0,1), xlab="", ylab="", pch=16)
points(MatC[["Mother"]]$x,MatC[["Mother"]]$y, pch=17, col="red")

pdf(file="Matern.pdf")   # Plot abspeichern
par(mar=c(2,2,2.5,1), cex.main=1.8, mfrow=c(1,1))
plot(MatC[["MatC"]]$x, MatC[["MatC"]]$y, main="Matérn-Cluster-Prozess", xlim=c(0,1), ylim=c(0,1), xlab="", ylab="", pch=16)
points(MatC[["Mother"]]$x,MatC[["Mother"]]$y, pch=17, col="red")
dev.off()



