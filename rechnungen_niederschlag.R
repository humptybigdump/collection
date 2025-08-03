
## Arbeitsverzeichnis setzen
# setwd('...')

# Aufgabe 1- Wasserdampf im Zimmer

   # Funktionsdefinitionen
   magnus <- function(temp) 6.1078 * exp( 17.1 * temp/(235 + temp))
   taup <- function(pdruck) 235 * log(pdruck/6.1078) / ( 17.1 - log(pdruck/6.1078))

   Rw <-  461.6
   flaeche <- 20
   hoehe <- 2


   (est <-   magnus(20) )      # hPa

   (abs100 <- est*100/(Rw * 293.15))      # est in Pa
   (m100 <- abs100*flaeche * hoehe )

   (abs60 <-   est*0.6*100/(Rw * 293.15))
    (m60 <- abs60*flaeche * hoehe)

    (hw60 <- m60/flaeche)

    # halbe Masse auskondensieren
    (mhalb <- m60/2)
(    dichtehalb <- mhalb/(flaeche*hoehe))

   (  pdruckhalb <- dichtehalb * Rw * 293.15)    # Pa

   taup(pdruckhalb*1e-2)           # hPa

#####
#  Abbildung Magnusformel

  temps <- 0:50
  es <- magnus(temps)
  feuchten <- seq(0,1, by=0.1)
  redes <- es%o%feuchten

  opa <- par(mar=c(5,4,2,2)+0.1)
  plot(temps, es, t="l", ylim=c(-5, 122), xlim = c(0,55),
       lwd=2,
       bty="l",
       ylab=expression(paste("Dampfdruck", ~italic(e)," /hPa")),
       xlab=expression(paste("Temperatur", ~vartheta," /°C"))
       )

  matlines(temps, redes, col=1, lty=c(1,2))
  text(54, apply(redes, 2, max), paste(feuchten*100, "%"))
   text( 20, 100, expression(paste( italic(e) == italic(f) %*% 6.1078~"hPa" %*% exp(frac(17.1 %*% vartheta, 235~"°C"  + vartheta)))))
   #    text( 10, 100, expression(paste( paste(italic(e), "*", )[W] == italic(f) %*% 6.1078 %*% exp(frac(17.1 %*% vartheta, 235  + vartheta)))))

  mtext( expression(paste("Rel. Luftfeuchtigkeit ", ~italic(f))), 4)

  par(opa)
  
###
 #  Aufgabe Luftpaket aus Erwins Übung

 #### #####
  T0 = 25                     # Ausgangstemp
  z0 = 115                    # Ausgangshöhe
  f_paket <- c(0.2, 0.4, 0.8) # rel. Feuchten
  flaeche <- 400*300
  hoehe <- 200
  taTG <- 0.98/100                # trockenadiabatischer T-Gradient
  faTG <- 0.6/100                 # feuchtadiabatischer T-Gradient

  pH2O <- magnus(T0)*f_paket       # Partialdruck Wasserdampf hPa
  tau_paket <- taup(pH2O)          # °C

  # Hebungskondensationsniveau mit tau konstant
  hkn0 <- (T0 - tau_paket)/taTG + z0       # m

  # Hebungskondensationsniveau mit tau abnehmend
  hkn1 <- (T0 - tau_paket)/(taTG-0.17/100) + z0       # m

  # temp profil Atmosphäre
  z <- seq(0,5000, by = 100) + z0
  dT <- c(0,rep(1.4, 5), rep(1,5), rep(0.7,40))
  #dT <- c(0,rep(1.5, 5), rep(1,5), rep(0.7,40))

  # T-Profile
  TA =  T0-cumsum(dT)      # Temperaturprofil der Atmosphäre

  Tdry <- T0 - (z-z0)* taTG  # Temperaturprofil bei trockenadiabatischem Aufstieg

#### Aufstieg der Pakete
  # T-Profil Paket
  # bis hkn1 trockenadiabat

   Thkn <- T0 - (hkn1 - z0)*taTG
   # check: Temperatur an HKN unter Umgebungstemperatur?
           Thkn < approx( z, TA,xout=hkn1)$y

   # danach feuchtadiabat
   Ttop <- Thkn - (max(z) - hkn1)*faTG

  ### Sättigungsdampfdruck in 3000 m Höhe
   T3000 <-   Thkn - (3000 - hkn1)*faTG

   p3000 <- magnus(T3000)

   delta_rho <- (pH2O - p3000)*100 /462 /(T0+273.15)       # p in Pa      kg/m^3
    # absF3000 <- *100 /462 /(T0+273.15) * 1000
   ( m_wasser <- delta_rho * flaeche * hoehe)
    N_hoehe <- m_wasser/flaeche

######## Plots

 ### PLot Tempprofil  + trockenadiabate + HKN
   opa <- par(mar=c(5,5,2,2)+0.1)

   plot (TA, z, t="l", lwd=2, col=1, bty="l",
          xlab="Temperatur /°C", ylab="Höhe /(m ü NN)")
   legend("topright", c("Profil Atmosphäre"),
          col=c(1,2,8), lty=1, bty="n", lwd=2)


   par(mar=c(5,5,2,2)+0.1)
   plot (Tdry, z, t="l", lwd=2, col=2, bty="l",
        xlab="Temperatur /°C", ylab="Höhe /(m ü NN)")
    lines(TA, z, col="white", lwd=5)
    lines(TA, z, col=1, lwd=2)
    legend("topright", c("Profil Atmosphäre", "Trockenadiab. Aufstieg"),
           col=c(1,2,8), lty=1, bty="n", lwd=2)
  
   par(mar=c(5,5,2,2)+0.1)
   plot (Tdry, z, t="l", lwd=2, col=2, bty="l",
        xlab="Temperatur /°C", ylab="Höhe /(m ü NN)")
    abline(h=hkn1, col=8, lwd=2)
    lines(Tdry, z, col="white", lwd=10)
    lines(Tdry, z, col=2, lwd=2)
    lines(TA, z, col="white", lwd=10)
    lines(TA, z, col=1, lwd=2)
    legend("topright", c("Profil Atmosphäre", "Trockenadiab. Aufstieg", "HKN"),
           col=c(1,2,8), lty=1, bty="n", lwd=2)
  
   par(mar=c(5,5,2,2)+0.1)
   plot (Tdry, z, t="n" ,  bty="l", xlim=c(-18, 26),
        xlab="Temperatur /°C", ylab="Höhe /(m ü NN)")

    abline(h=hkn1, col=8, lwd=2)
    lines(TA, z, col="white", lwd=10)
    lines(TA, z, col=1, lwd=2)
  for(jj in 1:3) lines(c(T0, Thkn[jj], Ttop[jj]), c(z0, hkn1[jj], max(z)), col="white", lwd=5)

  for(jj in 1:3) lines(c(T0, Thkn[jj], Ttop[jj]), c(z0, hkn1[jj], max(z)), col=5-jj, lty=2, lwd=2)

   legend("bottomleft", c("Profil Atmosphäre", "HKN"), inset=c(1,1)/20,
           col=c(1,8), lty=1, bty="o", lwd=2,  bg="white", box.col="white")
   legend("topright",
          legend=paste("Aufstieg Luftpaket ", f_paket*100  ,"%")
          , col=c(4:2), lty=c(2,2,2,1), bty="n", lwd=2, bg="white")
   par(opa)





   ### plot HKN
    opa <- par(mar=c(3,3,0.5,3.5)+0.1)
              # plot until some points before end
     plot (Tdry[c(1,length(Tdry)-15)], z[c(1,length(z)-15)], t="l", lwd =2,
           ann =F,
          # xlab=expression(paste(tau, ", ", Gamma)),
          # ylab=expression(paste("Höhe ", italic(z))),
          col=2, bty="l",
          ylim=c(100,3800), xlim=c(-15, 27),  xaxt="n", yaxt="n")

          u <- par("usr")
          arrows(u[1], u[3], u[2], u[3], code = 2, xpd = TRUE, length = 0.05, angle = 45)
          arrows(u[1], u[3], u[1], u[4], code = 2, xpd = TRUE, length = 0.05, angle = 45)

          axis(1, at=c( tau_paket[1], T0), label=c( expression(tau[0]), expression(T[0])))
          mtext(expression(paste("Höhe ", italic(z))), side=2, line=1 )
          mtext(expression(paste(tau, ", ", T)), side= 1, at =32)

       # Taupunktslinien dazu
       lines(c(tau_paket[1],tau_paket[1]), c(z0, hkn0[1]) , col= 4, lty=3, lwd =2 )
       lines(c(tau_paket[1], Thkn[1]), c(z0, hkn1[1]) , col= "darkgreen", lty=2, lwd =2  )

       # Beschriftung
         lines(c(-12,Thkn[1]), rep(hkn1[1],2))
         lines(c(tau_paket[1], 8), rep(hkn0[1],2))
         text( x= c(-11, 7), y=c(hkn1[1]-160, hkn0[1]+160),
               c(expression(HKN[tau == abnehm]), expression(HKN[tau == const])))
         text(x= c(-9, 7, 19), y= c(1150,1150,2050),
                 col=c( "darkgreen", "blue","red"),
                 xpd=NA,
                 c("Abnehmender\nTaupunkt", "Konstanter\nTaupunkt",
                 "Trockenadiabatischer\nTemperaturgradient")
                 )

       par(opa)


########
# Nominalbomben

  Vn <-  288*1e6*16.1*1e-3    # m³
  mn <-  Vn * 1000            # kg

  Ekond <- mn * 2.5e6         # J
  nbomb <- Ekond/8.4e13
  nhaush <- Ekond/(1.3e3*3.6e6)


### Übung Hydrologie - Niederschlag - IDW Interpolation

#
 ### IM Beispiel: 7 * 8 matrix
  require(xtable)

  geb  <- matrix(NA,7,8)

  #expand.grid(1:7, 1:8)
  #dist(geb)

  xgeb <- col(geb)  # matrix(rep(1:8,each=7), ncol=8)
  ygeb <- row(geb)  #matrix(rep(1:7,each=8), ncol=8, byrow=T)

  # Stationen an folgenden Punkten
  stat <- data.frame(x = c(5, 5, 3, 2), y = c(2, 4, 3, 6), rain = c(14.5, 10.8, 15, 25.3))

  #   Abstände Gitterpunkte zu stationen
  dist.stat <- list()

  # Liste mit Abständen Rasterzelle - jeweilige Station
  for(jj in 1:nrow(stat)){
  dist.stat[[jj]] <- sqrt((xgeb - stat$x[jj])^2 +   (ygeb - stat$y[jj])^2)
  }

  ## Gewichtetes Mittel ausrechnen

  # inverse Distanz
  invdist <-  lapply(dist.stat, function(x) 1/x)

   # gewichteter Punktwert
   zaehler <- list()
   for(jj in seq(along=invdist)) zaehler[[jj]] <- invdist[[jj]] * stat$rain[jj]

   # summieren
   zaehler <- apply( array(unlist(zaehler),c(7,8,4)) , 1:2, sum)
   nenner <- apply( array(unlist(invdist), c(7,8,4)) , 1:2, sum)

  # Punktwerte ausrechnen
    geb <- zaehler/nenner

    # Stationswerte ergänzen
    for(jj in 1:nrow(stat)) geb[stat$y[jj], stat$x[jj]] <- stat$rain[jj]

    # Gebietsmittel bestimmen
     mean(geb)

   # Nicht enthaltene Teile rausnehmen
   # Array index
   arind <- rbind( c(7,1), c(7,2), c(1,6), c(1,7), c(1,8), c(2,7), c(2,8), c(7,7), c(7,8), c(6,8))

   is.na(geb) <- arind

   mean(geb, na.rm=T)

    # Anteil der Stationen pro Punkt
     ant <- lapply(invdist, function(x) x/nenner)

     # für Punkte
      arind <- rbind( c(3,4), c(5,7), c(6,5), c(5,1), c(3,2))
      blub <- sapply(ant, function(x) x[arind])
        blub <- cbind( blub, geb[arind])

     #Tab für Latex
       print(xtable(blub, digits=2))


###### PLotten
    library(lattice)
    library(rasterVis)
    library(RColorBrewer)

    # Stationen mit Koordinaten
   plot.stat <- stat
   plot.stat$y <- 8 - plot.stat$y
   coordinates(plot.stat) <- ~ x+ y


  myTheme=rasterTheme(region=brewer.pal('Blues', n=9)[-(1)], panel.background = list(col='gray80'))
 
  levelplot(t(geb)[,nrow(geb):1], par.settings=  myTheme,
              main = paste("Interpolierte Niederschläge (MW = ", round(mean(geb, na.rm=T),1), "mm)"),
              xlab="", ylab="") +

           layer(sp.points(plot.stat, pch="X", cex=3.5, col="White"))+
   layer(sp.points(plot.stat, pch="X", cex=3, col="Darkgreen"))
 

 #############################

 # Pu, Tn

 dat <- read.csv("Nmax_60min_1980_2004_Weiherbach.csv")
  # Pfad ggf. anpassen!

  N <- nrow(dat)
  # aufsteigend sortieren
  dat[,1] <- sort(dat[,1], decreasing=F)

  print(xtable(dat, digits=1))

  dat$Pu <- rank(dat)/(N+1)

  dat$Tn <- 1/(1-dat$Pu)

   print(xtable(dat, digits=c(0,1,2,1))     )

   qplot(dat$P..mm., dat$Pu,
          xlab="Niederschlagssumme in 1 h (mm)",
          ylab=expression(italic(P[u])),
          xlim=c(15,50)
          )
  