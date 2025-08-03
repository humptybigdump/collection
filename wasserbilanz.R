#### Skript Abflussdaten Übung Hydrologie 1
# Jan Wienhöfer 2024

### 1.5 KLimatische Wasserbilanz / Doppelsummenkurven

# Erst Pegel Oberstdorf, unten dann Bechhofen

 ### Daten einlesen
 #  Oberstdorf
  obst <- read.csv("1.5_Wasserbilanz_OBST.csv",
                   skip=7, colClasses=c("character","character","numeric","numeric"),
                   sep=";")
    colnames(obst) <- c("Datum","Zeit","N","Q")

  # Datum als Zeitvariable
  obst$Datum <- as.Date(obst$Datum, "%d.%m.%Y")

  # Einzugsgebietsgröße
  obst.area <- 81e6                 # m²

  # Q in mm transformieren
  obst$Q.mm <- obst$Q*3600*1000/obst.area        #m³/s *3600 s/h *1000 L/m³ / (A_ezg [m²]) =  mm/h

  # kumulierte Summen über Zeit auftragen

  plot(cumsum(N)~Datum, obst,      # Niederschlag
       t="l", col=4,
       ylab="Kumuliert /mm", main="Oberstdorf/Stillach")

   lines(cumsum(Q.mm) ~Datum, obst,  # Abfluss
         col=6)
   legend("topl", c("Niederschlag", "Abfluss"),
          col=c(4,6), lty=1, bty="n")

   ## Doppelsummenkurven

   # aggregieren zu Tagesdaten
   obst <- aggregate(cbind(N, Q.mm) ~ Datum, data = obst, FUN=sum)

   ## in hydrologische Jahre zerlegen  - laut Din ab 1.11.
    # Jahreszahl aus Datum
    obst$hj <- as.numeric( strftime(obst$Datum, "%Y"))
    # Monate Nov und Dez finden und Jahreszahl für diese um eins erhöhen
    obst$hj[strftime(obst$Datum, "%m") %in% c(11,12)] <- obst$hj[strftime(obst$Datum, "%m") %in% c(11,12)]+1

    # Datensatz nach hydrologischen Jahren zerlegen
     hj.obst <- split(obst, obst$hj)

   # Doppelsummenkurven für jedes hyd Jahr bestimmen
   dmc.obst <- lapply(hj.obst, function(x) { cQ <- cumsum(x$Q.mm)    # kumulieren
                                        cN <- cumsum(x$N)       # kumulieren
                                        totN <- sum(x$N)
                                        res <- cbind(cN, cQ)/totN                  # normieren
                                        return(res)
                                        })

   # PLot Doppelsummenkurven
   plot(cQ~cN, dmc.obst[[1]], t="l", ylim=c(0,1),
        main="Oberstdorf Doppelsummenkurve" )
   for(jj in 2:length(dmc.obst)) lines(cQ~cN, dmc.obst[[jj]], col=jj)
     legend("topleft", names(dmc.obst), col=1:length(dmc.obst), lty=1, bty="n")

 #############
 #
 #  Bechhofen

  bech <- read.csv("1.5_Wasserbilanz_bech.csv", skip=7, colClasses=c("character","character","numeric","numeric"), sep=";")
  colnames(bech) <- c("Datum","Zeit","N","Q")
  bech$Datum <- as.Date(bech$Datum, "%d.%m.%Y")

  bech.area <- 93.4e6  # m²

  # Q in mm
  bech$Q.mm <- bech$Q*3600*1000/bech.area   # m³/s *3600 s/h *1000 L/m³ / (A_ezg [m²]) =  mm/h

  plot(cumsum(N)~Datum, bech, t="l", col=4, ylab="Kumuliert/mm", main="Bechhof/Wieseth")
   lines(cumsum(Q.mm) ~Datum, bech, col=6)
   legend("topl", c("Niederschlag", "Abfluss"), col=c(4,6), lty=1, bty="n")

  ## Doppelsummenkurven

  # aggregieren (summieren) zu Tagesdaten
  bech <- aggregate(cbind(N, Q.mm) ~ Datum, data = bech, FUN=sum)

  # in hydrologische Jahre zerlegen  - laut Din ab 1.11.
  bech$hj <- as.numeric(strftime(bech$Datum, "%Y") )
  bech$hj[strftime(bech$Datum, "%m") %in% c(11,12)] <- bech$hj[strftime(bech$Datum, "%m") %in% c(11,12)]+1

  plot(cumsum(N)~Datum, bech, t="l")
   lines(cumsum(Q.mm)~Datum, bech, col=3)

   hj.bech <- split(bech, bech$hj)

   dmc.bech <- lapply(hj.bech, function(x) { cQ <- cumsum(x$Q.mm)    # kumulieren
                                        cN <- cumsum(x$N)       # kumulieren
                                        totN <- sum(x$N)
                                        res <- cbind(cN, cQ)/totN                  # normieren
                                        return(res)
                                        })

  # Plot Double mass curve
   plot(cQ~cN, dmc.bech[[1]], t="l", ylim=c(0, 1) ) # 1.1*max(sapply(dmc.bech, function(x) max(x[, "cQ"]) ))) )
   for(jj in 2:length(dmc.bech)) lines(cQ~cN, dmc.bech[[jj]], col=jj)
   legend("topleft", names(dmc.bech), col=1:length(dmc.bech), lty=1, bty="n")
