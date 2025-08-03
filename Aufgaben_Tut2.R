# Aufgabe 1
  # a) Erstelle je eine numerische, charakterliche und logische Variable.
  
# numerisch
numerisch <- 4

# charakterlich
charakter <- "Wasserfloh"

# logisch
logisch <- TRUE

  # b) ‹berpr¸fe die Datentypen.

class(numerisch) # -> numeric
class(charakter) # -> character
class(logisch) # -> logical

# Aufgabe 2
  # a) Speichere die Gewinne von Paula (5 Euro), Leon (-1,20 Euro), Marie (-3,60 Euro) und Moritz (10,80 Euro) in einem Vektor ab.
  woche_1 <- c(5, -1.20, -3.60, 10.80)
  
  # b) Benenne den Vektor nach Spieler*innen
  names(woche_1) <- c("Paula", "Leon", "Marie", "Moritz")
  
  # c) Ordne die Gewinne/Verluste den Spieler*innen zu
  woche_2 <- c(-1.20, 4.60, 0.80, 1.20)
  names(woche_2) <- c("Paula", "Leon", "Marie", "Moritz")
  
  # d) Berechne die Gesamtgewinne/-verluste. War die erste Gruppe erfolgreicher als die zweite?
  sum(woche_1) # -> 11 Euro
  sum(woche_2) # -> 5,40 Euro
  total_woche_1 <- sum(woche_1)
  total_woche_2 <- sum(woche_2)
  total_woche_1 > total_woche_2 # -> TRUE: Woche 1 war erfolgreicher als Woche 2.
  mean(woche_1) # -> 2,75 Euro
  mean(woche_2) # -> 1,35 Euro
  
  # e) Erstelle einen Gesamtvektor sowie anschlieﬂend einen Gewinn- und einen Verlustvektor
  total_lotto <- woche_1 + woche_2
  gewinne_lotto <- total_lotto > 0
  verluste_lotto <- total_lotto < 0
  
  # f¸r Schnelle: Waren die Jungen oder die M‰dchen erfolgreicher?
  # Woche 1
  jungen_woche_1 <- woche_1[c("Leon", "Moritz")]
  maedchen_woche_1 <- woche_1[c("Paula", "Marie")]
  sum(jungen_woche_1) > sum(maedchen_woche_1) # -> TRUE: Die Jungen waren erfolgreicher als die M‰dchen.
  
  # Woche 2
  jungen_woche_2 <- woche_2[c("Leon", "Moritz")]
  maedchen_woche_2 <- woche_2[c("Paula", "Marie")]
  sum(jungen_woche_2) > sum(maedchen_woche_2) # -> TRUE: Die Jungen waren erfolgreicher als die M‰dchne.
  
  # Gesamt
  jungen_total <- jungen_woche_1 + jungen_woche_2
  maedchen_total <- maedchen_woche_1 + maedchen_woche_2  
  sum(jungen_total) > sum(maedchen_total) # -> TRUE: Die Jungen waren erfolgreicher als die M‰dchen.  
  