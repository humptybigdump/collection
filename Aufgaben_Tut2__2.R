# Aufgabe 1: Variablen erstellen
  # a) Erstelle je eine numerische, charakterliche und logische Variable.
    numerisch <- 4
    charakter <- "rot"
    logisch <- TRUE
  # b) Überprüfe die Datentypen.
    class(numerisch) # L: numeric
    class(charakter) # L: character
    class(logisch) # L: logical
  
# Aufgabe 2: Vektoren
  # a) Vektor für die Gewinne und Verluste
    lotto_w1 <- c(5, -1.2, -3.6, 10.8)
  # b) Vektor nach Spieler*innen umbenennen
    names(lotto_w1) <- c("Paula", "Leon", "Marie", "Moritz")
    # Option 2:
    namen <- c("Paula", "Leon", "Marie", "Moritz")
    names(lotto_w1) <- namen
    lotto_w1
  # c) Vektor Folgewoche
    lotto_w2 <- c(-1.2, 4.6, 0.8, 1.2)
    names(lotto_w2) <- c("Paula", "Leon", "Marie", "Moritz")
  # d) Gesamt- und Durchschnittsgewinne + Vergleich der Wochen
    # Gesamtgewinn Woche 1
    sum(lotto_w1) # L: 11 Euro
    # Gesamtgewinn Woche 2
    sum(lotto_w2) # L: 5,40 Euro
    # Woche 1 erfolgreicher als Woche 2?
    sum(lotto_w1) > sum(lotto_w2) # L: TRUE
    # durchschnittlicher Gewinn Woche 1
    mean(lotto_w1) # L: 2,75 Euro
    # durchschnittlicher Gewinn Woche 2
    mean(lotto_w2) # L: 1,35 Euro
  # e) Auswahlvektoren mit Gewinnen und Verlusten
    # Summe der beiden Wochen
    gesamt <- lotto_w1 + lotto_w2
    # Gewinne
    gewinne <- gesamt > 0
    gewinne # L: Paula, Leon, Moritz
    verluste <- gesamt < 0
    verluste # L: Marie
  # Für Schnelle: Vergleich Jungen und Mädchen
    # Woche 1
    jungen_w1 <- lotto_w1[c("Leon", "Moritz")]
    maedchen_w1 <- lotto_w1[c("Paula", "Marie")]   
    sum(jungen_w1) > sum(maedchen_w1) # L: TRUE
    # Woche 2
    jungen_w2 <- lotto_w2[c("Leon", "Moritz")]
    maedchen_w2 <- lotto_w2[c("Paula", "Marie")]    
    sum(jungen_w2) > sum(maedchen_w2) # L: TRUE
    # beide Wochen
    jungen <- jungen_w1 + jungen_w2
    maedchen <- maedchen_w1 + maedchen_w2    
    sum(jungen) > sum(maedchen) # L: TRUE
    