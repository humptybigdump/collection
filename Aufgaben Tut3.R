# Aufgabe 1:
    
    # a) Datensatz erstellen
    name <- c("Philipp", "Hannes", "Mirko", "Elena", "Jessica", "Noah")
    thema <- c("Podcasts", "Tweets", "TikTok", "Streaming", "Fernsehen", "Blogs")
    seiten <- c(12, 17, 15, 15, 14, 13)
    note <- c(4.0, 2.3, 1.0, 2.7, 1.3, 2.0)
    bestanden <- c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE)
    
    hausarbeiten <- data.frame(name, thema, seiten, note, bestanden)
    
    # b) Erste Beobachtungen und Struktur
    head(hausarbeiten)
    str(hausarbeiten)   
    
    # c) Thema Mirko
    hausarbeiten[3, "thema"]
    # -> TikTok
    
    # d) Unterschreiten der Seitenzahl
    subset(hausarbeiten, subset = seiten < 15)
    # -> Philipp, Jessica, Noah

    # Für Schnelle: Student*innen, die Seminar bestanden haben
    bestanden_vektor <- hausarbeiten$bestanden
    hausarbeiten[bestanden_vektor, ]
    
    subset(hausarbeiten, subset = bestanden == TRUE)
    
# Aufgabe 2:

  # a) Vektor Medienbeobachtung
  sendungen <- c("Quarks", "TVOG", "GNTM", "Sportschau", "heute-Show")
  names(sendungen) <- c("Melanie", "Antonia", "Max", "Maris", "John")
  sendungen

  # b) Faktor Medienbeobachtung
  sendungen_faktor <- factor(sendungen)
  sendungen_faktor
  
  # c) Faktor Bewertungen
  bewertungen <- c("nbe", "be", "be", "be", "nbe")
  bewertungen_faktor <- factor(bewertungen)  
  levels(bewertungen_faktor) <- c("bestanden", "nicht bestanden") 
  bewertungen_faktor
  
  # d) Melanie schneller als Max?
    # Tempo-Faktor
    sprechtempo <- c("mittel", "schnell", "sehr schnell", "langsam", "mittel")
    sprechtempo_faktor <- factor(sprechtempo, ordered = TRUE)
    sprechtempo_faktor
    
    # Melanie
    melanie <- sprechtempo_faktor[4]
    
    # Max
    max <- sprechtempo_faktor[3]
    
    # Melanie > Max?
    melanie > max
    # -> Melanie hat nicht schneller als Max gesprochen
    
