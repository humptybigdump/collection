# Aufgabe 1: Datensätze
  # a) Erstelle einen Datensatz aus den angegebenen Daten.
    # Namen: Philipp, Hannes, Mirko, Elena, Jessica, Noah
    # Themen: Podcasts, Tweets, TikTok, Streamingdienste, Fernsehen, Blogs
    # Seitenanzahl: 12, 17, 15, 15, 14, 13
    # Noten: 4.0, 2.3, 1.0, 2.7, 1.3, 2.0
    # Bestanden: TRUE, TRUE, TRUE, FALSE, TRUE, TRUE
  # Variablen erstellen
  name <- c("Philipp", "Hannes", "Mirko", "Elena", "Jessica", "Noah")
  topic <- c("Podcasts", "Tweets", "TikTok", "Streaming", "Fernsehen", "Blogs")
  pages <- c(12, 17, 15, 15, 14, 13)
  grade <- c(4.0, 2.3, 1.0, 2.7, 1.3, 2.0)
  status <- c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE)
  # Datensatz erstellen
  theses <- data.frame(name, topic, pages, grade, status)
  
  # b) Überprüfe deinen Datensatz, indem du dir die ersten Beobachtungen und die Struktur ausgeben lässt
  head(theses)
  str(theses)
  
  # c) Welches Thema hat Mirko bearbeitet?
  theses[3, "topic"] # L: TikTok
  
  # d) Welche Student*innen haben Hausarbeiten < 15 Seiten geschrieben?
  subset(theses, subset = pages < 15) # L: Philipp, Jessica, Noah
  
  # Für Schnelle: Anzeige der Student*innen, die das Seminar bestanden haben.
  subset(theses, subset = status == TRUE) # L: Philipp, Hannes, Mirko, Jessica, Noah
  
# Aufgabe 2: Faktoren
  # a) Benannte Vektoren erstellen
  tvshow <- c("Quarks", "TVOG", "GNTM", "Sportschau", "heute-Show")
  names(tvshow) <- c("Melanie", "Antonia", "Max", "Maris", "John")
  
  # b) Vektor in Faktor umwandeln
  tvshow_fact <- factor(tvshow)
  
  # c) Status-Faktor
  semstatus <- c("nbe", "be", "be", "be", "nbe")
  names(semstatus) <- c("Melanie", "Antonia", "Max", "Maris", "John")  
  semstatus_fact <- factor(semstatus)
  levels(semstatus_fact) <- c("bestanden", "nicht bestanden")
  
  # d) Sprechtempo-Faktor
  speed <- c("mittel", "schnell", "sehr schnell", "langsam", "mittel")
  speed_fact <- factor(speed, ordered = TRUE, levels = c("langsam", "mittel", "schnell", "sehr schnell"))
  melanie <- speed_fact[4]
  max <- speed_fact[3]
  melanie > max # L: Melanie hat nicht schneller gesprochen als Max.
  