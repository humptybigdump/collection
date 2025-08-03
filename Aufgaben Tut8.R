library(sjmisc)
library(dplyr)
library(DescTools)

# Datensatz importieren
library(readxl)
Fische <- read_excel("Datensatz_Fische.xlsx") # Text in der Klammer durch Speicherort ersetzen (Dateipfad)

# A) Zusammenhang Farbe und Geschlecht?
  # Häufigkeitstabllen
  frq(Fische$Geschlecht)
  frq(Fische$Farbe)

  # Kreuztabelle
  geschlecht_x_farbe <- table(Fische$Geschlecht, Fische$Farbe)
  print(geschlecht_x_farbe)  
  # Spaltenprozente
  prop.table(geschlecht_x_farbe, 2)
    # Inter: blau - grün: 0 %; blau - rot: 31 % -> rote Fische sind häufiger inter als blaue oder grüne.
    # Männlich: blau - grün: 55 % -> grüne Fische sind häufiger männlich als blaue; blau - rot: 18 % -> rote Fische sind häufiger männlich als blaue; grün - rot: 37 % -> grüne Fische sind häufiger männlich als rote.
    # Weiblich: blau - grün: 55% -> blaue Fische sind häufiger weiblich als grüne; blau - rot: 49 % -> blaue Fische sind häufiger weiblich als rote; grüne - rot: 6% -> rote Fische sind häufiger weiblich als grüne.
  # Zeilenprozente
  prop.table(geschlecht_x_farbe, 1)
    # Blau: inter-männlich: 23 % -> Intersex. Fische sind häufiger blau als männliche; inter - weiblich: 40 % -> weibliche Fische sind häufier blau als intersexuelle; männlich - weiblich: 63 % -> weibliche Fische sind häufiger blau als männliche.
    # Grün: inter - männlich: 60 % -> männliche Fische sind häufiger grün als intersexuelle; inter - weiblich: 0 %; männlich - weiblich: 60 % -> männliche Fische sind häufiger grün als weibliche.
    # Rot: inter - männlich: 37 % -> Intersex. Fische sind häufiger rot als männliche; inter - weiblich: 40 % -> intersex. Fische sind häufiger rot als weibliche; männlich - weiblich: 3 %
  # -> Wir vermuten insgesamt überwiegend Zusammenhänge zwischen Farbe und Geschlecht.

# B) siehe Word-Datei

# C) Chi2-Test
  # Vorannahmen
    # unabhängig? -> Ja, versch. Fische.
    # Erwartete Häufigkeiten > 5? -> Ja, siehe Indifferenztabelle
  chisq.test(Fische$Geschlecht, Fische$Farbe, correct = FALSE)
  # ->  p-value < 0,05 -> H0 ablehnen und davon ausgehen, dass es einen Zusammenhang gibt. 
  
# D) Cramérs V
  CramerV(x = Fische$Farbe, y = Fische$Schwanzflosse)
  # -> Cramérs V = 0.39: Es gibt einen mittelstarken Zusammenhang zwischen  der Farbe und Form der Schwanzflosse der Fische.  