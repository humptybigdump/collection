# Lösungen zu den Coding-Sessions der Einführungsveranstaltung des R-Praktikums
# Übungsleiter: Fabius Ott


########################################################################################
# GRUNDLAGEN:
# Erstelle einen Vektor mit den Zahlen von 1 bis 100 auf zwei verschiedene Arten.
vec <- 1:100
vec2 <- seq(1,100)

# Lasse dir die ersten fünf Elemente anzeigen.
vec[1:5]

# Lasse dir das zweite, das fünfzigste und das vorletzte Element anzeigen.
vec[c(2,50,length(vec)-1)]

# Ermittle die Summe der Vektor-Einträge auf zwei verschiedene Arten (Tipp: Gaußsche Summenformel)
sum(vec)
n <- 100
summe <- (n*(n+1))/2
summe

# Fülle eine Matrix mit 10 Zeilen mit den Einträgen des Vektors.
matr <- matrix(vec, nrow = 10)

# Addiere die erste und die letzte Spalte der Matrix und gebe das Ergebnis aus.
erg <- matr[,1] + matr[,ncol(matr)]
erg


########################################################################################
# DESKRIPTIVE STATISTIK:

# Realisiere eine Zufallsstichprobe von 100 standard-normalverteilten Zufallsvariablen.
# Stelle hierbei die Reproduzierbarkeit sicher.
set.seed(42)
zsp <- rnorm(100)

# Berechne den Interquartilsabstand dieser Stichprobe (QA = Q3-Q1).
qa <- quantile(zsp, 0.75) - quantile(zsp, 0.25)
names(qa) <- "QA"
stats::IQR(zsp)  # Alternativlösung


########################################################################################
# PLOTS:

# Plotte die zuvor erstellte Zufallsstichprobe als Boxplot.
graphics::boxplot(zsp, notch = TRUE)

# Plotte die Verteilung der Stichprobe als Histogramm mit 5 Klassen.
# Gebe dem Plot eine sinnvolle Überschift.
# (Tipp: Suche mit der Help-Funktion nach den passenden Parametern)
hist(zsp, breaks = 5, main = "Histogramm der Zufallsstichprobe")

# Plotte nun beide Plotts nebeneinander in eine Grafik. (Tipp: Nutze die par() Funktion)
# Gebe beiden Plots sinnvolle Überschriften.
par(mfrow=c(1,2))
boxplot(zsp, notch = TRUE, main = "Boxplot")
hist(zsp, breaks = 5, main = "Histogramm")


########################################################################################
# LOGISCHE OPERATOREN:

# Aufgabe User Authentication:
# Erstellen Sie eine Variable "passwort" und weisen Sie ihr einen Wert zu.
passwort <- 'Statistikk'

# Erstellen Sie eine Variable "passwort_correct" und weisen Sie ihr einen Wert zu.
passwort_correct <- 'Statistik'

# Schreiben Sie einen if-Block, der überprüft, ob das Passwort korrekt sind. 
# Verwenden Sie zusätzlich eine Begrenzung für die Anzahl der Versuche, um eine Brute-Force-Attacke abzuwenden.
# Geben Sie eine entsprechende Nachricht aus, ob die Authentifizierung erfolgreich war oder nicht 
# und informieren Sie über die Anzahl der Versuche, bevor der Zugriff gesperrt wird.
# Tipp: Für Konsolenausgaben, welche eine Information für den Programmierer darstellen,
# kann auch die Funktion message() verwendet werden.
counter <- 0
max_counter <- 5

if((passwort_correct == passwort) & counter <= max_counter) {
  message('Passwort korrekt!')
} else if ((passwort_correct != passwort) & (counter+1 < max_counter)){
  message('Passwort falsch!')
  counter <-  counter + 1
  tries_left <- max_counter - counter
  message('Offene Versuche: ', tries_left)
} else{
  message('Passwort falsch!')
  message('Keine Versuche mehr offen!')
}


########################################################################################
# FUNKTIONEN:

# Schreibe eine Funktion mit den Parametern x & n, welche das Ergebnis x^n zurückgibt.
# Falls kein n angegeben ist, soll standardmäßig x quardiert werden.
# Benutze eine for-Schleife für die Lösung.
x_hoch_n <- function(x, n = 2) {
  result <- 1
  for(i in 1:n) {
    result <- result * x 
  }
  return(result)
}
