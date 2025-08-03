## Um in R-Studio einen Befehl auszuführen positioniert man den Cursor/ den blinkenden Strick in die Zeile, die man
## ausführen möchte und drückt die Tastenkombination strg - Enter
## im Folgenden sind alle Befehle gelistet, um die in Excel oder LibreOffice Calc exportierte Tabelle in R zu laden
## und einige deskriptive Statistiken zu berechnen

## Jede Zeile die mit einem "#" beginnt wird als Kommentar interpretiert und wird von R nicht beachtet - Kommentare
## sind sehr hilfreich, um seinen eigenen Code besser zu verstehen (zB auch wenn man ihn einige Wochen nicht
## mehr angesehe hat) und sollte genutzt werden!

## Tipp: Kopiert euch den Code zweimal, sodass ihr immer auf die ursprüngliche Version zurückgreifen können!


####
#### Deskriptive Statistik
####

## hier wird der Dateipfad gesetzt in dem die Datei Wald.csv gespeichert wurde
## das könnte auf einem Windows-Rechner z.B. sein: "D:/Studium/Daten/". Wenn man den Pfad kopiert unbedingt darauf achten,
## dass die Trennzeichen immer ein "/" sein müssen und nicht ein "\". Auf Windows-Rechnern muss man die Trennzeichen idR
## umdrehen.

setwd("D:/Daten/R_deskriptive_Statistik")

## nun wird die csv bzw. txt-Datei mit dem Befehl "read.table" in die Variable "data" geladen. Wurde die Excel-Datei aus
## Excel exportiert dann heisst sie vermutlich "Wald.txt" dies muss unten dann angepasst werden.
## die weiteren Optionen, die verwendet werden sind "sep" und "header". Mit "sep" wird definiert wie die Einträge
## in der geladenen Datei getrennt sind (in unserem Fall mit tab)
## mit "header=T" wird dem Programm gesagt, dass die Tabelle, die wir laden einen Tabellenkopf hat in dem die Namen
## für jede Spalte enthalten sind

data <- read.table("Wald.csv", sep="\t", header=T)

## die Tabelle ist nun in die Variable "data" geladen, indem wir data eintippen/bestätigen sehen wir was in der 
## Variable gespeichert ist
data

## es gibt zwei Wege um auf einzelne Spalten der Tabelle data zuzugreifen. Entweder mit dem $-Zeichen und dem Namen der Kolumne
## oder mit eckigen Klammern, die an den Variablennamen angehängt werden. Innerhalb der eckigen Klammern wird mit der Zahl vor dem
## Komma auf die Zeile zugegriffen und mit der Zahl nach dem Komma auf die Spalte. Beispiele folgen:

## Zugriff auf ganze Spalten
data$BHD_in_cm
data[,2]

data$Art
data[,1]

## Zugriff auf eine einzelne Zeile
data[5,]

## Zugriff auf einen einzelnen Eintrag (Zeile und Spalte)
data[7,3]

## wir können nur einige Statistiken für unseren Datensatz berechnen:

## statistische Zusammenfassung des ganzen Datensatz bekommt man mit der Funktion summary(). Mit dem Befehl werden die 
## Minimalwert, Maximalwert, Mean, Median sowie das 1. und 3. Quantil für jede Spalte mit Zahlwerten angezeigt. Für
## Spalten mit Klassen (kategorischer Werte) wird aufgelistet wie häufig die einzelnen Klassen vorkommen

summary(data)

## Alternativ können Statistiken auch einzeln berechnet werden, z.B. median und mean oder Standartabweichung und Varianz für 
## eine einzelne Spalte:

mean(data[,2])
median(data[,2])
sd(data[,2])
var(data[,2])

## Die Ergebnisse können auch jeweils in eine Variable gespeichert werden:

mean_col2 <- mean(data[,2])
mean_col2

median_col2 <- median(data[,2])
median_col2

## In unserem Datensatz sind wir eigentlich mehr daran interessiert die deskriptiven Statistiken für die einzelnen Bestände zu 
## berechnen, als für alle Einträge. In der Spalte Bestand ist angegeben zu welchem Bestand (1 oder 2) der jeweilige Eintrag gehört.
## Wir sollen jetzt die Tabelle in zwei Tabellen aufspalten, um Mittelwerte etc. der Bestände zu vergleichen.
## Dafür werden wiederum die eckigen Klammern benutzt um alle Zeilen mit einer bestimmten Eigenschaft nämlich "data$Bestand %in% 1"
## auszuwählen. Übersetzt heisst der Befehl also: Nehme alle Zeilen in der Tabelle heraus für die gilt, dass in der Kolumne Bestand
## eine 1 steht. All diese Einträge werden dann in der Variable data_B1 gespeichert.

data_B1 <- data[data$Bestand %in% 1,]
data_B2 <- data[data$Bestand %in% 2,]

data_B1
data_B2

## Jetzt können die Mittelwerte etc. von den beiden Beständen verglichen werden:

mean(data_B1[,2])
mean(data_B2[,2])

## => der durchschnittliche Durchmesser von Bestand 1 ist größer als der von Bestand 2
## AUFGABE: Vergleichen Sie die Mittelwerte für die Höhe der beiden Bestände und berechnen SIe auch die Mediane für Höhe und Durchmesser (BHD)


## Anfertigen von Plots:


## für eine genauere Beschreibung der Plots und was sie zu bedeuten haben, siehe theoretischer Teil des heutigen Tages
###### Histogram

hist(data_B1[,3])
hist(data_B2[,3])

## sollen die Plots nebeneinander geplottet werden:

# mit dem par(mfrow=c(1,2)) wird R mitgeteilt, dass es die nächste Grafik so plotten soll, dass es eine Zeile mit mit zwei Grafiken gibt
# würde man den Befehl umschreiben z.B. zu: par(mfrow=cc(2,3)) würde R die Krafik so plotten, dass es zwei Zeilen mit je 3 Grafiken (also
## 6 insgesamt) geben würde. Mit dem Befehl ylim=c(0,6) wird angegeben von wo bis wo der y-Achsenwertebereich reichen soll. Also hier von 0 
## bis 6. Für die x-Achse würde der Befehl z.B. xlim=c(0,10) heissen.

par(mfrow=c(1,2))
hist(data_B1[,3], ylim=c(0,6))
hist(data_B2[,3], ylim=c(0,6))

## den Grafikmodus beenden kann man mit dev.off()

dev.off()


##### Scatterplots

## mit Scatterplots kann man den Zusammenhang zwischen zwei Variablen grafisch aufbereiten
## hier z.B. wie sich die Höhe im Vergleich zum Durchmesser verhält:

## alle Daten:

plot(data[,2], data[,3])

## anderes Symbol (pch) und andere Farbe (col):

plot(data[,2], data[,3], pch=3, col="blue")

## zwei PLots für die zwei Bestände:

plot(data_B1[,2], data_B1[,3], pch=4, col="red")
plot(data_B2[,2], data_B2[,3], pch=4, col="red")

## AUFGABE: PLotten Sie die zwei Scatterplots für die beiden Bestände nebeneinander und sorgen Sie dafür, dass die y und x-Achsen
## dieselben Wertebereiche zeigen


##### Boxplots

## mit dem Befehl Boxplot wird ein Boxplot der gewählten Spalten angezeigt. Das besondere am Beispiel ist, dass mit einem Befehl 
## mehrere Boxplots erzeugt werden. Das liegt daran, dass in der eckigen Klammer mehrere Spalten gewählt wurden. Das geht in dem 
## man mit einem Doppelpunkt die Spalten von x bis y wählt. Also hier alle Spalten von 2 bis 3.

boxplot(data[,2:3])

## AUFGABE: PLotten Sie die Boxplots der Spalten 2 und 3 (BHD und Höhe) für die beiden Bestände nebeneinander und sorgen Sie dafür, 
## dass die y und x-Achsen dieselben Wertebereiche zeigen. Dazu sollen die Farben des ersten Plots grün sein ("green") und im zweiten
## blau ("blue")

par(mfrow=c(1,2))
boxplot(data_B1[,2:3], ylim=c(0,60), col="green")
boxplot(data_B2[,2:3], ylim=c(0,60), col="blue")
