
# hier sind die Daten für den Abstand vom Ziel, für Wurfarm und Alternative
wurfarm <- c(10, 8, 10, 0, 17)
alternative <- c(3, 23, 5, 7, 13)

# zeichne Boxplots für die Daten
boxplot(wurfarm, alternative, main="Wurfexperiment", names=c("Wurfarm","Alternativarm"), ylab="Entfernung [cm]")

# optional: zeichne die Punkte in den Boxplot ein
# Schritt 1: bestimme die X-Y-Koordinaten der Punkte. 
# Die X-Koordinaten sind 1 (Wurfarm) und 2 (Alternative),
# die Y-Koordinaten sind wurfarm und alternative.
# Damit die Punkte nicht übereinander liegen, werden die X-Koordinaten verwackelt
wxy=xy.coords(jitter(rep(1,length(wurfarm))),wurfarm)
axy=xy.coords(jitter(rep(2,length(alternative))),alternative)

# Schritt 2:jetzt die Punkte ausgeben
points(wxy, pch=15)
points(axy, pch=15)

# Standardabweichung und Übersicht ausrechnen
sd(wurfarm)
summary(wurfarm)
sd(alternative)
summary(alternative)

#statistische Tests
t.test(wurfarm, alternative, alt="less")
wilcox.test(wurfarm, alternative, alt="less")
