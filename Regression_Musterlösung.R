####################################################################################################################

#### MUSTERLÖSUNG ####

####################################################################################################################
# Vorbereitungen

# Pfad zum Workspace setzen
# Passe den Pfad so an, dass du den Datensatz auf deinem Rechner laden kannst.
setwd("C:/Users/Clarissa/Downloads")

# Pakete einbinden
library(tidyverse)
library(readr)

# Datensatz laden
income_happiness <- read_csv('income_data.csv')

####################################################################################################################
# Explorative Analyse

# Der Datensatz enthält fiktive Werte zum Jahreseinkommen einer Person und ihrer Lebenszufriedenheit. 
# Das Jahreseinkommen nimmt Werte zwischen 15.000€ und 75.000€ an, wobei bei der Kodierung eine 
# Anpassung (geteilt durch 10.000) vorgenommen wurde. Ein Wert von 2 in den Daten entspricht also 
# einem Jahreseinkommen von 20.000€. Die Lebenszufriedenheit wurde mittels einer gut validierten 
# 10-stufigen Skala erhoben. Der Wert 1 entspricht einer sehr geringen Lebenszufriedenheit und 10 
# einer maximal hohen Zufriedenheit.

# Mach dich mit dem Datensatz vertraut. 
view(income_happiness)
summary(income_happiness)


####################################################################################################################
# Aufgabe 1: Lineare Regression durchführen [0,5 Punkte] 

# Führe mit den Daten eine lineare Regression durch. Uns interessiert, ob eine lineare Beziehung 
# zwischen dem Einkommen und der Lebenszufriedenheit besteht. Wir erwarten, dass ein höheres 
# Einkommen die Lebenszufriedenheit steigern kann.
# Gib das Ergebnis in der Konsole aus. 

income.happiness.lm <- lm(happiness ~ income, data = income_happiness)
summary(income.happiness.lm)

####################################################################################################################
# Aufgabe 2: Annahmen prüfen

# Um das Ergebnis einer linearen Regression interpretieren zu dürfen, müssen die Regressionsannahmen 
# erfüllt sein. Dies prüfen wir schrittweise.

# 1. Die Beziehung zwischen den unabhängigen Variablen und der abhängigen Variable ist linear.
# Prüfe diese Annahme, indem du ein Streudiagramm ausgibst. Beschreibe knapp, was du siehst und  
# bewerte, ob die Annahme erfüllt ist. Nutze hierfür die print-Funktion. [1 Punkt]
income_scatter.graph <- ggplot(income_happiness, aes(x=income, y=happiness))+
  geom_point()
income_scatter.graph
print("Im Diagramm sieht man, dass die Datenpunkte entlang einer Gerade liegen. 
      Wir können daher annehmen, dass ein linearer Zusammenhang zwischen der unabhängigen
      und abhängigen Variable besteht.")

# 2. Die Daten wurden mittels Zufallsstichprobe aus der Grundgesamtheit gezogen.
# Diese Annahme kannst du aufgrund des Untersuchungsdesigns als erfüllt ansehen.

# 3. Die unabhängigen Variablen weisen keine lineare Beziehung auf.
# Da es sich um eine bivariate Regression handelt und nur eine unabhängige Variable vorhanden ist,
# muss diese Bedingung nicht geprüft werden.

# 4. Exogenität: Der erwartete Wert des Fehlers ist 0.
# Diese Annahme kannst du aufgrund des Untersuchungsdesigns als erfüllt ansehen.

# 5. Homoskedastizität: Die Varianz des Fehlerwertes ist für alle Werte der erklärenden 
#    Variablen gleich.
# Erstelle eine Grafik, auf der die Residuen und gefitteten Werte abgebildet sind. 
# Beschreibe knapp, was du siehst und bewerte, ob die Annahme erfüllt ist. 
# Nutze hierfür die print-Funktion. [1 Punkt]
par(mfrow=c(2,2))
plot(income.happiness.lm)
par(mfrow=c(1,1))
print("Die rote Linie, die den Mittelwert der Residuen angibt, ist nahe null und verläuft horizontal. Wir müssen also keine Ausreißer oder systematischen Verzerrungen annehmen. Homoskedastizität liegt vor.")

####################################################################################################################
# Aufgabe 3: Ergebnisse präsentieren

# 3a) Interpretation [1,5 Punkte]  
# Interpretiere die Ergebnisse der Regressionsanalyse, so wie du es im Seminar gelernt hast.
# Nutze die Textbausteine und passe sie an die Ergebnisse dieser Analyse an.
# Gib deine Textantwort mittels print() in der Konsole aus.                                
print("Eine einfache lineare Regression mit der Zufriedenheit als der abhängigen und dem Einkommen als der erklärenden Variable ist signifikant, F (1,496) = 1483, p < ,001.")
print("74,93% der Varianz der Zufriedenheit kann mit der Variable Einkommen erklärt werden. Der Regressionskoeffizient der Variable Einkommen ist 0,714 und ist signifikant (t (496) = 38,505; p < ,001).")
print("Das Einkommen ist ein signifikanter Prädiktor für die Lebenszufriedenheit. Pro 10.000€ höherem Verdienst nimmt die Lebenszufriedenheit um 0.714 Punkte zu (b_1 = 0,714; t (496) = 38,505 p < ,001).")

# 3b) Grafische Aufbereitung [0,5 Punkte]
# Zur Ergebnispräsentation gehört zudem eine Grafik, in der die Datenpunkte und die Regressions-
# gerade eingetragen sind. Gib diese ebenfalls aus.
income.graph <- ggplot(income_happiness, aes(x=income, y=happiness))+
  geom_point() +
  geom_smooth(method="lm", col="black")
income.graph

# 3c) Verständnis
# Können wir mit dem Modell vorhersagen, wie hoch die Lebenszufriedenheit einer Person ist, 
# die jedes Jahr eine Million Euro Verdient? Weshalb bzw. weshalb nicht?
# Gib deine Antwort mit print aus. [0,5 Punkte]
print("Eine Person, die Einkommensmillionär ist, würde nach dem Modell eine höhere Lebenszufriedenheit erreichen, als der maximale Lebenszufriedenheitswert der Skala zulässt. Das Modell ist also nur in einem kleinen Wertebereich für die Vorhersage von Lebenszufriedenheit geeignet und kann entsprechend nicht für Einkommensmillionäre genutzt werdens.")

####################################################################################################################
