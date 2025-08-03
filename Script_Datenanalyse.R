### Datensatz aus Excel einlesen
## Paket readxl installieren und laden - wird denötigt, damit Excel-Daten eingelesen werden können

install.packages("readxl")

library(readxl)

## Nun legen wir ein Arbeitsverzeichnis fest, d.h. wir benennen einen Ordner, an dem der Datensatz abgelegt ist, den wir einlesen wollen, also unsere Excel-Datei

setwd("C:\\Users\\iv0543\\Documents\\ITZ\\Lehre\\WS 24-25\\Forschungsmodul\\Tutorials Datenanalyse")

## Dateneinlesen. Dazu benennen Sie 

Demo_daten <- read_excel("Datensatz.xlsx")

## Prüfen, ob Daten richtig eingelesen sind
# Schauen, ob die ersten sechs Zeilen im Datensatz stimmen

head(Demo_daten)

# Struktur der Daten anzeigen
str(Demo_daten)

### Daten aufbereiten
## Variablen invertieren (reverse code)
# erforderliches Paket laden, Hilfe anfordern

install.packages("car")
library(car)

# Variablen invertieren
Demo_daten$Einst2_inv<-recode(Demo_daten$Einst2, "1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1")
Demo_daten$Einst4_inv<-recode(Demo_daten$Einst4, "1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1")
Demo_daten$Einst5_inv<-recode(Demo_daten$Einst5, "1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1")

Demo_daten$Mwahrn2_inv<-recode(Demo_daten$Mwahrn2, "1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1")
Demo_daten$Mwahrn4_inv<-recode(Demo_daten$Mwahrn4, "1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1")
Demo_daten$Mwahrn5_inv<-recode(Demo_daten$Mwahrn5, "1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1")

# Variable recodieren
Demo_daten$Einst2_gr<-recode(Demo_daten$Einst2_inv, "1=-1; 2=-1; 3=-1; 4=0; 5=1; 6=1; 7=1")

#zur Kontrolle: Variablen anzeigen lassen

Demo_daten$Einst2_inv
Demo_daten$Einst2

Demo_daten$Einst4_inv
Demo_daten$Einst4

Demo_daten$Einst5_inv
Demo_daten$Einst5

## neue Variable berechnen
# Geburtsjahr (Alter) umrechnen in Alter
Demo_daten$Alter<-2024-Demo_daten$Jahrgang 

#zur Kontrolle: Variablen anzeigen lassen
Demo_daten$Jahrgang
Demo_daten$Alter

## fehlende Werte
# Wert 9 als fehlenden Wert codieren

Demo_daten$Einst1[Demo_daten$Einst1==9] <- NA

Demo_daten$Einst2[Demo_daten$Einst2==9] <- NA

Demo_daten$Einst3[Demo_daten$Einst3==9] <- NA

Demo_daten$Einst4[Demo_daten$Einst4==9] <- NA

Demo_daten$Einst5[Demo_daten$Einst5==9] <- NA

Demo_daten$Mwahrn1[Demo_daten$Mwahrn1==9] <- NA

Demo_daten$Mwahrn2[Demo_daten$Mwahrn2==9] <- NA

Demo_daten$Mwahrn3[Demo_daten$Mwahrn3==9] <- NA

Demo_daten$Mwahrn4[Demo_daten$Mwahrn4==9] <- NA

Demo_daten$Mwahrn5[Demo_daten$Mwahrn5==9] <- NA

Demo_daten$Einst2_inv[Demo_daten$Einst2_inv==9] <- NA

Demo_daten$Einst4_inv[Demo_daten$Einst4_inv==9] <- NA

Demo_daten$Einst5_inv[Demo_daten$Einst5_inv==9] <- NA

## Wertelabels
# Sind Variablen Faktoren oder numerisch?
table(Demo_daten$Sonntagsfr)
class(Demo_daten$Sonntagsfr)
 str(Demo_daten)
 
#Sonntagsfrage, Geschlecht und Bildung in Faktoren (Variablen, deren Werte statt Zahlenwerte Labels annehmen können) umwandeln
 Demo_daten$Sonntagsfr<-factor(Demo_daten$Sonntagsfr)
 Demo_daten$Geschlecht<-factor(Demo_daten$Geschlecht)
 
 str(Demo_daten)
 
 Demo_daten$Sonntagsfr<-factor(Demo_daten$Sonntagsfr, levels = c(1,2,3,4,5,6,7,8,9,99),
                               labels = c("CDU/CSU", "SPD", "Grüne", "AfD", 
                                          "FDP", "die LINKE", "BSW", "andere", 
                                          "Nichtwähler", "kkA"))
 
 Demo_daten$Geschlecht<-factor(Demo_daten$Geschlecht, levels = c(1,2,3),
                               labels=c("weiblich", "männlich", "divers"))
 
 
 Demo_daten$Schulabschluss<-factor(Demo_daten$Schulabschluss, ordered = TRUE)
 
 str(Demo_daten)
 
 Demo_daten$Schulabschluss<-factor(Demo_daten$Schulabschluss, ordered = TRUE, levels = c(1,2,3,4),
                            labels=c("ohne", "Hauptschule", "Realschule", "Gymnasium"))
 
 table(Demo_daten$Schulabschluss)
 table(Demo_daten$Geschlecht)
 table(Demo_daten$Sonntagsfr)
 
 ## Labels für Variablen
 # Packages runterladen
 install.packages("Hmisc")
library(Hmisc)
 
 label(Demo_daten$Einst1)<-"Erhaltungszustand"
 label(Demo_daten$Einst2)<-"Gefahr"
 label(Demo_daten$Einst3)<-"Schutz"
 label(Demo_daten$Einst4)<-"Bestandsregulierung"
 label(Demo_daten$Einst5)<-"Bejagung"
 
 label(Demo_daten$emotion_1)<-"runterzogen"
 label(Demo_daten$emotion_2)<-"dankbar"
 label(Demo_daten$emotion_3)<-"geängstigt"
 label(Demo_daten$emotion_4)<-"verunsichert"
 label(Demo_daten$emotion_5)<-"Hoffnung"
 label(Demo_daten$emotion_6)<-"wütend"
 label(Demo_daten$emotion_7)<-"Orientierung"
 label(Demo_daten$emotion_8)<-"froh"
 label(Demo_daten$emotion_9)<-"Überfordert"
 label(Demo_daten$emotion_10)<-"hilflos"
 label(Demo_daten$emotion_11)<-"verwirrt"
 
 #### Grundauswertung (Exploration, Kontrolle)
 ## Häufigkeitsverteilungen
 # Tabellen in Rohform mit r
 
 table(Demo_daten$Einst1)
 table(Demo_daten$Einst2)
 table(Demo_daten$Einst3)
 
 
 # zielführendere Tabellen bekommen wir mit speziellen Packages
 # zunächst sollten Sie sich für jede Variable Häufigkeitsverteilungen anschauen (und für den Anhang aufbereiten)
 # sehr hilfreich finde ich die Häufigkeitstabellen aus dem Paket "expss" - ein Paket, das besonders nutzerfreundlich
 # für ehemalige Anwender von Excel und Spss ist
 # installieren und laden und expss
 
 install.packages("expss")
 library(expss)
 

 # für Häufigkeitsverteilungen nutzen wir in expss die Funktion fre
# für einzelne Variablen
 

 fre(Demo_daten$Einst2)
 fre(Demo_daten$Geschlecht)
 
# für mehrere Variablen auf einen Schlag (empfehle ich nur bedingt)

 fre(list(Demo_daten$Einst2, Demo_daten$Einst3))
 
 # so erhalten Sie Hilfen zur Funktion fre

 ?fre
 
 ## Mittelwerte und Streuungsmaße
 # Standfunktion in r "summary" (Nachteil: keine Streuungsmaße)
 # für alle variablen auf einmal (macht aber für kategoriale Variablen keinen Sinn, es gibt schließlich i.d.R heute kein durchschnittliches Geschlecht)
 
 summary(Demo_daten)
 
# für eine Variable (so können Sie nach und nach Variable für Variable ansehen)
  summary(Demo_daten$Einst1)
 
# für bestimmte Variablen im Datensatz (zum Beispiel alle Items einer Batterie in einem Rutsch berechnet)
 summary(Demo_daten[1:5])

# Nachteil der summary-funktion: gibt keine Streuungsmaße wie Varianz oder Standardabweichung an, 
# Varianz und Standardabweichung sollten Sie aber immer angeben, deshalb nutzen wir das Paket Psych
# Psych installieren und laden
 
 install.packages("psych")
 library(psych)
 
# In Psych gibt es die Funktion "describe", die uns artithmetisches Mittel und Standardabweichung u.v.a.m. angibt
# Describe für eine Variable
 
 describe(Demo_daten$Einst1)
 
# describe gibt auch Werte für die Verteilung an, z. B. ist die Verteilung schief.
# Wir lassen das an dieser Stelle außer Acht und programmieren, dass die Daten zur Schiefe (skew) nicht weiter ausgegegeben werden.
 describe(Demo_daten$Einst1, skew = FALSE)
 
# So kann man programmieren, dass man Mittelwert etc. nur für die Variablen 20 bis 25 bekommt.
 describe(Demo_daten[1:5], skew = FALSE)
 
### Vorbereitung der Hauptanalysen
 ##Korrelationen zwischen Items
 # Neues Paket installieren und laden, mit dem man sofort Korrelationstabellen als Word-Dok erstellen kann
 
 install.packages("apaTables")
 library(apaTables)
 
 # Wir fassen jetzt zunächst alle Variablen, die wir korrelieren wollen, in ein Subset
 # Pearson's Korrelationskoeeffizient r kann Werte zwischn -1 und 1 annehmen. 0 bedeutet: kein Zusammenhang, 
 # 1 bedeutet: perfekter positiver Zusammenhang, -1 perfekt negativer Zusammenhang
 
 Einst <- subset(Demo_daten, select=c(Einst1, Einst2_inv, Einst3, Einst4_inv, Einst5_inv))
 Emos <-subset(Demo_daten, select=c(emotion_1, emotion_2, emotion_3, emotion_4, emotion_5, emotion_6,
                          emotion_7, emotion_8, emotion_9, emotion_10, emotion_11))
 MedW <- subset(Demo_daten, select=c(Mwahrn1, Mwahrn2_inv, Mwahrn3, Mwahrn4_inv, Mwahrn5_inv))
 
 # wir können uns Korrelationen mit einer ganz grundlegenden Funktion ausgeben lassen, allerdings ohne 
 #Hinweis auf statistische Signifikanz
 cor(Einst)
 cor(Einst, use="complete.obs")
 cor(Emos, use="complete.obs")
 
 # mit dem neuen Paket können wir uns direkt eine Korrelationstabelle als Word-Dok erstellen lassen!
 apa.cor.table(Einst, filename = "Einstellung.doc")
 apa.cor.table(Emos, filename = "Medienwahrnehmungen.doc")
 apa.cor.table(Emos, filename = "Emotionen.doc")
 
 ## Faktorenanalyse
 install.packages("psy")
 library(psy)
 
 install.packages("nFactors")
 library(nFactors)
 
#wir müssen zunächst alle Fälle (=Zeilen im Datensatz) für diese Analyse ausschließen, bei denen ein Teilnehmer nicht geantwortet hat (missings, NA)
# Für die Faktorenanalyse ist verlangt, dass jeder Fall (=jede Zeile, die verwendet wird) für alle Items Datenvorliegen hat

 Einst_comp <- Einst[complete.cases(Einst), ]
 Emos_comp <- Emos[complete.cases(Emos), ]
 MedW_comp <-MedW[complete.cases(MedW), ]
 
 head(Einst_comp)
 head(Emos_comp)
 head (MedW_comp)
 
 
 
 #Voraussetzungen prüfen: korrelieren die Items genug miteinander? Dazu wenden wir den Bartlett-Test an
 cortest.bartlett(Einst_comp)
 cortest.bartlett(Emos_comp)
 cortest.bartlett(MedW_comp)
 
 #Voraussetzungen prüfen: Kaiser-Meyer Olkin Kriterium (soll Werte über 0,5 annehmen)
 KMO(Einst_comp)
 KMO(Emos_comp)
 KMO(MedW_comp)
 
 # Extraktion von Faktoren mit MAP-Test 
 # MAP-Test gibt Orientierung zur Frage, wie viele Faktoren  gebildet werden sollten bzw. sinnvoll aus den Daten extrahiert werden können
 # Funktion nfactors mit Varimax-Rotation und Maximum-Likelihood-Estimation
 # Wir interpretieren den Output als ersten Anhaltspunkt dafür, wie viele Faktoren wir extrahieren sollten
 
 nfactors(Einst_comp, rotate="varimax", fm="mle")
 nfactors(MedW_comp, rotate="varimax", fm="mle")
 nfactors(Emos_comp, rotate="varimax", fm="mle")
 
 # weitere Hinweise auf die Frage, wie viele Faktoren sinnvoller Weise gebildet werden sollten
 # sinnvoller Weise heißt hier: was wird statistisch den Daten gerecht (NICHT: was ist theoretisch sinnvoll)
 
 scree(Einst_comp)
 scree(Emos_comp)
 scree(MedW_comp)
 
 # ML Faktorenanalys
 # 1 (Einstellungen) und 2 (Emotionen) Faktoren
 # Varimax Rotation
 
 FA1 <- factanal(Einst_comp, 1, rotation="varimax")
 print(FA1, digits=2, cutoff=.3)
 
 
 FA2 <- factanal(Emos_comp, 3, rotation="varimax")
 print(FA2, digits=2, cutoff=.3)
 
 FA3 <-factanal(MedW_comp, 1, rotation="varimax")
 print(FA3, digits=2, cutoff=.3)
 

 # Berechnung der Kommunalitäten (daraus kann man sehen, wie viel Varianz der einzelnen Items durch die Faktoren erklärt wird)
 1-FA1$uniquenesses
 1-FA2$uniquenesses
 1-FA3$uniquenesses
 
 # Faktorenanalyse für die Emotionen OHNE Emotion 7 ("Orientierung") - alle Schritte von oben noch mal durchgeführt
 
 EmoswO <-subset(Demo_daten, select=c(emotion_1, emotion_2, emotion_3, emotion_4, emotion_5, emotion_6,
                                    emotion_8, emotion_9, emotion_10, emotion_11))
 
 EmoswO_comp <- EmoswO[complete.cases(Emos), ]
 
 cortest.bartlett(EmoswO_comp)
 
 KMO(EmoswO_comp)
 
 nfactors(EmoswO_comp, rotate="varimax", fm="mle")
 
 scree(EmoswO_comp)
 
 FA4 <- factanal(EmoswO_comp, 3, rotation="varimax")
 print(FA4, digits=2, cutoff=.3)
 
 ## Reliabilitätsanalyse und Skalenerstellung
 # Cronbachs Alpha: testet, ob Items, die eine Skala bzw. ein Konstrukt abbilden intern konsistent sind
 # Alpha nimmt werte zwischen 0 und 1 an, Werte >= 0,70 sein gelten als reliabel
 
 libraby(psych)
 
 alpha(Einst)
 
 alpha(subset(Demo_daten, select=c(Einst1, Einst2_inv, Einst3, Einst4_inv, Einst5_inv)))
       
 alpha(subset(Emos, select=c(emotion_1, emotion_3, emotion_4, emotion_6, emotion_9, emotion_10, emotion_11)))
 
 alpha(MedW)
 
 ## Erstellen der neuen Variablen (Skalen) nach der Reliablitätsanalyse
 # Mittelwert-Score - wir bilden den Mittelwert über alle Items, die Sie nach der Faktoren und Reli-Analyse zusammenfassen wollen
 
Demo_daten$Einst_Skala<-rowMeans(subset(Demo_daten, select=c(Einst1, Einst2_inv, Einst3, Einst4_inv, Einst5_inv)), na.rm = TRUE)

Demo_daten$Emos_Skala<-rowMeans(subset(Demo_daten, select=c(emotion_1, emotion_3, emotion_4, emotion_6, emotion_9, emotion_10, emotion_11)))

Demo_daten$MWahrn_Skala<-rowMeans(subset(Demo_daten, select=c(Mwahrn1, Mwahrn2_inv, Mwahrn3, Mwahrn4_inv, Mwahrn5_inv)), na.rm = TRUE)
 
### letzter SChritt: Prüfen von Zusammenhängen zwischen neu erstellen Variablen/Skalen
# neues Subset erstellen mit Variablen, deren Zusammenhänge geprüft werden sollen

Skalen <- subset(Demo_daten, select=c(MWahrn_Skala, Emos_Skala, Einst_Skala))


# mit dem neuen Paket können wir uns direkt eine Korrelationstabelle als Word-Dok erstellen lassen!
apa.cor.table(Skalen, filename = "Skalen.doc")

#lineare Regression

Regr<-lm(MWahrn_Skala~Einst_Skala+Emos_Skala+Alter, data=Demo_daten)
print(Regr)

plot(Demo_daten$Einst_Skala, Demo_daten$MWahrn_Skala)

plot(Regr, 2)

plot(Regr, 1)

library(car)
vif(Regr)

plot(Regr, 4)
