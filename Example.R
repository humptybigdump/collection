# Print auf Konsole
'Example Code'
# Variablen
x <- 1
# Graph drucken
plot(c(x,2,3))
# Rechnen und zuweisen
x+1
y <- 3
x + y

x <- c(1,2)
z <- c(3,4)

x + z

# Dataframes, Tabellen

daten <- data.frame(A=c(1,2,3),
                    B=c(4,5,6),
                    C=c(7,8,9))
# Daten auswählen:
daten$A # Spalte A
daten[1,2] # x,y - achse
daten[,"B"]
daten[,c(1,3)] # A und C
daten[c(1,3),] # 1 und 3
daten[c(1,3),3] # von A und C nur Spalte 3 (y-achse)
daten [,-1] # ich will nicht A sehen


# Funktionen

addieren <- function(a,b){
  a+b
}

addieren(1,1)
addieren(b=1,a=2)
# Durchschnitt
mean(c(0,1,2,3,4,5,6,7,8,9,10))
?mean() # API Hilfe zu einer Funktion aufrufen

# Packages
#install.packages("sna") # package installieren
library(tidyverse) # package einbinden

set.seed(1) # random seed setzen für vergleichbare Ergebnisse
# rnorm macht Zufallswert mit Normalverteilung
df <- data.frame(x=rnorm(n=1000),
                 y=rnorm(n=1000),
                 z=rnorm(n=1000))
plot(df)
head(df)

df %>% select("x","y")


# DATAFRAME in detail - Reihennamen

daten <- data.frame(s1=c(1,2,3),
                    s2=c(4,5,6),
                    s3=c(7,8,9), row.names=c("z1","z2","z3"))

s1=c(1,2,3)
s2=c(4,5,6)
s3=c(7,8,9)

as.data.frame(cbind(s1,s2,s3)) # gleiches ergebnis row namen eher ungebräuchlich!


# csv daten

# csv = englisch (semikolon in daten), csv2 = deutsch (Komma in daten)
write.csv2(x=daten,
           file = "Folder//Daten.csv")

read.csv2(file = "Folder//Daten.csv")

daten_falsch <- read.csv(file = "Folder//Daten.csv")


# Excel

library(openxlsx)



daten <- read.xlsx("Folder//Daten.xlsx")

daten[1,2]


daten[1,2] <- NA # NA = nicht angegebene Felder

daten[daten == 99] <- NA # NA in alle Felder mit 99 schreiben

daten$D <- NA

# t-test varianzanalyse

# t-test

df <- data.frame(x=rnorm(n=1000),
                 y=rnorm(n=1000),
                 z=rnorm(n=1000))

### Normaltest nach Shapiro


# shapiro arbeitet mit matrix statt df!!!
shapiro.test(c(t(as.matrix(df[1]))))
# hoher p-wert = Daten normalverteilt
# sollte auch so sein bei rnorm!

# histrogramm plotten
hist((c(t(as.matrix(df[1])))))
# auch in histogram zu sehen
t.test(x = df,
       mu = 0.1) # t-test mit nur 1 stichprobe

# t-test bei unabhängigen stichproben
# 2 gruppen anhand eines merkmals vergleichen

daten <- read.csv(file = "Folder//Daten.csv")

# Hypothese: Unterschiede in c1 (gruppe 1 und 2) auf bezug zu y1? y1 ist abhängige Variable
# y1 ~ c1(1,2)

# daten selektriere
y_g1 <- daten[daten$c1 ==1 ,"y1"] # nur daten mit c1=1 gewählt

y_g2 <- daten[daten$c1 ==2 ,"y1"]

# test durchführen
res <- t.test(x=y_g1, 
              y=y_g2,
              paired=FALSE, # paired = verbundene Stichproben - bei true Welch test
              var.equal=TRUE # Varianzen der Gruppen gleich?
)

str(res)
res$statistic # nur ergebis betrachten


# vorraussetzungen t test
# normalitätsverteilung
# varianzen der gruppen gleich (homogen) [varianzhomogenität]

# normalität mit shapiro - am robustesten
# nullhypothese - normalverteilt p<0.05 - daten nicht normalverteilt
# MANN WHITNEY U test ist nicht parametrische alternative zum t-test im fall p<0.05

shapiro.test(y_g1)


# varianzhomogenität mit levene test

library("car")

lev_data <- daten[daten$c1==1 | daten$c1==2 ,c("y1","c1")]

leveneTest(y = lev_data$y1,
           group =lev_data$c1)

# p > 0.05 = Varianzhomogenität gegegeben
# ist p <= 0.05 var.equal=FALSE setzen in t.test() => Welch t-test
#res <- t.test(x=y_g1, 
#              y=y_g2,
#              paired=FALSE, # paired = verbundene Stichproben - bei true Welch test
#              var.equal=FALSE # Varianzen der Gruppen nicht gleich!
#)


# t-test vorraussetzungen:
# Shapiro test p>0.05 [Normalverteilt]
# Levene test p>0.05 [Varianzhomogenität]



# Mann-Whitey U test (auch Wilcoxon  genannt)

# Variablen sind nur ordinal oder nicht gleichverteilt -> Wilcoxon




library(openxlsx)
daten <- read.xlsx("Folder//Daten.xlsx")
names(daten)
# y2 ~ c2(2,3)
# abhängige variable y2 vergleich mit c2 gruppen 2 und 3
table(daten$c2)

attach(daten) # -> schlechter codingstil
table(c2)
detach() # rückgängig

# daten aufbereiten
y_g2 <- daten$y2[daten$c2 == 2]
y_g3 <- daten$y2[daten$c2 == 3]

# wilcoxon

wilcox.test(x=y_g2,y=y_g3)
wilcox.test(x=y_g2,y=y_g3)$p.value
# signifikant =  signifikanter Unterschied 


# ANOVA


# faktor wichtig!

# daten erzeugen
set.seed(1)

df <- data.frame(x = sample(x=1:3, size=100, replace = TRUE), # 100 samples radomosiert auf zahlen 1-3 mit zurücklegen
                 y = rnorm(100))
head(df)

# regression
formu <- as.formula(y~x)
regr <- lm(formula = formu,data = df)
summary(regr)

#anova
anov <- aov(formula = formu,data = df)
summary(anov)

# -> beide fast gleich!
# ! ANOVA braucht Gruppierungsvariable sonst wird lm ausgeführt!

formu <- as.formula(y~as.factor(x))
anov <- aov(formula = formu,data = df)
summary(anov)

# ANOVA
# kann mehrere Gruppen miteinander vergleichen ( t-test nur 2!)
daten <- read.xlsx("Folder//Daten.xlsx")

table(daten$c1)
# Länge der Daten unterschiedlich -> kein t-test möglich alpha inflation
# Alphafehler-Kumulierung -> fehler 1. Art durch multiples Testen in derselben Stichprobe. 
# Darum ANOVA nutzen

aov(y1~c1, data = daten)
aov(daten$y1~as.factor(daten$c1))

summary(aov(daten$y1~daten$c1)) 
summary(aov(daten$y1~as.factor(daten$c1))) # Video!

# p-wert <=0.05 bedeuted, dass zwischen 2 gruppen signifikanzen bestehen
# aber Achtung: p von 0.05 beruht auf einem Richtwert aus 1927!
# welche gruppen posthoc tests

pairwise.t.test(x = daten$y1,
                g = as.factor(daten$c1),
                p.adjust.method = "none")

pairwise.t.test(x = daten$y1,
                g = as.factor(daten$c1),
                p.adjust.method = "bonferroni")

# zwischen welchen gruppen bestehen signifikante unterschiede?

# Vorraussetzungen ANOVA
# - Normalverteilung
# - Varianzhomogenität

# tapply führt funktion mehrmals aus
tapply(X = daten$y1, INDEX = daten$c1, FUN=shapiro.test)

# > über 0.05 Normalverteilung erfüllt

library(car)

car::leveneTest(y = daten$y1, group = daten$c1, center = "mean")

# > über 0.05 Varianzhomogenität besteht

vals <- summary(anov)

vals[[1]]$`Mean Sq`[1] / vals[[1]]$`Mean Sq`[2]




# 35 Korrelation Pearson und Spearman
# Zusammenhangshypothesen überprüfen

daten <- read.csv(file = "Folder//Daten.csv")
names(daten)

# alle daten mit x miteinander korrellieren

#1. Daten aus datensatz extrahieren
daten_x <- daten[,2:9]
names(daten_x)
# alternative methode mit grep nach pattern in namen suchen
daten_grep <- daten[,grep(pattern = "x",names(daten))]
names(daten_grep)

# Korrelation
cor(daten_x, method ="pearson")  # parametrisch mit Pearson
cor(daten_x, method ="spearman") # nicht-parametrisch mit Spearman

library(psych) 
??psych # Dokumentation Package - 3.5 testing correlations

psych::corr.test(daten_x, method ="pearson") # parametrisch mit Pearson

psych::corPlot(daten_x,method="pearson") # Visualisierung als matrix


psych::corr.test(daten_x, method ="pearson") # nicht- parametrisch mit Spearman

corr_mat=cor(daten_x,method="spearman") # Spearman correlation matrix
psych::corr.test(corr_mat,method="spearman") # nicht-parametrisch mit Spearman
psych::corPlot(corr_mat)



# 36. Einfache lineare OLS Regression

library(openxlsx)
daten <- read.xlsx("Folder//Daten.xlsx")
names(daten)

# Hyptothese: abhängige variable y1 wird von unabhängiger variable x1 beeinflusst
# y1 ~ x1

# daten aufbereiten

daten_reg <- daten[,c("y1","x1")] # nur relevatente daten
names(daten_reg)

# regression (y1 ~ x1)

res <- lm(formula = y1 ~ x1, data = daten_reg)
lm(formula = y1 ~ x1, data = daten) # geht auch!

summary(res) # ergebnisse im detail

# adjusted R-squared nicht so aussagekräftig bei einfacher regression 
# Multiple R-squared nutzen! -> wieviel % der Varianz werden erklärt
# F-Wert -> je höher umso besser
# p value signifikanz 0.05

#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  1.45138    0.17730   8.186  8.2e-16 ***
#  x1         -0.48447    0.03488 -13.888  < 2e-16 ***

# -0.48447 ist Regressionskoeffizient
# seigt x1 um 1, fällt y1 um 0.484

# => statistisch signifikanter, negativer Effekt


# wie groß ist effekt?
# -> Standarditierse Relationskoeffizienten (beta-Werte) benötigt => Z-Transformation

daten_reg$y1_std <- scale(daten_reg$y1) # Z-Transformation
daten_reg$x1_std <- scale(daten_reg$x1) # Z-Transformation

res <- lm(formula = y1_std ~ x1_std, data = daten_reg)
summary(res)
# ist effekt groß? wieder anschauen x1=-4.024e-01
# >=0.1 = kleiner effekt
# >=0.3 = mittelstarker effekt
# >=0.5 = starker effekt


# 36. Multiple lineare OLS Regression
# Regression mit mehr als 1 unabhängiger Variable

# Vorgehen:
# 1.) Regression testen => Modellobjekt erstellen
# 2.) Adjusted R-squared, F statistik und p-wert anschauen (summary())
# 3.) Multikollinearität prüfen durch Invariance Inflation Factor (<3,5,10)
# 4.) Normalverteilung der Residuen (Modellfehler) mit Shapiro testen


daten <- read.csv(file = "Folder//Daten.csv")
#View(daten) # ansicht der tabelle

# Variablen wählen abhängig y1, unabhängig x1,x2,x3
daten_reg <- daten[,c("y1","x1","x2","x3")] # nur relevatente daten

# elegante lösung
library(tidyverse) #dplyr package
daten_reg <- select(daten,"y1","x1","x2","x3") # alternative auswahl
# piping character %>%
# ewig erweiterbar, weniger verschachtelung
daten_reg <- daten %>% select("y1","x1","x2","x3") # %>% daten...


# Regression test (y1 ~ x1,x2,x3)
# also y1 in abhängigkeit von x1,x2 und x3

res <- lm(formula = y1 ~ x1 + x2 + x3,
          data = daten_reg)
summary(res)
# Auf Adjusted R-squared schauen bei MR
# Adjusted R-squared ist die Modellkomplexität (die Anzahl von Variablen) im Verhältnis zu den Daten
# Wert kleiner als Multiple R-squared da er im Verhältnis zu den Daten steht
# F-Wert hoch, p<0.05 => signifikanz bei x1 und x2

# Vorraussetzungen Multiple Regression:
# - Multikollinearität
# - Normalverteilung der Residuen

# - Multikollinearität (zwei oder mehr unabhängige Variablen (x1,x2,x3) haben sehr starke Korrelation)
# => Invariance inflation factor (nicht über 10,5 oder 3)
# package car nutzen zum Test:

library(car)
car::vif(res) # Multikollinearität testen
# Werte um 1 -> keine Probleme!
# Bei Korrelation eine der korrelierenden Variablen rausnehmen



# Normalverteilung der Residuen (Fehler [Residuen] des Modells sind normalverteilt?)

shapiro.test(residuals(res))
# p=0.8296 >> 0.05 => Normalverteilt!


# Weiterführende Hilfen

library(lavaan)

data <- read.csv(file = "Folder//Daten.csv")

head(data, n=3) # n zeigt wieviele zeilen angezeigt werden


# Pfadanalyse

# Variablen im modell durch pfade verbunden
# x1, x2 beeinflusst y
# Pfadmodell in lavaan
#   __      _
#  |x1| -> |y|
--      -
  #         /\
  #   __    | ~ (Regression)
  #  |x2| ---
#   --

# ~~ Korrelation

library(psych)
corr_model <- psych::corr.test(x = data[, c("VAR_A","VAR_B")])
print(corr_model) 
# 0.13 = Korrelationskoeffizient
# 0.21 = Wahrscheinlichkeitswert (p-Wert) für 0.13

# Korrelation ist standartisierte Kovarianz (Messeinheiten spielen keine Rolle)
# Kovarianz zwischen beiden Variablen
cov(data$VAR_A,data$VAR_B)
# 0.0639


# Modell spezifizieren
corr_lavaan <- 'VAR_A~~VAR_B' # Korrelation (~~) zwischen beiden werten

# Modell mit den daten vergleichen (fit)
fit <- sem(model = corr_lavaan, data=data)

summary(fit, standardized = T) # covarianz, p-wert ablesbar
# korrelation durch standardized = T (T=True) angezeigt

# mehrere Variablen gleichzeitg
corr_lavaan <- 'VAR_A ~~ VAR_B + VAR_C' 
fit <- sem(model = corr_lavaan, data=data)
summary(fit, standardized = T) 


# Einfache Regression in Lavaan

# unstandardisiert mit p-wert
sim_reg_model <- lm(formula = VAR_A ~ VAR_B, data = data)
summary(sim_reg_model) # nur Kovarianz 

# standardisiert mit p-wert
sim_reg_model <- lm(formula = scale(VAR_A) ~ scale(VAR_B), data = data)
summary(sim_reg_model) # Korrelation hinzufügen

# Regressionsmodell mit Lavaan erstellen
reg_lavaan <- 'VAR_A ~ VAR_B' # !! nur eine tilde ~ ist Regression
fit <- sem(model = reg_lavaan, data=data)
summary(fit, standardized = T) # Korrelation hinzufügen



# Multiple Regression in Lavaan

# unstandardisiert
mreg_lavaan <- lm(formula = VAR_A ~ VAR_B + VAR_C + VAR_D + VAR_E, data = data) 
summary(mreg_lavaan) # nur Kovarianz 

# standardisiert
mreg_lavaan <- lm(formula = scale(VAR_A) ~ scale(VAR_B) + scale(VAR_C) + scale(VAR_D) + scale(VAR_E), data = data) 
summary(mreg_lavaan) # Korrelation hinzufügen

# Lavaan
mreg_lavaan <- 'VAR_A ~ VAR_B + VAR_C + VAR_D + VAR_E'
fit <- sem(model = mreg_lavaan, data=data)
summary(fit, standardized = T) # Korrelation hinzufügen

# Pfadmodelle definieren & interpretieren

pfad_modell <- '
VAR_A ~ VAR_B + VAR_C + VAR_D 
VAR_B + VAR_C + VAR_D ~ VAR_E
'
# Frage:
# VAR_E beeinflusst VAR_B + VAR_C + VAR_D
# VAR_B + VAR_C + VAR_D beeinflusst VAR_A

fit <- sem(model = pfad_modell, data=data)
summary(fit, standardized = T)
# Estimate = Regressionskoeffizienten
# Std.all  = Standardisierten Effekte
# P(>|z|)  = p-Werte 

# VAR_A ~ VAR_C signifikant mit 0.022 ein negativer effekt
# VAR_C  ~ VAR_E signifikant mit 0.015 ein positiver effekt

# Antwort:
# VAR_E beeinflusst VAR_C  positiv 
# VAR_C beeinflusst VAR_A negativ 
# VAR_E -> VAR_C -> VAR_A
#      ~ (+)  ~ (-)


# Pfadmodell verbessern

# Mehr output -> Verbesserungsvorschläge für das Modell
summary(fit, fit.measures = T, modindices = T)

# alternative beschreibung

summary(fit)
fitmeasures(fit)
# op = "~~" -> Nur Einträge mit Regression einzeichnen
# sort. = T -> absteigende Reihenfolge
# minimum.value = 10 -> minimaler Wert bei mi
modificationindices(fit, sort. = T, minimum.value = 0, op = "~~")


# Umsetzung der Vorschläge durch hinzufügen einer Zeile
pfad_modell_better <- '
VAR_A ~ VAR_B + VAR_C + VAR_D 
VAR_B + VAR_C + VAR_D ~ VAR_E
VAR_C ~~ VAR_D
VAR_B ~~ VAR_C
VAR_B ~~ VAR_D
'
fit <- sem(model = pfad_modell_better, data=data)
summary(fit, standardized = T)

# alternative darstellung
parameterestimates(fit)
# ci.lower => Konfidenzintervalle von effekten
# ci.upper => Konfidenzintervalle von effekten



# Mediationsmodelle

# neben direkten Effekten auch indirekte Effekte untersuchen




# abc sind labels
mediation_model <-'
VAR_A ~ b1*VAR_B + b2*VAR_C + b3*VAR_D + c*VAR_E
VAR_B ~ a1*VAR_E
VAR_C ~ a2*VAR_E
VAR_D ~ a3*VAR_E

# indirekte Effekte
RES_A := a1*b1
RES_B := a1*b1+c

RES_C := a2*b2
RES_D := a2*b2+c
'
# Mediationsmodell bei 56. 3:00 gezeigt

fit <- sem(model = mediation_model, data=data)
# Mehr output -> Verbesserungsvorschläge für das Modell
summary(fit, fit.measures = T, modindices = T)



# Konfirmatorische Faktorenanalyse CFA
# Idee von Datenmodell testen zu erdachten Pfadmodell
# Latente Variablen (auch als Faktoren bezeichnet) - werden von mehreren festen Variablen gemessen
# Beispiel Ergebnisse von Fragebögen -> lassen sich diese Ergebnisse in den Daten wiederfinden?

# Im gegensatz dazu: Explorative Faktorenanalyse: Aus Daten Struktur erkennen
library(lavaan)
data <- read.csv(file = "Folder//Daten.csv")
# lv_ heißt latente variablen hier
cfa_model <-'
lv_VAR_B =~ VAR_B1 + VAR_B2 + VAR_B3 + VAR_B4 + VAR_B5 + VAR_B6 + VAR_B7 + VAR_B8 + VAR_B9 + VAR_B10
lv_VAR_C =~ VAR_C1 + VAR_C2 + VAR_C3 + VAR_C4 + VAR_C5 + VAR_C6 + VAR_C7 + VAR_C8 + VAR_C9 + VAR_C10
lv_VAR_D =~ VAR_D1 + VAR_D2 + VAR_D3 + VAR_D4 + VAR_D5 + VAR_D6 + VAR_D7 + VAR_D8 + VAR_D9 + VAR_D10
'
#lv variablen existieren nicht -> sie sind latent und aus anderen zusammengesetzt


fit <- cfa(model = cfa_model, data = data)
summary(fit)
summary(fit, fit.measures = T, modindices = T) # fit measures ansehen!
# chi-square möglichst signifikant - klappt selten bei großen datenmengen
# RMSEA unter 0,08
# SRMR unter 0,08

# hier passt modell nicht ganz so gut
summary(fit)
fitmeasures(fit)
modificationIndices(fit, sort=T, minimum.vale = 10, op = "~~")


# Strukturgleichungsmodelle
# Ein Strukturmodell (Pfadmodell) kombiniert mit einem Messmodell (CFA)
# Also hier latente Variable X beeinfluss latente Variable Y
# die latenten Variablen werden beeinfluss von Messmodell 1:50 Video 61


sem_model <-'
# Messmodell (Regressionen)
dp =~ VAR_B1 + VAR_B2 + VAR_B3 + VAR_B4 + VAR_B5 + VAR_B6 + VAR_B7 + VAR_B8 + VAR_B9 + VAR_B10
sd =~ VAR_C1 + VAR_C2 + VAR_C3 + VAR_C4 + VAR_C5 + VAR_C6 + VAR_C7 + VAR_C8 + VAR_C9 + VAR_C10
sr =~ VAR_D1 + VAR_D2 + VAR_D3 + VAR_D4 + VAR_D5 + VAR_D6 + VAR_D7 + VAR_D8 + VAR_D9 + VAR_D10


#Strukturmodell (Korrelationen)
VAR_A ~ b1*dp + b2*sd + b3*sr + c*VAR_E
dp ~ a1*VAR_E
sd ~ a2*VAR_E
sr ~ a3*VAR_E

# Indirekte Effekte
RES_E := a1*b1
RES_B := a1*b1+c


fit <- sem(model = sem_model, data = data)
summary(fit, fit.measures = T, modindices = T) 

### Kurzer Überblick ###

# Inferenzstatistik
# Von Beobachtung auf Grundgesamtheit schließen -> Konfidenz, dass Annahme stimmt
# Rückschluss auf Grundgesamtheit
# Schätzen mit Sicherheit (Konfidenz)
# Hypothesentest

# Mittelwert einer unbekannten Population? -> Wichtig Standardfehler des Mittelwerts
# Standardfehler (SE) -> Fehler zu echtem Mittelwert +- (Konfidenzintervall M+-SE)


# Hypothesentest
#
# Wie wahrscheinlich ist eine Beobachtung unter der Annhame, dass die Nullhypothese zutrifft?
# Hypothese = Glaubwürdige Annahme, beruht z.B. auf empirischen Untersuchung & Theorie
# Konfirmatorischer Ansatz: deduktives Wissen, vom allgemeinen Wissen zum spezifischen Ansatz
# Analogie Gericht: Unschuldsvermutung (H0) -> Beweise bewerten -> Wahrscheinlichkeit für Schuld

# Formulieren einer Forschungs-Hypothese (H1)
# Ausgehen vom Gegenteil -> Nullhypothese (H0)
# -> Alphafehler (Typ 1) p<0.05 (Angeklagter ist schuldig wird aber nicht schuldig gesprochen)
# Unterschied in Stichprobe wenn tatsächlich kein Unterschied in Gesamtpopulation besteht
# Vergleich was sehen wir tatsächlich - was ist in der Grundgesamtwert -> p-Wert [0,1]
# Je höher p-Wert umso wahrscheinlicher, dass Annahme besteht obwohl Gesamtpopulation das nicht zeigt
# H0 testen -> p-Wert < 0.05 -> H0 (Es gibt keinen Zusammenhang) wird abgelehnt
# => statistisch signifikanter Unterschied (H1)


# t-Test Theorie
# Zwei Stichproben (Gruppen) miteinander vergleichen
# Variablen - Abhängig (Ergebnis) ist metrisch, Gruppierung (Faktor) ist nominal
# H0 -> Es gibt keinen Unterschied
# Annahmen
# Normalverteilt mit metrisches Daten (Shapiro-Wilk), Varianzhomogenität mit Levene (nicht bei Welch-test) vorrausgesetzt!
# Wenn Daten nicht normalverteilt sind oder abhängige Daten mit ordinalem Skalenniveau -> Mann-Whitney U-Test

# ANOVA Theorie
# Unterschiede zwischen mehreren Stichproben (Gruppen)
# Variablen - Abhängig (Ergebnis) ist metrisch, Gruppierung (Faktor) ist nominal
# Normalverteilt mit metrisches Daten (Shapiro-Wilk), Varianzhomogenität mit Levene vorrausgesetzt!
# Angabe F-Wert (Effektstärke) und p-Wert
# Ergebnis zeigt nur, dass Unterschied besteht!
# => Post-hoc Test zur Umgehung der Alpha-Inflation
# Wenn man test mehrfach wiederholt steigt Typ 1 Fehler an (H0 zurückgewiesen obwohl sie eigentlich wahr ist)

# Kruskal-Wallis Test
# Unterschiede zwischen mehreren Stichproben (Gruppen) 
# DATEN NICHT Normalverteilt
# oder ABHÄNGIGE VARIABLE MIT ORDINALEM SKALENNIVEAU

# Korrelation mit Pearson
# Zusammenhänge zwischn 2 metrischen Variablen testen
# Daten müssen normalverteilt sein
# Ergebnis: r [-1,1] (Korrelationskoeffizient) und p-Wert
# r um 0.3 schwach; r um 0.5 moderat; r um 0.7 starke Beziehung

# Korrelation mit Spearman
# Wenn daten nicht normalverteilt sind
# eine Variable hat ordinales Skalenniveau
# Ergebnis: r [-1,1] (Korrelationskoeffizient) und p-Wert
# r um 0.3 schwach; r um 0.5 moderat; r um 0.7 starke Beziehung


# Was macht gute (quantitative) Forschung aus?
#
# -> Validität (keine Confoundings beeinflussen Messergebnisse), Reliabilität (Zuverlässigkeit der Messergebnisse) und Objektivität (Kein Bias)
#  
#  Gute Forschung ist systematisch, objektiv und genau. 
# Systematische Forschung folgt einer Reihe von vorgegebenen Schritten, um Daten zu sammeln. 
# Objektive Forschung ist unvoreingenommen und bevorzugt nicht eine bestimmte Sichtweise gegenüber 
# einer anderen. Genaue Forschung ist präzise und zuverlässig und liefert Ergebnisse, 
# denen man vertrauen kann.


# Was ist eine Hypothese?
#
# -> Eine auf dem Stand der Wissenschaft  gegründete Annahme, die bisher noch nicht auf ihre Gültigkeit verifiziert wurde.
#  
# Eine Hypothese ist eine Vermutung oder eine fundierte Annahme über die Ursache 
# eines bestimmten Ereignisses oder Zustands. 
# Sie basiert oft auf begrenzten Informationen und kann daher revidiert oder aufgegeben werden, 
# wenn mehr Beweise verfügbar sind. 
# Eine gute Hypothese ist überprüfbar,
# d.h. sie kann durch Experimente oder Beobachtungen verifiziert werden.



# Was ist Statistik?
#
# -> quantitativer (numerischer) Ansatz, um Daten zu sammeln und zu analysieren
# Wichtig für gute Entscheidungsfindung


# Paradigma?
#
# = Meta-Theorie
# Pradigma in statistischer Forschung: Neo-Positivismus
# Probabilstisches Verständnis der Welt
# Meist positivistisches Paradigma - Weltanschauung der Statistik
# Nur unvollständige Perspektive erfassbar
#
# Es gibt eine objektive Realität, aber wir haben nur unvollständigen Zugang zu ihr.


# Deduktion?
#
# Aus allgemeinen Wissen auf einen Einzelfall Rückschlüsse bilden
# Konfirmatorischer Ansatz
# Theorie prüfen anfanh empirischer Daten
# Wird verwendet, um aus Prämissen Schlussfolgerungen zu ziehen
#
# Wir beginnen mit einem allgemeinen Wissen (Theorien, Annahmen, frühere empirische Erkenntnisse) 
# und betrachten dann einen konkreten Fall (zum Beispiel deinen Datensatz, deine Beobachtungen).
# Auf diese Weise erweiterst du die bisherige Wissensbasis.

# Forschungsprozess:
#  1. Theorie überblicken und modellieren
#  2. Daten sammeln
#  3. Abgleich vornehmen -> Theorie den gesammelten Daten gegenüberstellen

# Allgemeines Wissen: Modell / Theorie in Einleitung & Hintergrund beschreben
# Einzelfall: emprischer Datensatz
# Abschluss: Diskussion zwischen Theorie und Empirie
# 
# Wir nutzen quantitative Daten - oft in großem Umfang - um Entscheidungen zu treffen. 
# Oft geht es darum, Muster zu erkennen, die wir mit den Augen nicht sehen können, 
# es kann darum gehen, Stichproben von Daten zu beschreiben oder Hypothesen über Gruppenunterschiede 
# oder Beziehungen zwischen Variablen zu testen.

# Mehr differenziertes Verständnis von Variable (z.B. Geschlecht)?
# -> nicht direkt beobachtbar => latente Variable

# Ziel der Statistik?
#
# Rückschluss von Stichprobe (sample) auf Grund-Gesamtheit schließen (Inferenz)
# Stichprobe wird aus Grundgesamtheit gezigen
# Auf Basis der statistischen Auswertung wird Rückschluss auf Grundgesamtheit gezogen
# 
# Eine Stichprobe ist eine Teilmenge der Grundgesamtheit. 
# Wir verwenden ein Stichprobenverfahren, um zu dieser Teilmenge zu gelangen. 
# Anschließend führen wir Berechnungen mit der Stichprobe durch und ziehen Rückschlüsse 
# auf die Grundgesamtheit.

# Was sind wichtige Gründe, um Statistik zu lernen?
#
# Statistik spielt in der Forschung eine wichtige Rolle.
# Um die Forschung anderer zu verstehen, ist es wichtig, Statistikkenntnisse zu haben. 
# Außerdem ist es wichtig, dass du selbst quantitative Forschungsstudien durchführst 
# (z.B. für deine Abschlussarbeit). Ganz allgemein helfen dir Statistiken dabei, 
# fundiertere Entscheidungen im Geschäfts- und Privatleben zu treffen. 
# Da auch Journalisten und Politiker viel mit Hilfe von Statistiken kommunizieren, 
# sind statistische Kenntnisse unerlässlich, um ein informierter Bürger zu sein.



# Statistische Daten
# Zeilen für Fälle (jeder Studienteilnehmer)
# Variablen in Spalten (Daten des Studienteilnehmers)

# Latente Variablen: nur indirekt beobachtbar (in Skalen bzw. Sammlung mehrerer Items/Variablen)
# - brauchen komplexe Operationalisierung
# Manifeste Variablen: direkt beobachtbar (messbar)
# -> Validität, Reliabilität und Objektivität
#
# Validität: Es wird das gemessen was auch angedacht war (keine Confoundings)
# Reliabilität: Erneute Messung führt zu gleichen Ergebnissen
# Objektivität: Egal wer die Studie durchführt

# Hypothesen sind glaubwürdige Annehmen und wichtige Beusteine für statistische Modelle



# Datenerhebung

# Stichprobe: Ziel -> unverzerrt, repräsentativ
# Arten: Zufallsaufwahl (weniger Zielgerichtet, alle Kriterien werden implizit berücksichtigt), 
# Quotenstichprobe (Zielgerichtet, Dimensionen sind wichtig wie Alter etc,), 
# Convienence Sampling (Logistisch einfachste Lösung, verzerrte Stichproben)

# Mindeststichprobengröße ist abhängig von Komplexität und Größe des untersuchten Effekts
# Fausregel in der Psychologie: 100-200 Fälle
# 2. Faustregel: Pro Variable 20 Fälle

# Fehler sind Abweichungen von der Grundgesamtheit
# Stichproben müssen repräsentativ sein!
# Zufallsfehler (unsystematisch)
# Systematischer Fehler (Verzerrt Ergebnis!)

# Arten der Datenerhebung:
# Interviews
# Beobachtungen (Experimente)
# Sammlung von Dokumenten (Sekundärdaten) -> Daten extrahieren, aber unflexibel
# Mehrheit: Quantitative Daten

# Skalenniveaus: Nominal, Ordinal, Intervall, Rational
# -> Reliabilitätsanalyse und Faktorenanalyse sind Techniken um Skalen zu erstellen und zu testen

# Klarheit und Genauigkeit wichtig!
# Kognitiver Vortest mit Think aloud Protokoll auf Studienfragen

# Explorative Foktorenanalyse (EFA)
#
# Welches Item gehört zu welcher Skala?
#
# Welche Faktoren gibt es in einer Menge von Items?
# -> Herausfinden wie ein komplexes Konstrukt gemessen werden 
# Struktur für Items zu finden
#
# -> EFA Schritte
# Wieviele Faktoren gibt es? 
# Faktoren extrahieren (Nach Mustern/struktur in Daten suchen)
# Faktoren rotieren 

# Interpretation EFA
# Tabelle mit Faktoren in den Spalten (Anzahl Faktoren = Anz. Spalten)
# Zeilen sind Items
# Ladung: Von jeden Item auf jeden Faktor (zwischen [-1, +1])
# Ladungen > 0,6 suchen -> Indiz für Zusammengehörigkeit
# Cross-loadings: Es gibt 1 Item, dass auf mehrere Faktoren eine starke Ladung (Beziehung) hat 
# Querbelastung prüfen
# Zuweisung von Faktor-Namen -> Was Bedeuten die Faktoren? Bezeichnung, Label
