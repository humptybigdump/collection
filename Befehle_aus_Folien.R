#############Einfuehrung in R #############

# Uebung Nr. 1, 21.12.2020
# Autor: Nadine Kostorz, IFV

# Code-Schnipsel aus den Folien 


#Zuweisungen
x= 0.5
y=1
z=y+2

Orte_BaWue <- c("Karlsruhe", "Heidelberg", "Stuttgart")


########### Daten importieren/ einlesen ########### 

### CSV-Daten einlesen ###

## Daten einlesen
# pfad in "" muss mit dem Speicherort des Datensatzes auf dem PC übereinstimmen, Achtung bei Ausrichtung der \ ---> / 
# header = TRUE gibt an, ob der Datensatz Ueberschriften hat, die importiert werden sollen, 
# sep="" gibt an mit welchem Zeichen die Daten getrennt werden, bei CSV-Dateien sind es meist , oder ;

meineDaten <- read.table("//ifv-fs/Empirische Daten/WS20-21/Testdaten.csv", header=TRUE, sep=",")


#Daten als data.table einlesen -> siehe Bsp in Uebung 1

meineDaten= fread("//ifv-fs.ifv.kit.edu/Empirische Daten/WS20-21/Testdaten.csv")



### Excel-Daten einlesen ###

# benoetigtes Package
# installieren (einmalig)
install.packages(xlsx)

# Funktionsbibiliothek muss jedes Mal geladen werden
library(xlsx)

# Einlsen der ersten Zeile eines Datensatzes
meineDaten1 <- read.xlsx("//ifv-fs.ifv.kit.edu/Empirische Daten/WS20-21/Testdaten.xlsx", 1)

# Einlesen eines bestimmten Tabellenblatts aus Excel
meineDaten2 <- read.xlsx("//ifv-fs.ifv.kit.edu/Empirische Daten/WS20-21/Testdaten.xlsx", sheetName = "Tabelle1")

########### Workspace speichern ########### 

# Gesamten Workspace speichern 
save.image("C:/ ... Your Path ... /all_data.RData")


#Ausgewaehlte Daten speichern
save(x,Orte_Bawue,
     file = "C:/ ... Your Path ... /data_1_and_2.RData")

# Weitermachen mit dem Workspace
load("C:/ ... Your Path ... /all_data.RData") # muss mit vorherigen Pfad uebereinstimmen

########### Daten exportieren ########### 

# Excel-Export
library(xlsx)
write.xlsx(Daten, "//....Your Path.../Dateiname.xlsx", sheetName="Sheet1")

#CSV-Export
write.csv(Daten, "//....Your Path.../Dateiname.csv")


###########Einfache deskritptive Statistik ########### 

#Mittelwert
mean(meineDaten$Variable)

#Varianz
var(meineDaten$Variable)

#Häufigkeitstabelle
table(meineDaten$Variable)

#Kreuztabelle
table(meineDaten$Variable1,meineDaten$Variable2)

