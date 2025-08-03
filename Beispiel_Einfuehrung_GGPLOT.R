
######### TESTEN GGPLOT 2 #########################
### Installation des packages

# clear memory
rm(list = ls())

# einmalig
install.packages("ggplot2")
install.packages("data.table")


# immer bei Erstellung von Plots
library(ggplot2)

# Data tabel muss ebenfalls geladen werden, weil man sonst die Daten nicht einlesesn kann.
library(data.table)

# Einlesen Daten

wege <- fread("//ifv-fs.ifv.kit.edu/Lehre/Lehrveranstaltungen/Empirische Daten im Verkehrswesen/WS24-25/Übung/MOP/MOP-Daten/W.csv")

#### Beispiel durch. Wegedauer nach Verkehrsmittel ###

# Berechnung der durchn. Wegedauer pro Verkehrsmittel 
mean_duration_by_VMDIW = aggregate(DAUER~VMDIW, wege, mean)

# Plot
ggplot(mean_duration_by_VMDIW, aes(VMDIW, DAUER)) + geom_point()

# Benennung der Verkehrsmittel (Reihenfolge entspricht der Nummerierung im Codeplan!)
VMDIW_NAME<- c("Fuss", "Rad","Elektrofahrrad", " Mofa etc.", "PKW Fahrer", "PKW Mitfahrer", "Stadtbus", "Fernbus", "StraBa", "SBahn", "Fernzug","Flugzeug", "sonstige")

# cbind =  Hinzufügen einer Spalte -> siehe Cheat Sheet 
cbind(mean_duration_by_VMDIW, VMDIW_NAME)

# Erneuter Plot, diesmal mit Beschriftung der Verkehrsmittelnahmen 
ggplot(mean_duration_by_VMDIW, aes(VMDIW_NAME, DAUER)) + geom_point()



## Anpassung der Plots ## 

# Größe der Punkte ändern 
ggplot(mean_duration_by_VMDIW, aes(VMDIW_NAME, DAUER)) + geom_point(size = 4)

# Farbe aendern 
ggplot(mean_duration_by_VMDIW, aes(VMDIW_NAME, DAUER)) + geom_point(colour = "blue")

ggplot(wege, aes(KM, DAUER)) + geom_point(colour="green")                                        

ggplot(wege, aes(VMDIW))+geom_histogram(binwidth = 7, color="black", fill="red")
ggplot(wege, aes(VMDIW))+geom_histogram(binwidth = 7, color="black", fill="#009682")

# Prompt für schönere Abbildung
# benenne Achsenbeschriftungen um 
# x-Achse = Hauptverkehrsmittel 
# y-Achse = Anzahl 
# Füge eine horizontale Linie, gestrichelt bei 5000 ein 
ggplot(wege, aes(VMDIW)) +
  geom_histogram(binwidth = 7, color = "black", fill = "#009682") +
  labs(x = "Hauptverkehrsmittel", y = "Anzahl") +
  geom_hline(yintercept = 5000, linetype = "dashed", color = "red")

# Speichern
ggsave("hfgk_VMDIW.jpg", width =12, height=7)
ggsave(plot_name,"hfgk_VMDIW.jpg", width =12, height=7)





