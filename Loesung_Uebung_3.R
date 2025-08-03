# Uebung Nr 3 - Emp. Daten im Verkehrswesen
#
# 27.01.2025

### Clear memory
rm(list = ls())


# einmalig
install.packages("ggplot2")
# install.packages("ggplot2")

library(data.table)
library(ggplot2)

## Einfuehrung GGPLOT2 ##

# Aufgabe 1.1

wege <- fread("//ifv-fs.ifv.kit.edu/Lehre/Lehrveranstaltungen/Empirische Daten im Verkehrswesen/WS24-25/Übung/MOP/MOP-Daten/W.csv")


# Aufgabe 1.2
# Geschwindigkeit auf Fußwegen mit eingefärbten roten Punkten
ggplot(wege[VMDIW==11], aes(KM, SPEED)) + geom_point(colour="red", size=2)


# Aufgabe 1.3
# Berechnung der durchn. Geschwindigkeit pro Verkehrsmittel
# und Abspeichern in einem separaten Datensatz
mean_speed_by_VMDIW = aggregate(SPEED~VMDIW, wege, mean)

# Plot
ggplot(mean_speed_by_VMDIW, aes(VMDIW, SPEED)) + geom_point()


# Benennung der Verkehrsmittel (Reihenfolge entspricht der Nummerierung im Codeplan!)
VMDIW_NAME<- c("Fuss", "Rad","Elektrofahrrad", " Mofa etc.", "PKW Fahrer", "PKW Mitfahrer", "Stadtbus", "Fernbus", "StraBa", "SBahn", "Fernzug","Flugzeug", "sonstige")

# cbind =  Hinzufügen einer Spalte -> siehe Cheat Sheet 
cbind(mean_speed_by_VMDIW, VMDIW_NAME)

# Erneuter Plot, diesmal mit Beschriftung der Verkehrsmittelnahmen 
ggplot(mean_speed_by_VMDIW, aes(VMDIW_NAME, SPEED)) + geom_point(size = 5, colour="blue")


# Aufgabe 1.4
#Häufigkeit Wege mit Verkehrsmitteln
ggplot(wege, aes(VMDIW))+geom_histogram(binwidth = 1)

# Aussagekärftiger wird der Plot, wenn die Verkehrsmittel auf der x-Achse wieder als solche benannt werden.
# Mapping
VMDIW__MAPPING<- c("11" = "Fuss", 
                   "21" = "Rad",
                   "22" = "Elektrofahrrad",
                   "31" = " Mofa etc.", 
                   "41" = "PKW Fahrer",
                   "51" = "PKW Mitfahrer", 
                   "61" = "Stadtbus", 
                   "62" = "Fernbus", 
                   "71" = "StraBa", 
                   "72" = "SBahn", 
                   "73" = "Fernzug",
                   "81" = "Flugzeug",
                   "91" = "sonstige")

wege$VMDIW_NAME <- factor(wege$VMDIW, levels = names(VMDIW__MAPPING), labels = VMDIW__MAPPING)

# Plot erstellen
ggplot(wege, aes(VMDIW_NAME)) +
  geom_bar() +  # geom_bar für Häufigkeitsdarstellung
  xlab("Verkehrsmittel") +
  ylab("Häufigkeit") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Wir sehen sehr eindeutig, dass die meisten Wege mit dem Pkw zurueckgelegt werden


# Aufgabe 1.5
# Boxplots für die Geschwindigkeiten der VMDIW
ggplot(wege, aes(group = VMDIW_NAME, x = VMDIW_NAME, y = KM, color = VMDIW_NAME)) + 
  xlab("Verkehrsmittel") +
  ylab("Distanz in km") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  geom_boxplot() 

# Plot speichern
ggsave("plot_boxplot.png", width=5, height=5)

