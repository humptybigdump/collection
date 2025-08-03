## Beispiele für die Sitzung Grafische Darstellungen

## Benötigte Pakete laden

library(tidyverse)
library(gridExtra) # wird benötigt, um mehrere Diagramme nebeneinander zu platzieren
library(lubridate) # Für die bessere Verarbeitung von Datumsangaben
library(scales) # Funktionen für die Veränderung der Skalen in Diagrammen

## Datensatz einlesen - liegt als CSV-Datei im Ordner data vor

wissenschaftliche_akteure <- read_csv("data/codierung_wissenschaftliche_akteure.csv")

## Wir machen das Geschlecht zunächst zu einer Faktor-Variable. So können wir 
## die Reihenfolge der Werte vorgeben, die später in der Visualisierung verwendet
## werden soll.

wissenschaftliche_akteure <- wissenschaftliche_akteure %>%
  mutate(gender = factor(gender,
                         levels = c("männlich", "weiblich", "anderes/nicht bestimmbar")))

## Kreisdiagramm
## Für das Kreisdiagramm wird zuerst über mutate eine Variable erzeugt, die
## den Anteil der verschiedenen Ausprägungen beinhaltet und dann eine Variable, die
## die Position beinhaltet, in der später die Beschriftung eingefügt wird.
## Dann wird ein Balkendiagramm über geom_col erzeugt und mit coord_polar
## zu einem Kreisdiagramm gemacht. Mit scale_fill_brewer wird die Farbpalette gewählt,
## mit ggtitle der Titel erzeugt.

wissenschaftliche_akteure %>% count(gender) %>% 
  mutate(share = n/sum(n), gender = fct_reorder(gender, share, .desc = TRUE),
         ypos = 1-(cumsum(share)-0.5*share)) %>% 
  ggplot(aes(x="", y=share, fill=gender)) +
    geom_col(width = 1, color = "white") +
    coord_polar("y", direction = -1) +
    theme_void(base_size = 24) +
    geom_text(aes(y = ypos+0.02, label = paste(round(share * 100, digits = 1), "%")),
              color = "black", size = 8) +
    scale_fill_brewer(palette="Paired", name = "Geschlecht") +
    ggtitle(paste0("Geschlechterverteilung der Forschenden (n = ",
                   nrow(wissenschaftliche_akteure), ")"))

## Balkendiagramme
## Balkendiagramme werden mit geom_bar oder geom_col erzeugt. Mit geom_bar ist die Höhe der 
## Balken davon abhängig, wie oft eine Ausprägung im Datensatz vorkommt, mit geom_col
## vom Wert einer Variable.

## Wenige Ausprägungen
## Mit theme_minimal wird ausgewählt, dass der Hintergrund etc. des Diagramms recht schlicht sein soll

wissenschaftliche_akteure %>%
  ggplot(aes(gender, fill = gender)) +
    geom_bar() +
    labs(x = "Geschlecht", y = "Anzahl Forschende",
         title = paste0("Geschlechterverteilung der Forschenden (n = ",
                       nrow(wissenschaftliche_akteure), ")")) +
    scale_fill_brewer(palette = "Paired") +
    theme_minimal(base_size = 24) +
    theme(legend.position = "none",
          panel.grid.major.x = element_blank())

## Balken unterteilt basierend auf zweiter Variable
## Hier wird geom_col verwendet, weil vorher der Anteil der einzelnen Ausprägungen je
## Themenbereich als neue Variable errechnet wurde und damit schon im Datensatz ist.
## Über das fill-Argument wird angegeben, von welcher Variable die Farbe der Balken abhängig sein soll.
## Unter scale_fill_brewer wird erneut angegeben, welche Farbpalette verwendet werden soll,
## zusätzlich noch, welche Bezeichnungen für die verschiedenen Ausprägungen vergeben werden.

wissenschaftliche_akteure %>%
  group_by(issue, gender) %>% summarise(n = n()) %>% 
  mutate(share = n/sum(n)) %>% 
  ggplot(aes(gender, share, fill = issue)) +
  geom_col(position = "dodge") +
  labs(x = "Geschlecht", y = "Anteil",
       title = paste0("Geschlechterverteilung der Forschenden\nje Themenbereich (n = ",
       nrow(wissenschaftliche_akteure), ")"),
       fill = "Themenbereich") +
  scale_fill_brewer(type = "qual", palette = "Set1",
                    labels = c("Antibiotikaresistenz", "Biotech", "Klimawandel",
                               "Neuroscience")) +
  scale_x_discrete(labels = c("männlich", "weiblich", "anderes/\nnicht bestimmbar")) +
  theme_minimal(base_size = 24) + theme(panel.grid.major.x = element_blank())

wissenschaftliche_akteure %>% filter(issue != "amr") %>% 
  group_by(zeitraum, gender) %>% summarise(n = n()) %>% 
  mutate(share = n/sum(n)) %>% 
  ggplot(aes(gender, share, fill = zeitraum)) +
  geom_col(position = "dodge") +
  labs(x = "Geschlecht", y = "Anteil",
       title = paste0("Geschlechterverteilung der Forschenden\nje Zeitraum (n = ",
                      wissenschaftliche_akteure %>% filter(issue != "amr") %>% nrow,
                      ")"),
       fill = "Zeitraum") +
  scale_fill_brewer(type = "qual", palette = "Set2",
                    labels = c("2000-2002", "2015-2019")) +
  scale_x_discrete(labels = c("männlich", "weiblich", "anderes/\nnicht bestimmbar")) +
  theme_minimal(base_size = 24) + theme(panel.grid.major.x = element_blank())

## Viele Ausprägungen
## Hier hat die dargestellte Variable sehr viele Ausprägungen, weswegen die einzelnen
## Balken sehr schmal sind.

wissenschaftliche_akteure %>% filter(!is.na(discipline)) %>%
  mutate(discipline = factor(discipline) %>% fct_infreq()) %>% 
  {ggplot(., aes(discipline)) +
      geom_bar() +
      labs(x = "Forschungsgebiet", y = "Anzahl Forschende",
           title = paste0("Häufigkeit der wissenschaftlichen Disziplinen (n = ",
                          nrow(.),
                          ")")) +
      scale_x_discrete(guide = guide_axis(angle = -60)) +
      theme_minimal(base_size = 20) + theme(panel.grid.major.x = element_blank())} 

## Skalen verändern
## Hier habe ich über coord_cartesian angegeben, dass die y-Achse nur von 1125 bis 1150 gehen soll.
## Zuvor habe ich in mutate über fct_collapse mehrere Ausprägungen einer Variable zusammengefasst.

wissenschaftliche_akteure %>%
  mutate(aktort = fct_collapse(aktort, Deutschland = "Deutschland",
                               other_level = "Anderes Land") %>%
           fct_infreq()) %>% 
  ggplot(aes(aktort)) +
    geom_bar() +
    coord_cartesian(ylim = c(1125, 1150), expand = TRUE) +
    labs(x = "Nationale Verortung", y = "Anzahl Forschende",
         title = paste0("Nationale Verortung der Forschenden (n = ",
                        nrow(wissenschaftliche_akteure), ")")) +
    theme_minimal(base_size = 24) + theme(panel.grid.major.x = element_blank())

## So sieht das Diagramm eigentlich aus

wissenschaftliche_akteure %>%
  mutate(aktort = fct_collapse(aktort, Deutschland = "Deutschland",
                               other_level = "Anderes Land") %>%
           fct_infreq()) %>% 
  ggplot(aes(aktort)) +
  geom_bar() +
  labs(x = "Nationale Verortung", y = "Anzahl Forschende",
       title = paste0("Nationale Verortung der Forschenden (n = ",
                      nrow(wissenschaftliche_akteure), ")")) +
  theme_minimal(base_size = 24) + theme(panel.grid.major.x = element_blank())

## Hier habe ich über scale_y_continous den Bereich der y-Achse deutlich vergrößert

wissenschaftliche_akteure %>% 
  ggplot(aes(gender, fill = gender)) +
  geom_bar() +
  labs(x = "Geschlecht", y = "Anzahl Forschende",
       title = paste0("Geschlechterverteilung der Forschenden (n = ",
                      nrow(wissenschaftliche_akteure), ")")) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(limits = c(0, 10000)) +
  theme_minimal(base_size = 24) +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank())

## Trendlinie
## Die Trendlinie wird über den Befehl geom_line eingefügt, mit position_dodge wird
## eingestellt, dass die Linie jeweils über der Mitte der einzelnen Balkenteile aufhört.

wissenschaftliche_akteure %>% filter(issue != "amr") %>% 
  group_by(zeitraum, gender) %>% summarise(n = n()) %>% 
  mutate(share = n/sum(n)) %>% 
  ggplot(aes(zeitraum, share, fill = gender, group = gender, color = gender)) +
  geom_col(position = "dodge") +
  geom_line(position = position_dodge(width = 1), linewidth = 1) +
  labs(x = "Zeitraum", y = "Anteil",
       title = paste0("Geschlechterverteilung der Forschenden\nje Zeitraum (n = ",
                      wissenschaftliche_akteure %>% filter(issue != "amr") %>% nrow,
                      ")"),
       fill = "Geschlecht") +
  scale_x_discrete(labels = c("2000-2002", "2015-2019")) +
  scale_fill_brewer(type = "qual", palette = "Paired",
                    labels = c("männlich", "weiblich", "anderes/\nnicht bestimmbar")) +
  scale_color_brewer(type = "qual", palette = "Paired") +
  guides(color = "none") +
  theme_minimal(base_size = 24) + theme(panel.grid.major.x = element_blank())
  

## Histogramm
## Zuerst erzeuge ich ein Balkendiagramm mit sehr vielen Ausprägungen, bei dem
## man nicht gut erkennen kann, wie die Verteilung eigentlich aussieht. Dabei habe ich 
## alle Wissenschaftler:innen mit mehr als 1000 Publikationen ausgeschlossen, damit das Diagramm
## nicht zu unübersichtlich wird.

wissenschaftliche_akteure %>% filter(publications < 1000) %>% 
  {ggplot(., aes(publications)) +
  geom_bar() +
  labs(x= "Anzahl Publikationen", y = "Forschende mit dieser Publikationszahl",
       title = paste0("Verteilung der Publikationszahl (n = ",
                      nrow(.), ")"))+
      theme_minimal(base_size =  24)}

## Einfaches Histogramm mit den Standardeinstellungen (30 Balken)
## Ein Histogramm ist im Vergleich deutlich übersichtlicher. Die zugehörige Funktion
## heißt geom_histogram, standardmäßig werden 30 Balken erzeugt.

wissenschaftliche_akteure %>% filter(publications < 1000) %>% 
  {ggplot(., aes(publications)) +
      geom_histogram(color = "white") +
      labs(x= "Anzahl Publikationen", y = "Forschende mit dieser Publikationszahl",
           title = paste0("Verteilung der Publikationszahl (n = ",
                          nrow(.), ")")) +
      theme_minimal(base_size =  24)}

## Vergleich verschiedener Histogramme mit unterschiedlicher Balkenzahl
## (3, 5, 10, 30, 50 oder 100 Balken)
## Erzeugt werden die verschiedenen Diagramme über die map-Funktion, mit
## marrangeGrob werden sie dann zusammengefügt.

histograms <- map(c(3,5,10,30,50,100), ~ wissenschaftliche_akteure %>%
      filter(publications < 1000) %>% 
      ggplot(aes(publications)) +
        geom_histogram(bins = .x, color = "blue") +
        labs(title = paste0("Histogramm mit ", .x, " Balken"),
             x = "Anzahl Publikationen", y = "Forschende mit dieser Publikationszahl"))

marrangeGrob(grobs = histograms, nrow = 2, ncol = 3,
             layout_matrix = matrix(seq_len(2 * 3), byrow = TRUE,
                                    nrow = 2, ncol = 3),
             top = NULL)

## Liniendiagramm
## Verwendet wird ein Datensatz mit Informationen unter anderem zur Entwicklung der Arbeitslosenzahlen
## in den USA von Juli 1967 bis April 2015, jeder Eintrag ist ein Monat.
## Liniendiagramme erzeugt man mit geom_line. Mit geom_vline kann man vertikale Linien
## im Diagramm erzeugen, hier muss man nur die x-Achsen-Schnittstelle angeben.
## Mit geom_hline erzeugt man entsprechend horizontale Linien.
## Über linetype habe ich die Art der Linie angegeben, 2 steht für dashed, also gestrichelt.

economics %>% mutate(unemploy_share = unemploy*100/pop) %>% 
  ggplot(aes(date, unemploy_share)) + geom_line(size = 2) +
  geom_vline(xintercept = as.Date("1982-12-01"), color = "red", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as.Date("2009-10-01"), color = "red", linetype = 2, size = 1.5) +
  scale_y_continuous(limits = c(0, 5.5), breaks = c(0,1,2,3,4,5)) +
  labs(title = "Entwicklung der Arbeitslosenquote in den USA\nvon 1967 bis 2015",
       x = NULL, y = "Anteil Arbeitslose an Gesamtbevölkerung (%)",
       caption = "Daten von der Federal Reserve Bank of St. Louis") +
  theme_minimal(base_size =  24)

## Verteilungskurven
## Die Funktion für Verteilungskurven heißt geom_density

wissenschaftliche_akteure %>% filter(publications < 1000) %>% 
  {ggplot(., aes(publications)) +
      geom_density(size = 2) +
      labs(x= "Anzahl Publikationen", y = "Häufigkeitsdichte",
           title = paste0("Verteilung der Publikationszahl (n = ",
                          nrow(.), ")")) +
      theme_minimal(base_size =  24)}

## Hier vergleiche ich Verteilungskurve und Histogramm. Dafür werden beide erst als
## variablen abgespeichert und dann mit grid.arrange zusammengefügt.

liniendiagramm <- wissenschaftliche_akteure %>% filter(publications < 1000) %>% 
  {ggplot(., aes(publications)) +
      geom_density(size = 2) +
      labs(x= "Anzahl Publikationen", y = "Häufigkeitsdichte") +
      theme_minimal(base_size =  20)}

histogramm <- wissenschaftliche_akteure %>% filter(publications < 1000) %>% 
  {ggplot(., aes(publications)) +
      geom_histogram(color = "white", bins = 100) +
      labs(x= "Anzahl Publikationen", y = "Forschende mit dieser Publikationszahl") +
      theme_minimal(base_size =  20)}

grid.arrange(histogramm, liniendiagramm, ncol = 2)

## Boxplots
## Die Funktion für Boxplots heißt geom_boxplot. Auch hier vergleiche ich Boxplots,
## Liniendiagramm und Histogramm. Dafür erzeuge ich wieder die einzelnen Diagramme, bei 
## Histogramm und Verteilungskurve musste ich dabei die Achsen tauschen über coord_flip
## (so kann man übrigens auch die Balken in einem Balkendiagramm horizontal laufen lassen),
## dann wird alles wieder mir grid.arrange zusammengefügt.

wissenschaftliche_akteure %>% filter(publications <= 1000) %>% 
  {ggplot(., aes(issue, h_index)) + geom_boxplot(size = 1.5, outlier.size = 2.5) +
  scale_x_discrete(labels = c("Antibiotikaresistenz", "Biotech", "Neuroscience"),
                   name = "Themenbereich") +
  labs(title = paste0("Verteilung der H-Indizes in den verschiedenen\nThemenbereichen (n = ",
                      nrow(.), ")"), y = "H-Index") +
      theme_minimal(base_size =  24)
  }

liniendiagramm_h_index <- wissenschaftliche_akteure %>% filter(publications <= 1000) %>%
  ggplot(aes(h_index)) + geom_density(size = 2) + coord_flip() + labs(x = NULL, y = NULL) +
  scale_y_continuous(breaks = NULL) + theme_minimal(base_size =  20)

histogramm_h_index <- wissenschaftliche_akteure %>% filter(publications <= 1000) %>%
  ggplot(aes(h_index)) + geom_histogram(bins = 50) + coord_flip() +
  labs(x = "H-Index", y = NULL) + scale_y_continuous(breaks = NULL) +
  theme_minimal(base_size =  20)

boxplot_h_index <- wissenschaftliche_akteure %>% filter(publications <= 1000) %>%
  ggplot(aes(0, h_index)) + geom_boxplot(size = 1.5, outlier.size = 2.5) +
  labs(y = NULL, x = NULL) + scale_x_continuous(breaks = NULL) +
  theme_minimal(base_size =  20)

grid.arrange(histogramm_h_index, liniendiagramm_h_index, boxplot_h_index,
             nrow = 1)

## Weitere Diagrammtypen
## Um die Möglichkeiten von ggplot zu demonstrieren, kommen hier noch ein paar weitere
## Diagramme: Violin Plots als Alternative zu Boxplots, geom pointrange zur Visualisierung
## von Hetergonetität in einer Variable und einen Countplot für die Visualisierung von zwei kategorialen Variablen

wissenschaftliche_akteure %>% filter(publications <= 1000) %>% 
  {ggplot(., aes(issue, h_index)) + geom_violin(linewidth = 1.25) +
      scale_x_discrete(labels = c("Antibiotikaresistenz", "Biotech", "Neuroscience"),
                       name = "Themenbereich") +
      labs(title = paste0("Verteilung der H-Indizes in den verschiedenen\nThemenbereichen (n = ",
                          nrow(.), ")"), y = "H-Index") +
      theme_minimal(base_size =  24)
  }

wissenschaftliche_akteure %>% 
  filter(publications <= 1000) %>% group_by(issue) %>%
  summarise(mean_h_index = mean(h_index, na.rm = TRUE),
            sd_h_index = sd(h_index, na.rm = TRUE),
            lower = mean_h_index - sd_h_index,
            upper = mean_h_index + sd_h_index) %>% 
  ggplot(aes(issue, mean_h_index)) +
      geom_pointrange(aes(ymin = lower, ymax = upper)) +
      scale_x_discrete(labels = c("Antibiotikaresistenz", "Biotech", "Neuroscience"),
                       name = "Themenbereich") +
      labs(title = "Durchschnittliche H-Indizes in den verschiedenen\nThemenbereichen (n = 1501)",
           y = "H-Index") +
      theme_minimal(base_size =  24) + theme(panel.grid.major.x = element_blank())
  
wissenschaftliche_akteure %>% filter(issue != "amr") %>% 
  {ggplot(., aes(issue, zeitraum)) +
      geom_count(colour = "grey30") + scale_size_area(max_size = 40) +
      labs(x = NULL, y = NULL, size = "Anzahl Forschende",
           title = paste0("Forschende pro Thema und Zeitraum (n = ",
                          nrow(.), ")")) +
      scale_x_discrete(labels = c("Biotechnologien", "Klimawandel", "Neurowissenschaften")) +
      scale_y_discrete(labels = c("Später Zeitraum", "Früher Zeitraum")) +
      theme_minimal(base_size =  24)}
