## Beispiele für die Sitzung Testen

## Benötigte Pakete werden geladen

library(tidyverse)

## Datensatz einlesen - liegt als CSV-Datei im Ordner data vor

wissenschaftliche_akteure <- read_csv("data/codierung_wissenschaftliche_akteure.csv")

## Wie im letzten Script werden Konfidenzintervalle für den Mittelwert der Publikationszahl
## in verschiedenen Fachbereichen.

wissenschaftliche_akteure %>% filter(issue != "klimawandel") %>% group_by(issue) %>%
  summarise(Mittelwert = mean(publications, na.rm = TRUE),
            Standardabweichung = sd(publications, na.rm = TRUE),
            Standardfehler = Standardabweichung/sqrt(n()),
            `5%` = Mittelwert - Standardfehler * 1.96,
            `95%` = Mittelwert + Standardfehler * 1.96,
            n = n()) %>% 
  ggplot(aes(issue, Mittelwert)) + 
  geom_pointrange(aes(ymin = `5%`, ymax = `95%`), size = 2.5) +
  scale_x_discrete(name = "Themenbereich",
                   labels = c("Antibiotikaresistenz\n(n = 374)", "Biotech\n(n = 731)",
                              "Neurowissenschaften\n(n=878)")) +
  labs(y = "Mittelwert der Publikationszahl",
       title = "Durchschnittliche Publikationszahl in verschiedenen Themenbereichen",
       subtitle = "Linien zeigen 95 %-Konfidenzintervalle") +
  theme_minimal(base_size = 24)

## Um Konfidenzintervalle zu erzeugen, die sich nicht überlappen, wird hier zuerst in allen
## Fällen aus dem Bereich Biotech die Publikationszahl um 30 erhöht. Dann wird erneut
## wie oben ein Diagramm mit Konfidenzintervallen erzeugt. Mit geom_hline wird eine
## horizontale Linie erzeugt, die am unterenden Rand des oberen Konfidenzintervalls eingezeichnet ist.

wissenschaftliche_akteure %>% filter(issue %in% c("biotech", "neuroscience")) %>% 
  mutate(publications = if_else(issue == "biotech", publications + 30, publications)) %>%
  group_by(issue) %>% summarise(Mittelwert = mean(publications, na.rm = TRUE),
                                   Standardabweichung = sd(publications, na.rm = TRUE),
                                   Standardfehler = Standardabweichung/sqrt(n()),
                                   `5%` = Mittelwert - Standardfehler * 1.96,
                                   `95%` = Mittelwert + Standardfehler * 1.96,
                                   n = n()) %>% 
  ggplot(aes(issue, Mittelwert)) + 
  geom_pointrange(aes(ymin = `5%`, ymax = `95%`), size = 2.5, linewidth = 2) +
  scale_x_discrete(name = "Zeitraum",
                   labels = c("Biotech\n(n = 731)", "Neurowissenschaften\n(n=878)")) +
  labs(y = "Mittelwert der Publikationszahl",
       title = "Durchschnittliche Publikationszahl in verschiedenen Themenbereichen",
       subtitle = "Linien zeigen 95 %-Konfidenzintervalle") +
  geom_hline(yintercept = 135.5, linetype = "dashed", color = "red", linewidth = 2) +
  theme_minimal(base_size = 24)

## Um einen Hypothesentest visuell zu zeigen, wird als erstes eine Verteilungskurve
## erzeugt, die die Nullhypothese zeigt. Verwendet wird wie gehabt geom_function, unter
## mean wird der angenommene Mittelwert und unter sd die geschätzte Standardabweichung eingegeben.

ggplot() + xlim(60, 80) +
  geom_function(fun = dnorm, args = list(mean = 70, sd = 2), size = 1.5) +
  geom_vline(xintercept = 70, color = "red", size = 2) +
  annotate("text", x = 65.8, y = 0.05, label = "Angenommener Mittelwert",
           size = 8, color = "red") +
  scale_y_continuous(labels = NULL, name = NULL) +
  theme_minimal(base_size = 24) +
  labs(x = NULL, title = "Angenommene Stichprobenverteilung",
       subtitle = "Diese Verteilung der Stichprobenmittelwerte erwarten wir,\nwenn die Nullhypothese stimmt")

## Mit ggplot_build werden einzelne Punkte aus dieser Verteilung extrahiert, das wird
## benötigt, um den mittleren Bereich grau einzufärben. Es werden die Punkte verwendet,
## die zwischen den kritischen z-Werten für die mittleren 95 % der Verteilung liegen.

h0_plot <- ggplot() + xlim(55, 80) +
  geom_function(fun = dnorm, args = list(mean = 70, sd = 2), size = 1.5)

h0_data <- ggplot_build(h0_plot)$data[[1]] %>% select(x,y) %>% 
  filter(between(x, 66.08, 73.92)) %>%
  bind_rows(tibble(x = c(66.08, 66.08, 73.92, 73.92), y = c(0, dnorm(c(66.08, 73.92), 70, 2), 0)))

## Basierend auf diesen Daten wird mit geom_area die graue Fläche erzeugt. Mit geom_vline
## werden die Grenzen des Annahmebereichs eingezeichnet sowie die Linie, die den Wert
## in der Stichprobe zeigt.

h0_plot +
  geom_area(data = h0_data, aes(x,y), fill = "grey") +
  geom_vline(xintercept = 60, color = "red", size = 2) +
  geom_vline(xintercept = 66.08, linetype = "dashed", size = 2) +
  geom_vline(xintercept = 73.92, linetype = "dashed", size = 2) +
  labs(x = NULL, title = "Stichprobenmittelwert unterscheidet sich signifikant vom angenommenen Wert",
       subtitle = "Grau eingefärbter Bereich zeigt mittlere 95 % der Stichprobenverteilung.") +
  annotate("text", x = 56.8, y = 0.1,
           label = "Mittelwert der Stichprobe\np = 0,0000006",
           size = 8) +
  scale_y_continuous(labels = NULL, name = NULL) +
  theme_minimal(base_size = 24)

## Genauso gehen wir vor, um den Annahmebereich für ein Signifikanzniveau von 99 %
## einzuzeichnen.

h0_data_99 <- ggplot_build(h0_plot)$data[[1]] %>% select(x,y) %>% 
  filter(between(x, 64.85, 75.15)) %>%
  bind_rows(tibble(x = c(64.85, 64.85, 75.15, 75.15), y = c(0, dnorm(c(64.85, 75.15), 70, 2), 0)))

h0_plot +
  geom_area(data = h0_data_99, aes(x,y), fill = "grey") +
  geom_vline(xintercept = 60, color = "red", size = 2) +
  geom_vline(xintercept = 64.85, linetype = "dashed", size = 2) +
  geom_vline(xintercept = 75.15, linetype = "dashed", size = 2) +
  labs(x = NULL, title = "Stichprobenmittelwert unterscheidet sich signifikant vom angenommenen Wert",
       subtitle = "Grau eingefärbter Bereich zeigt mittlere 99 % der Stichprobenverteilung.") +
  annotate("text", x = 56.8, y = 0.1,
           label = "Mittelwert der Stichprobe\np = 0,0000006",
           size = 8) +
  scale_y_continuous(labels = NULL, name = NULL) +
  theme_minimal(base_size = 24)

## Das gleiche Prinzip, dieses Mal für einen Hypothesentest zu einem Anteilswert.

h0_plot2 <- ggplot() + xlim(0.12, 0.18) +
  geom_function(fun = dnorm, args = list(mean = 0.15, sd = 0.0077), size = 1.5)

h0_data2 <- ggplot_build(h0_plot2)$data[[1]] %>% select(x,y) %>% 
  filter(between(x, 0.135, 0.165)) %>%
  bind_rows(tibble(x = c(0.13499999, 0.165), y = c(0, 0)))

h0_plot2 +
  geom_area(data = h0_data2, aes(x,y), fill = "grey") +
  geom_vline(xintercept = 0.157, color = "red", size = 2) +
  geom_vline(xintercept = 0.135, linetype = "dashed", size = 2) +
  geom_vline(xintercept = 0.165, linetype = "dashed", size = 2) +
  labs(x = NULL, title = "Stichprobenanteilswert unterscheidet sich nicht signifikant vom angenommenen Wert",
       subtitle = "Grau eingefärbter Bereich zeigt mittlere 95 % der Stichprobenverteilung.") +
  annotate("text", x = 0.165, y = 40, size = 8,
           label = "Anteilswert in der Stichprobe\np = 0,35") +
  scale_y_continuous(labels = NULL, name = NULL) +
  theme_minimal(base_size = 24)

## Für einen einseitgen Test filtern wir nur mit einem kritischen Wert, um die grau
## eingefärbte Fläche zu erzeugen. Hier werden zwei Diagramme erzeugt, einmal mit dem
## gleichen Stichprobenkennwert wie zuvor, einmal mit einem, der weit links in der Verteilung liegt.

h0_data2_einseitig <- ggplot_build(h0_plot2)$data[[1]] %>% select(x,y) %>% 
  filter(x < 0.1626654) %>% bind_rows(tibble(x = c(0.12, 0.1626654), y = c(0, 0))) 
  
h0_plot2 +
  geom_area(data = h0_data2_einseitig, aes(x,y), fill = "grey") +
  geom_vline(xintercept = 0.157, color = "red", size = 2) +
  geom_vline(xintercept = 0.1626654, linetype = "dashed", size = 2) +
  labs(x = NULL, title = "Bei einseitigen Tests ist der Rückweisungsbereich auf einer Seite größer",
       subtitle = "Grau eingefärbter Bereich zeigt linke 95 % der Stichprobenverteilung.") +
  annotate("text", x = 0.165, y = 40, size = 8,
           label = "Anteilswert in der Stichprobe\np = 0,175") +
  scale_y_continuous(labels = NULL, name = NULL) +
  theme_minimal(base_size = 24)

h0_plot2 +
  geom_area(data = h0_data2_einseitig, aes(x,y), fill = "grey") +
  geom_vline(xintercept = 0.127, color = "red", size = 2) +
  geom_vline(xintercept = 0.1626654, linetype = "dashed", size = 2) +
  labs(x = NULL, title = "Unwahrscheinliche Werte auf der anderen Seite der Verteilung werden verpasst",
       subtitle = "Grau eingefärbter Bereich zeigt linke 95 % der Stichprobenverteilung.") +
  annotate("text", x = 0.135, y = 35, size = 8,
           label = "Anteilswert in der Stichprobe\np = 0,999") +
  scale_y_continuous(labels = NULL, name = NULL) +
  theme_minimal(base_size = 24)

## Fehler erster Art
## Zur Veranschaulichung des Fehlers erster Art werden wieder 10000mal zufällig 1000 Werte aus
## unserer Stichprobe gezogen und dann jeweils der Mittelwert berechnet. Aus diesen
## Mittelwerten werden 20 ausgewählt und als Punkte eingezeichnet.
## Die Punkte werden unterschiedlich eingefärbt, je nachdem, ob sie noch innerhalb des
## 95%-Konfidenzintervalls um den wahren Wert liegen. Dieses Intervall wird mit 
## zwei gestrichelten Linien gekennzeichnet, der wahre Mittelwert mit einer roten.
## Zuletzt werden die Achsen vertauscht.

set.seed(44)

stichproben_kennwerte <- map_dfr(1:10000, ~ wissenschaftliche_akteure %>% 
                                   slice_sample(n = 1000) %>%
                                   summarise(Mittelwert = mean(publications, na.rm = TRUE)))

set.seed(31)

stichproben_kennwerte %>% slice_sample(n = 20) %>%
  mutate(alpha_error = between(Mittelwert, 113.5963, 148.8763), index = row_number()) %>% 
  ggplot() + geom_point(aes(index, Mittelwert, color = alpha_error), size = 6) +
  geom_hline(yintercept = 113.5963, linetype = "dashed", size = 2) +
  geom_hline(yintercept = 148.8763, linetype = "dashed", size = 2) +
  geom_hline(yintercept = 131.2363, color = "red", size = 2) +
  scale_x_discrete(labels = NULL, name = NULL) +
  annotate("text", x = 10, y = 134.5, label = "Wahrer Mittelwert", size = 8) +
  annotate("text", x = 2, y = 115.5, label = "Nullhypothese wird abgelehnt", size = 8) +
  labs(x = "Stichprobenmittelwerte",
       title = "In 5 % der Stichproben wird die Nullhypothese fälschlich abgelehnt",
       subtitle = "Rote Linie ist wahrer Mittelwert, gestrichelte Linien markieren Annahmebereich") +
  coord_flip() + theme_minimal(base_size = 24) + theme(legend.position = "none")

## Fehler zweiter Art
## Für den Fehler zweiter Art wird erst ein Datensatz erzeugt, der den eingefärbten
## Bereich zeigt (also den Teil der Verteilung, in dem ein Fehler zweiter Art passiert).

shade_data <- tibble(x = c(0.125, seq(0.125, 0.165, length = 100), 0.165),
                     y = c(0, dnorm(seq(0.125, 0.165, length = 100), 0.18, 0.0084), 0))

## Dann werden zwei unterschiedliche Normalverteilungen mit geom_function erzeugt, die schwarze zeigt
## die angenommene Verteilung, die blaue die wahre. Mit einer gestrichelten Linie wird der
## kritische Wert für den Rückweisungsbereich der Verteilung gezeigt.

ggplot() + xlim(0.12, 0.22) + geom_function(fun = dnorm, args = list(mean = 0.15, sd = 0.0075), size = 2) +
  geom_function(fun = dnorm, args = list(mean = 0.18, sd = 0.0084), color = "#1f78b4", size = 2) +
  geom_area(data = shade_data, aes(x,y), fill = "#1f78b4", alpha = 0.4) +
  geom_vline(xintercept = 0.165, linetype = "dashed", size = 2) +
  annotate("text", x = 0.133, y = 40, size = 8,
           label = "Angenommene Stichprobenverteilung\nAnteilswert = 0,15") +
  annotate("text", x = 0.205, y = 35, size = 8, color = "#1f78b4",
           label = "Wahre Stichprobenverteilung\nAnteilswert = 0,18") +
  annotate("text", x = 0.19, y = 7, size = 8,
           label = "3,7 % der Stichprobenwerte liegen\nim Annahmebereich der Nullhypothese,\nobwohl eigentlich ein anderer Wert vorliegt.") +
  scale_y_continuous(name = NULL, labels = NULL) +
  labs(x = NULL, title = "Fehler zweiter Art: Nullhypothese wird beibehalten, obwohl sie nicht zutrifft",
         subtitle = "Gestrichelte Linie markiert Rückweisungsbereich, blau gefärbter Bereich Wahrscheinlichkeit des Fehlers zweiter Art") +
  theme_minimal(base_size = 24)

# Ebenso wird für den Vergleich von zwei Verteilungen vorgegangen, die näher beieinander
# liegen.

shade_data2 <- tibble(x = c(0.125, seq(0.125, 0.165, length = 100), 0.165),
                     y = c(0, dnorm(seq(0.125, 0.165, length = 100), 0.17, 0.0079), 0))

ggplot() + xlim(0.12, 0.21) + geom_function(fun = dnorm, args = list(mean = 0.15, sd = 0.0075), size = 2) +
  geom_function(fun = dnorm, args = list(mean = 0.17, sd = 0.0079), color = "#1f78b4", size = 2) +
  geom_area(data = shade_data2, aes(x,y), fill = "#1f78b4", alpha = 0.4) +
  geom_vline(xintercept = 0.165, linetype = "dashed", size = 2) +
  annotate("text", x = 0.133, y = 40, size = 8,
           label = "Angenommene Stichprobenverteilung\nAnteilswert = 0,15") +
  annotate("text", x = 0.192, y = 35, size = 8, color = "#1f78b4",
           label = "Wahre Stichprobenverteilung\nAnteilswert = 0,17") +
  annotate("text", x = 0.185, y = 7, size = 8,
           label = "26,3 % der Stichprobenwerte liegen\nim Annahmebereich der Nullhypothese,\nobwohl eigentlich ein anderer Wert vorliegt.") +
  scale_y_continuous(name = NULL, labels = NULL) +
  labs(x = NULL, title = "Fehler zweiter Art: Nullhypothese wird beibehalten, obwohl sie nicht zutrifft",
       subtitle = "Gestrichelte Linie markiert Rückweisungsbereich, blau gefärbter Bereich Wahrscheinlichkeit des Fehlers zweiter Art") +
  theme_minimal(base_size = 24)

## Als Beispiel für die Kommunikation eines Hypothesentests wird ein t-Test mit der 
## Funktion t.test duchgeführt. Zuerst werden dafür künstlich Daten für den Test erzeugt.
## Mit dem mu-Argument wird dann angegeben, mit welchem Mittelwert die Daten verglichen werden sollen.

smartphone_nutzung <- rnorm(100)
smartphone_nutzung <- 18.47847*(smartphone_nutzung-mean(smartphone_nutzung))/sd(smartphone_nutzung) + 59.5911

t.test(smartphone_nutzung, mu = 70)
