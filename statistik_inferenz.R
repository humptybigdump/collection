## Beispiele für die Sitzung Zufallsstichprobe und Schaetzen

## Hier benötigen wir nur die Pakete aus dem tidyverse

library(tidyverse)

## Standardnormalverteilung
## Über geom_function kann man Kurven oder andere Diagramme basierend auf einer mathematischen
## Funktion erzeugen. Als Funktion verwende ich hier dnorm, womit die Dichteverteilung einer 
## Normalverteilung ausgegeben wird. Damit ein Diagramm erzeugt wird, musste ich vorher
## über xlim angeben, für welchen Wertebereich ich die Dichte haben möchte, hier für Werte
## zwischen -5 und 5.

ggplot() + xlim(-5, 5) + geom_function(fun = dnorm, linewidth = 1.5) +
  labs(title = "Normalverteilung", y = "Häufigkeitsdichte") +
  theme_minimal(base_size = 24)

## Viele Normalverteilungen
## Nach dem gleichen Prinzip wie oben erzeuge ich hier mehrere Normalverteilungen.
## Über args wird jeweils angegeben, wie der Mittelwert und die Standardabweichung der
## Verteilung sein soll.
## Über annotate füge ich die Beschriftungen hinzu, die Farben für die Beschriftung und die 
## Texte habe ich als Hexadezimalcode angegeben.

ggplot() + xlim(-5, 5) +
  geom_function(color = 'black', fun = dnorm, args = list(mean = 0, sd = 1), linewidth = 1.5) +
  geom_function(color = '#984ea3', fun = dnorm, args = list(mean = 1, sd = 1), linewidth = 1.5) +
  geom_function(color = '#1f78b4', fun = dnorm, args = list(mean = 0, sd = 0.5), linewidth = 1.5) +
  geom_function(color = '#33a02c', fun = dnorm, args = list(mean = 0, sd = 2), linewidth = 1.5) +
  annotate("text", x = 1.6, y = 0.7, label = "Mittelwert = 0, sd = 0.5",
           color = '#1f78b4', size = 8) +
  annotate("text", x = 3, y = 0.3, label = "Mittelwert = 1, sd = 1",
           color = '#984ea3', size = 8) +
  annotate("text", x = -2.1, y = 0.3, label = "Mittelwert = 0, sd = 1",
           color = 'black', size = 8) +
  annotate("text", x = -3.8, y = 0.1, label = "Mittelwert = 0, sd = 2",
           color = '#33a02c', size = 8) +
  theme_minimal(base_size = 24) +
  labs(x = NULL, y = "Häufigkeitsdichte",
       title = "Normalverteilungen mit unterschiedlichen Parametern")

## Erstes Beispiel: Smartphonenutzung. Ich erstelle zuerst die fiktive Grundgesamtheit
## der WMK-Studierenden, 300 Personen, für die ich einen Mittelwert von 65 und
## eine Standardabweichung von 20 annehme. Dafür nutze ich die Funktion rnorm,
## mit der man zufällig Werte aus einer Normalverteilung auswählen kann.
## Über set.seed wird ein Startpunkt für die Zufallsauswahl festgelegt, sodass bei jedem
## Ausführen das gleiche Ergebnis herauskommt.

set.seed(64)

grundgesamtheit_studierende <- rnorm(n = 300, mean = 65, sd = 20) %>% round()

## Jetzt ziehe ich aus dieser Grundgesamtheit eine Stichprobe von 20 Fällen,
## dafür nutze ich die sample-Funktion.

set.seed(2)

stichprobe_studierende <- sample(grundgesamtheit_studierende, size = 20)

## Jetzt kann ich Eigenschaften der Stichprobe beschreiben, beispielsweise den Mittelwert
## Dieser unterscheidet sich von dem in der Grundgesamtheit.

mean(stichprobe_studierende)
sd(stichprobe_studierende)

## Wenn ich weitere Stichproben auswähle, ist der Mittelwert jeweils leicht unterschiedlich.
## Das heißt, je nach Stichprobe komme ich zu leicht anderen Einschätzungen, was den
## Mittelwert der Grundgesamtheit betrifft.

mean(sample(grundgesamtheit_studierende, size = 20))
mean(sample(grundgesamtheit_studierende, size = 20))
mean(sample(grundgesamtheit_studierende, size = 20))
mean(sample(grundgesamtheit_studierende, size = 20))
mean(sample(grundgesamtheit_studierende, size = 20))
mean(sample(grundgesamtheit_studierende, size = 20))
mean(sample(grundgesamtheit_studierende, size = 20))

## Um die Stichprobenverteilung nachzubilden, ziehen wir 1000 Stichproben aus der
## Grundgesamtheit und schauen uns an, wie die Mittelwerte der Stichproben sich verteilen

stichprobenmittelwerte <- sapply(1:1000, function(x) mean(sample(grundgesamtheit_studierende, size = 20)))

tibble(stichprobenmittelwert = stichprobenmittelwerte) %>% 
  ggplot(aes(stichprobenmittelwert)) +
    geom_histogram(aes(y = after_stat(density)), color = "white") +
    labs(title = "Stichprobenverteilung für 1000 Stichproben mit n = 20",
       x = "Stichprobenmittelwert", y = "Häufigkeitsdichte") +
    theme_minimal(base_size = 20)

## Zweites Beispiel - Daten zur öffentlichen Präsenz von Expert:innen
## Datensatz einlesen - liegt als CSV-Datei im Ordner data vor

wissenschaftliche_akteure <- read_csv("data/codierung_wissenschaftliche_akteure.csv")

## Wir tun so, als wären unsere Daten die Grundgesamtheit und ziehen daraus 10000
## Mal eine Zufallsstichprobe mit 1000 Personen. Für jede Stichprobe wird der Mittelwert
## der Publikationszahl berechnet und daraus dann eine Visualisierung erstellt.
## Der wahre Mittelwert ist 131.
## Slice_sample ist die Funktion zur Auswahl von
## von 1000 zufälligen Einträgen, mit map_dfr wird das 10.000mal ausgeführt.
## So wird ein Datensatz erzeugt, in der jede Zeile die Eigenschaften einer Zufallsstichprobe
## enthält (also den Mittelwert). Diese 10.000 Mittelwerte können wir dann visualisieren.

set.seed(44)

stichproben_kennwerte <- map_dfr(1:10000, ~ wissenschaftliche_akteure %>% 
                                   slice_sample(n = 1000) %>%
                                   summarise(Mittelwert = mean(publications, na.rm = TRUE)))

stichproben_kennwerte %>% summarise(Standardfehler = sqrt(sum((Mittelwert-mean(Mittelwert))^2)/9999),
                                    Q1 = quantile(Mittelwert, c(0.25)),
                                    Q3 = quantile(Mittelwert, c(0.75)),
                                    `95%` = quantile(Mittelwert, c(0.95)),
                                    `5%` = quantile(Mittelwert, c(0.05)),
                                    Median = median(Mittelwert),
                                    Mittelwert = mean(Mittelwert))

## Über geom_vline füge ich die Linie für den wahren Mittelwert ein, mit geom_function
## die Normalverteilungskurve zum Vergleich.

stichproben_kennwerte %>% {ggplot(., aes(Mittelwert)) +
    geom_histogram(aes(y = after_stat(density)), fill = "lightgrey", color = "black") +
    geom_function(fun = dnorm, linetype = "dashed", linewidth = 1.5,
                  args = list(mean = mean(.$Mittelwert), sd = sd(.$Mittelwert))) +
    geom_vline(xintercept = 131, color = "red", linetype = "dashed", linewidth = 1.5) +
    annotate("text", x = 137, y = 0.01, label = "Wahrer Mittelwert",
             color = "red", size = 8) +
    labs(title = "Stichprobenverteilung für 10000 Stichproben mit n = 1000",
         subtitle = "Gestrichelte Linie ist Normalverteilung zum Vergleich",
         x = "Stichprobenmittelwert", y = "Häufigkeitsdichte") +
    theme_minimal(base_size = 20)}

stichproben_kennwerte %>% {ggplot(., aes(Mittelwert)) +
    geom_density(linewidth = 1.5) +
    geom_function(fun = dnorm, linetype = "dashed", linewidth = 1.5,
                  args = list(mean = mean(.$Mittelwert), sd = sd(.$Mittelwert))) +
    geom_vline(xintercept = 131, color = "red", linetype = "dashed", linewidth = 1.5) +
    annotate("text", x = 137, y = 0.01, label = "Wahrer Mittelwert",
             color = "red", size = 8) +
    labs(title = "Stichprobenverteilung für 10000 Stichproben mit n = 1000",
         subtitle = "Gestrichelte Linie ist Normalverteilung zum Vergleich",
         x = "Stichprobenmittelwert", y = "Häufigkeitsdichte") +
    theme_minimal(base_size = 20)}

## Konfidenzintervalle
## Um Konfidenzintervalle zu erläutern, erzeuge ich zunächst einen kleinen Datensatz
## mit verschiedenen Punkten, die dann in der Normalverteilung verortet werden. Für die
## Punkte werden mit xmin und xmax die 95%-Konfidenzintervalle eingezeichnet, daher wird jeweils
## 1,96 addiert oder subtrahiert.

points <- tribble(
  ~x, ~y, ~xmin, ~xmax,
  0, dnorm(0), 0 -1.96, 0 + 1.96,
  1.96, dnorm(1.96), 1.96 - 1.96, 1.96 + 1.96,
  2.2, dnorm(2.2), 2.2 - 1.96, 2.2 + 1.96,
  -1.3, dnorm(-1.3), -1.3 - 1.96, -1.3 + 1.96,
  -4, 0.01, -5, -4 + 1.96
)

## Dann wird wieder eine Standardnormalverteilung erzeugt und drei vertikale Linien
## eingefügt, eine rote für den Mittelwert und zwei schwarze für die kritischen Werte
## der mittleren 95 % der Verteilung. Mit geom_pointrange werden dann die Punkte aus 
## dem oben erzeugten Datensatz eingefügt

ggplot(points) + xlim(-5,5) + geom_function(fun = dnorm, size = 2) +
  geom_vline(xintercept = 1.96, linetype = "dashed", size = 2) +
  geom_vline(xintercept = - 1.96, linetype = "dashed", size = 2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 2) +
  geom_pointrange(aes(x = x, y = y, xmin = xmin, xmax = xmax), size = 2, color = "darkgrey") +
  labs(x = NULL, y = NULL,
       title = "95 %-Konfidenzintervalle für verschiedene Werte der Standardnormalverteilung",
       subtitle = "Gestrichelte Linien zeigen Mittelwert und kritische Werte für 95 % der Verteilung") +
  theme_minimal(base_size = 20)

## Interpretation von Konfidenzintervallen
## Um die Funktion von Konfidenzintervallen zu erklären, wird hier für 20 Stichproben gezeigt,
## ob ihr Konfidenzintervall den wahren Wert überdeckt. Dafür berechne ich zuerst den Standardfehler
## der Mittelwerte und darauf basierend dann die Konfidenzintervalle.

set.seed(46)

standard_error <- sd(wissenschaftliche_akteure$publications, na.rm = TRUE)/sqrt(1000)

mittelwerte_konfidenzintervalle <- stichproben_kennwerte %>% slice_sample(n = 20) %>%
  mutate(row_number = row_number(), upper = Mittelwert + 1.96 * standard_error,
         lower = Mittelwert - 1.96 * standard_error)

## Für die Abbildung werden die Mittelwerte aus 20 Stichproben als Punkte visualisiert und
## dann die Konfidenzintervalle über geom_errorbar. Mit geom_hline wird der wahre Mittelwert
## eingezeichnet (als rote, gestrichelte Linie) und dann die Achsen vertauscht.

ggplot(mittelwerte_konfidenzintervalle, aes(row_number, Mittelwert)) +
  geom_point(size = 6) +
  geom_errorbar(aes(ymin = lower, ymax = upper), size = 1.5) +
  geom_hline(yintercept = 131, linetype = "dashed", color = "red",
             size = 2) +
  scale_x_discrete(name = NULL, breaks = NULL) +
  labs(title = "95 %-Konfidenzintervalle von 20 Stichprobenmittelwerten",
       subtitle = "Bei einer Stichprobe überdeckt das Intervall nicht den wahren Wert",
       y = "Stichprobenmittelwert der Publikationszahl") +
  coord_flip() + theme_minimal(base_size = 20)

## Darunter wird das gleiche noch einmal mit 68 %-Konfidenzintervallen gemacht (also
## nur ein Standardfehler auf den Mittelwert addiert/davon abgezogen).

ggplot(mittelwerte_konfidenzintervalle, aes(row_number, Mittelwert)) +
  geom_point(size = 6) +
  geom_errorbar(aes(ymin = Mittelwert - standard_error,
                    ymax = Mittelwert + standard_error), size = 1.5) +
  geom_hline(yintercept = 131, linetype = "dashed", color = "red",
             size = 2) +
  scale_x_discrete(name = NULL, breaks = NULL) +
  labs(title = "68 %-Konfidenzintervalle von 20 Stichprobenmittelwerten",
       subtitle = "Bei sieben Stichproben überdeckt das Intervall nicht den wahren Wert",
       y = "Stichprobenmittelwert der Publikationszahl") +
  coord_flip() + theme_minimal(base_size = 20)

## Konfidenzintervalle visuell
## Zuerst wird die Geschlechtsvariable in einen Faktor umgewandelt, um die Reihenfolge
## der Ausprägungen in der Abbildung steuern zu können. Für die Abbildung wird dann
## ein Balkendiagramm für die Anteile der einzelnen Ausprägungen erstellt und mit
## geom_errorbar Fehlerbalken hinzugefügt.

wissenschaftliche_akteure <- wissenschaftliche_akteure %>%
  mutate(gender = factor(gender,
                         levels = c("männlich", "weiblich", "anderes/nicht bestimmbar")))

wissenschaftliche_akteure %>% count(gender) %>% mutate(share = n/sum(n)) %>% 
  ggplot(aes(gender, share, fill = gender)) +
  geom_col() +
  geom_errorbar(aes(ymin = share - 1.96 * sqrt(share * (1-share)/nrow(wissenschaftliche_akteure)),
                    ymax = share + 1.96 * sqrt(share * (1-share)/nrow(wissenschaftliche_akteure))),
                width = 0.5, size = 1.5) +
  labs(x = "Geschlecht", y = "Anteil an allen Forschenden",
       title = paste0("Geschlechterverteilung der Forschenden (n = ",
                      nrow(wissenschaftliche_akteure), ")"),
       subtitle = "Mit 95 %-Konfidenzintervallen") +
  scale_fill_brewer(palette = "Paired") +
  theme_bw() + theme(legend.position = "none", text = element_text(size = 24))

## Ähnlich wird es hier auch gemacht, nur, dass anstelle von geom_col und geom_errorbar
## geom_pointrange verwendet wird.

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

