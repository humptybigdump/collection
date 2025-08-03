## Pakete einlesen

library(tidyverse)
library(tidygraph)
library(ggraph)
library(igraph)

## Datensätze einlesen - den Datensatz to_kit lese ich hier nicht ein, meiner
## Ansicht nach sind die darin enthaltenen Tweets auch in at_kit - könntet ihr
## aber nochmal prüfen.

load("twitter/preprocessed_data_tweets_kit.Rdata")
load("twitter/preprocessed_data_tweets_at_kit.Rdata")

## Datensatz mit allen erwähnten Accounts erstellen - um Doppler zu entfernen, sortiere ich erst 
## nach der Zahl der Follower und entferne dann Duplikate basierend auf der ID der Accounts.
## Das Sortieren mache ich, damit die aktuellste Version eines Accounts im Datensatz enthalten bleibt
## (unter der Annahme, dass es wahrscheinlicher ist, dass die Followerzahl ansteigt als abnimmt).

all_user_information <- bind_rows(user_information, user_information_at_kit)
all_user_information <- all_user_information %>% arrange(desc(public_metrics.followers_count)) %>% 
  dplyr::distinct(id, .keep_all = TRUE)

## Einige Kantenarten kann man problemlos aus den Tweet-Datensätzen entnehmen - wer durch das KIT
## erwähnt wurde, ist im Datensatz tweet_information_longer enthalten, hier entspricht jede
## Beobachtung einem Paar aus Autor (hier immer KIT) und erwähnter Account. Entsprechend kann man
## einfach diese beiden Spalten auswählen, zusätzlich füge ich die Information über die Kantenart hinzu.
## Ähnlich ist es beim Datensatz tweet_information_at_kit_longer, hier ist jede Beobachtung
## ein Paar aus Autor und erwähnter Account. Ich unterschiede hier zwei Kantentypen,
## je nachdem, ob das KIT erwähnt wurde, oder ein anderer Account, dafür nutze ich die ID des KIT.

kanten_erwaehnt_durch_kit <- select(tweet_information_longer, author_id, mention_id) %>% 
  mutate(kantentyp = "durch_kit_erwaehnt")
kanten_erwaehnten_kit <- select(tweet_information_at_kit_longer, author_id, mention_id) %>% 
  filter(mention_id == "91110697") %>% mutate(kantentyp = "erwaehnte_kit")
kanten_erwaehnt_sonstige_accounts <- select(tweet_information_at_kit_longer, author_id, mention_id) %>% 
  filter(mention_id != "91110697") %>% mutate(kantentyp = "erwaehnte_anderen_account")

## Komplizierter wird es eigentlich nur bei einer Kantenart, nämlich bei der Information,
## ob zwei Accounts gemeinsam erwähnt wurden. Um das etwas zu vereinfachen, schiebe ich
## hier einen Schritt ein, um die Top-Accounts zu identifizieren. Das könnt ihr auch schon früher (und anders) machen,
## wenn ich die letzte Besprechung richtig verstanden habe, habt ihr solche eine Liste ja schon.
## Ich habe hier erstmal beide oben erzeugten Kantendatensätze zusammengeführt und die Fälle ausgeschlossen,
## in denen die Variable für den erwähnten Account leer ist. Dann habe ich jeweils bestimmt,
## was die 100 Accounts sind, die am häufigsten erwähnt wurden oder andere erwähnt haben.

kanten_alle_erwaehnungen <- bind_rows(kanten_erwaehnt_durch_kit, kanten_erwaehnten_kit,
                                      kanten_erwaehnt_sonstige_accounts) %>% 
  rename(from = author_id, to = mention_id) %>% filter(!is.na(to))

top_accounts_from <- kanten_alle_erwaehnungen %>% count(from) %>% arrange(desc(n)) %>% slice_head(n = 100)
top_accounts_to <- kanten_alle_erwaehnungen %>% count(to) %>% arrange(desc(n)) %>% slice_head(n = 100)

## Hier wird es dann wie gesagt etwas komplizierter, vermutlich geht das auch noch eleganter.
## Ich erzeuge zunächst einen leeren Datensatz, in den später alle Paare von gemeinsam erwähnten
## Accounts eingefügt werden.

gemeinsame_erwaehnungen <- data.frame(from = "", to = "")[FALSE,]

## Dann gehe ich in einer Schleife alle Einträge der Variable entities.mentions
## in den Datensätzen tweet_information und tweet_information_at_kit durch.
## Zur Erinnerung: die einzelnen Einträge sind hier eigene Dataframes mit je vier Variablen.
## Wenn der Eintrag leer ist (also kein Account erwähnt wurde), gehe ich zum nächsten Eintrag.
## Wenn er nicht leer ist, wähle ich nur noch die Accounts aus, die in den Top-Accounts-Listen sind.
## Bleibt dann nur noch ein erwähnter Account übrig, geht es zum nächsten Eintrag.
## Bleiben mehrere, werden alle möglichen Zweier-Kombinationen der erwähnten Accounts bestimmt
## und an den oben erzeugten Datensatz angehängt.
## ACHTUNG: das dauert ziemlich lange, also nicht wundern, wenn R hier etwas braucht.

for(mentions in c(tweet_information$entities.mentions, tweet_information_at_kit$entities.mentions)){
  if (is.null(mentions)){
    next
  }
  mentions <- filter(mentions, id %in% c(top_accounts_from$from, top_accounts_to$to))
  if (nrow(mentions) <= 1){
    next
  }
  entity_combinations <- combn(mentions$id, 2, simplify = TRUE)
  entity_combinations <- as.data.frame(t(entity_combinations))
  gemeinsame_erwaehnungen <- rbind(gemeinsame_erwaehnungen, rename(entity_combinations, from = V1, to = V2))
}

## Danach vereine ich Doppler und erzeuge eine neue Variable, die angibt, wie oft diese Doppler vorkommen.
## Jetzt gibt es noch ein Problem: es gibt Fälle im Datensatz, in denen die gleichen Accounts in unterschiedlichen
## Kombinationen bei to und from vorkommen. Also: im ersten Fall steht Account A unter from
## und Account B unter to, im zweiten Fall ist es umgekehrt, A steht bei to und b bei from. Da wir es hier aber 
## nicht mit einer gerichteten Beziehung zu tun haben, sind das eigentlich auch Duplikate, die vereint werden
## müssen. Dafür gehe ich einen kleinen Umweg: ich erzeuge eine neue Spalte,
## in der ich die Werte aus beiden Spalten sortiert zu einem String verbinde, dann basierend
## auf dieser neu erstellten Spalte gruppiere und die Gewichte der gruppierten Fälle addiere.
## Da durch das Sortieren die neu erzeugte Spalte für die oben beschriebenen Fälle gleich ist,
## werden diese Duplikate vereint. Zuletzt trenne ich die neue Spalte wieder in zwei einzelne auf,
## damit wir wieder die from - to Struktur haben. Auch hier wird dann noch der Kantentyp ergänzt.

gemeinsame_erwaehnungen <- gemeinsame_erwaehnungen %>% count(from, to, name = "weight")
gemeinsame_erwaehnungen_cleaned <- gemeinsame_erwaehnungen %>% rowwise() %>% 
  mutate(ids_combined = paste(sort(c(from, to)), collapse = " - ")) %>% ungroup() %>% 
  group_by(ids_combined) %>% summarise(weight = sum(weight)) %>%
  separate(ids_combined, c("from", "to"), sep = " - ")

gemeinsame_erwaehnungen_cleaned <- gemeinsame_erwaehnungen_cleaned %>%
  mutate(kantentyp = "gemeinsam_erwaehnt")

## Bevor dieser Datensatz ebenfalls zu den anderen Kanten hinzugefügt wird, muss
## ich diese noch etwas filtern und Duplikate vereinen. Ich belasse nur die Kanten
## im Datensatz, in denen mindestens einer der Top-Accounts vorkommt, und erzeuge auch 
## hier eine weight-Variable, die angibt, wie oft eine Verbindung zwischen zwei Knoten
## vorkam.

kanten_alle_erwaehnungen_top_accounts <- kanten_alle_erwaehnungen %>%
  filter(from %in% c(top_accounts_from$from, top_accounts_to$to) & 
           to %in% c(top_accounts_from$from, top_accounts_to$to)) %>%  
  count(to, from, kantentyp, name = "weight")

## Dann werden auch die Paare gemeinsam erwähnter Accounts zu den Kanten hinzugefügt.

kanten_alle_erwaehnungen_top_accounts <- bind_rows(kanten_alle_erwaehnungen_top_accounts,
                                                   gemeinsame_erwaehnungen_cleaned)

## Jetzt kann das Netzwerk erzeugt werden. Ich gehe hier einen Umweg über igraph,
## da es mit tidygraph direkt nicht funktioniert hat. Beim Erzeugen des
## Netzwerks wähle ich auch bei den Knoten nur die Top-Accounts aus.
  
twitter_netzwerk_erwaehnungen_top_accounts <-
  graph_from_data_frame(kanten_alle_erwaehnungen_top_accounts,
                        vertices = all_user_information %>%
                          dplyr::relocate(id) %>%
                          filter(id %in% c(top_accounts_from$from, top_accounts_to$to)))

twitter_netzwerk_erwaehnungen_top_accounts <- as_tbl_graph(twitter_netzwerk_erwaehnungen_top_accounts)

## Das erzeugte Netzwerk beinhaltet 164 Accounts und 4700 Kanten. Für die kann man jetzt
## diverse Netzwerkmaße berechnen. Das Netzwerk ist relativ übersichtlich, allerdings
## ist die Zahl der Kanten für Visualisierungen fast schon zu hoch, hier könnte darüber
## nachgedacht werden, doch nicht alle Kantentypen gleichzeitig darzustellen oder noch weniger Accounts zu verwenden.

twitter_netzwerk_erwaehnungen_top_accounts <- twitter_netzwerk_erwaehnungen_top_accounts %>% activate(nodes) %>% 
  mutate(actor_degree = centrality_degree(weights = weight), actor_betweenness = centrality_betweenness(directed = TRUE, normalized = TRUE),
         actor_authority = centrality_authority(weights = weight), actor_eigen = centrality_eigen())

## Für die Netzwerkvisualisierung erzeuge ich zuerst ein Layout (mit einem automatisch
## ausgewählten Algorithmus, hier könntet ihr noch austesten, was sinnvoll ist) und 
## dann die Visualisierung. Ich habe eingebaut, dass die Linienart abhängig
## vom Kantentyp ist und die Breite der Linie abhängig vom Gewicht.
## Zudem habe ich die Namen der Accounts mit eingefügt, die Größe abhängig vom Knotengrad.
## Gerade bei den Linienarten sieht man allerdings nicht so viel, weil es zu viele sind.
## Bei der Visualisierung müsst ihr wie oben beschrieben etwas rumprobieren, was sinnvoll ist.

layout_network <- create_layout(twitter_netzwerk_erwaehnungen_top_accounts, layout = "igraph", algorithm = "nicely")

ggraph(layout_network) +
  geom_edge_parallel(aes(linetype = kantentyp, width = weight)) +
  geom_node_point() +
  geom_node_text(aes(label = name, size = actor_degree),
    color = "darkgrey") +
  theme(panel.background = element_rect(fill = "white"))

