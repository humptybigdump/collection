## R-Uebung Daten-IT-Kommunikation 16.11.20
##
## Zur Uebung der Inhalte vom Samstag sollen Sie selbst eine kleine Analyse durchfuehren. Wir wollen weiter mit den
## RKI-Daten arbeiten und unter anderem selbst die Inzidenz berechnen. Bitte vervollstaendigen Sie dafuer dieses Script. 
## In den Kommentaren wird jeweils erklaert, was in den einzelnen Schritten passieren soll.
## Teilweise sind schon Bestandteile des Codes vorgegeben, die Sie dann noch ergaenzen sollen.
## Wir nutzen in erster Linie die Funktionen, die Sie am Samstag kennen gelernt haben.

## 1. Vorbereitung

## Zuerst muessen wir sozusagen unseren Arbeitsplatz einrichten. Legen Sie dafuer zuerst fest, in welchem Ordner R arbeiten soll
## (also was Ihr Working Directory ist). Im naechsten Schritt lesen wir die Pakete ein, die wir fuer unsere Analyse brauchen.
## In diesem Fall ist das nur das tidyverse.

setwd("C:/Users/NP/Nextcloud/WMK/projects/2020_ditkomm/2020-11-16")
library(tidyverse)

## 2. Daten einlesen

## Als naechstes benoetigen wir die Daten, mit denen wir dann arbeiten wollen. In dieser Analyse kombinieren wir zwei verschiedene
## Datensaetze. Den ersten kennen Sie bereits, es ist der Datensatz mit den Corona-Fall- und Todeszahlen des RKI.
## Bitte lesen Sie zuerst diesen Datensatz ein.

corona_daten_rki <- read_csv("RKI_COVID19.csv")

## Eine kleine Bereinigung muessen wir noch machen. Lars hatte am Samstag erwaehnt, dass Berlin im RKI-Datensatz in einzelne Bezirke unterteilt ist.
## Wir benoetigen jedoch die Daten fuer den gesamten Landkreis. Deswegen codieren wir die IDs der Landkreise ein wenig um.
## Bei allen Berliner Bezirken (KreisId liegt zwischen 11000 und 12000) wird die 11000 als ID eingetragen.

corona_daten_rki <- corona_daten_rki %>% mutate(IdLandkreis = if_else(as.numeric(IdLandkreis) >= 11000 & as.numeric(IdLandkreis) < 12000,
                                                                      "11000",
                                                                      IdLandkreis))
  
## Als zweiten Datensatz benoetigen wir Angaben zu den Bevoelkerungszahlen in den Landkreisen, um damit berechnen zu koennen, wie
## die Fallzahlen im Verhaeltnis zur Bevoelkerungszahl sind. Dafuer verwenden wir Daten des Statistischen Bundeamtes.
## Die Tabelle finden Sie hier: https://www-genesis.destatis.de/genesis//online?operation=table&code=12411-0015&bypass=true&levelindex=1&levelid=1605515472033#abreadcrumb
## Wir haben Sie Ihnen aber auch in Ilias hochgeladen. Es gilt hier aehnliche Dinge zu beachten wie beim Beerendatensatz von Samstag.
## Einiges haben wir Ihnen schon eingetragen. Pruefen muessen Sie unter anderem noch, welche Zeichen hier fuer fehlende Werte stehen.

bevoelkerungsdaten <- read_csv2("12411-0015.csv", skip = 5, n_max = 476, na = c("-"), locale = locale(encoding = "latin1"))

## Wie beim Beerendatensatz haben die ersten beiden Spalten keinen Namen und wurden deswegen von R (bzw. tidyverse) wieder X1 und X2 genannt.
## Zudem sind vier Spalten im Datensatz mit Daten aus 2015 bis 2018, die wir fuer unsere Analyse nicht brauchen.
## Im naechsten Schritt wollen wir also nur bestimmte Spalten behalten und diese umbenennen.

bevoelkerungsdaten <- bevoelkerungsdaten %>% select(kreis_id = X1, landkreis = X2, bevoelkerung_2019 = `31.12.2019`)

## Jetzt sollte Ihr Datensatz nur noch drei Spalten haben, in denen sich die Kreis-ID, der Name des Landkreises und die Bevoelkerungszahl befindet.
## Bei genauerer Betrachtung des Datensatzes faellt noch etwas auf: in ein paar der Zeilen ist die Spalte bevoelkerung_2019 leer,
## bzw. es finden sich nur NAs. Das liegt daran, dass es im Laufe der Zeit Veraenderungen im Zuschnitt der Landkreise gegeben hat,
## durch die manche Landkreise weggefallen sind. Diese Zeilen wollen wir natuerlich nicht im Datensatz haben.
## Wir verwenden hier eine Funktion, die Sie noch nicht kennen. is.na() prueft, ob der Wert einer Spalte (der Name der Spalte steht dann in den Klammern) NA ist.
## Durch das Ausrufezeichen wird der Wert negiert, das heisst, es werden nur die Zeilen ausgewaehlt, deren Wert nicht NA ist. 

bevoelkerungsdaten <- bevoelkerungsdaten %>% filter(!is.na(bevoelkerung_2019))

## Jetzt sollte Ihr Datensatz noch aus 401 Beobachtungen bestehen. Das koennen Sie pruefen, indem Sie die Funktion length() verwenden.
## Wenn Sie die untere Befehlszeile ausfuehren, sollte R Ihnen 401 ausgeben.

length(bevoelkerungsdaten$bevoelkerung_2019)

## 3. Daten zusammenfuehren und analysieren

## Ziel der Analyse ist es wie gesagt, die Zahl der Faelle relativ zur Bevoelkerung zu berechnen.
## Dafuer muessen wir die beiden Datensaetze zusammenfuehren. Aber zuerst muessen wir den RKI-Datensatz etwas umformen,
## um die Zahl der Faelle pro Landkreis zu haben. Das haben wir am Samstag bereits gemacht.
## Zur Erinnerung: zuerst werden nur die Zeilen ausgewaehlt, in denen es keine Datenbereinigung gab, dann werden die Faelle nach Landkreisen gruppiert
## und die Fallzahlen je Landkreis summiert. Hier gibt es eine kleine Veraenderung zum Samstag, bei der Gruppierung soll die Spalte mit der ID der Landkreise
## verwendet werden und nicht der Name der Landkreise, weil wir die ID spaeter brauchen, um die Datensaetze zusammenzufuehren.

fallzahlen_LK <- corona_daten_rki %>% 
  filter(!(NeuerFall == -1)) %>% 
  group_by(IdLandkreis) %>% 
  summarise(Faelle = sum(AnzahlFall))

## Jetzt lernen Sie noch eine neue Funktion kennen, die wir brauchen, um zwei Datensaetze zusammenzufuehren: left_join.
## Wie fast immer im tidyverse heisst die Funktion so wie das, was sie macht. Sie verbindet zwei Datensaetze.
## Die Verwendung ist hier auch ganz einfach. Die Parameter der Funktion sind die Namen der beiden Datensaetze, die verbunden werden,
## sowie der Name der Spalten, die genutzt werden sollen, um die Verbindung durchzufuehren. Denn es braucht ja einen "Schluessel",
## durch den R weiss, welche Zeilen aus den beiden Datensaetzen zusammengehoeren. In unserem Fall ist dieser Schluessel die ID der Landkreise.
## Tragen Sie die Schluesselspalten beim Argument by ein.

fallzahlen_bevoelkerung_kombiniert <- left_join(bevoelkerungsdaten, fallzahlen_LK, by = c("kreis_id" = "IdLandkreis"))

## Jetzt sollten Sie einen Datensatz mit 4 Spalten haben: die ID und der Name des Landkreises, die Bevoelkerungszahl 2019 und die Zahl der Faelle.
## Und nun koennen wir zur richtigen Analyse kommen: wir wollen eine neue Spalte hinzufuegen, in der die Zahl der Faelle pro 100.000 Einwohner eines Landkreises steht.
## ueberlegen Sie sich, wie man diese Zahl berechnen kann.

fallzahlen_bevoelkerung_kombiniert <- fallzahlen_bevoelkerung_kombiniert %>% mutate(fallzahl_relativ = 100000*Faelle/bevoelkerung_2019)
view(fallzahlen_bevoelkerung_kombiniert)

## Zusatzaufgaben: wenn das bisher alles zu leicht fuer Sie war oder Sie noch ein wenig weiter rumprobieren wollen, gibt es noch ein paar kleine
## Zusatzaufgaben, fuer die Sie ein bisschen googeln und rumprobieren muessen. 
## 1. Versuchen Sie, sich die Zeile mit der hoechsten Fallzahl pro 100.000 Einwohner ausgeben zu lassen. Und die mit der geringsten.
## 2. Pruefen Sie, ob es einen Unterschied zwischen Landkreisen mit hoher und niedriger Bevoelkerungszahl gibt.
## Hierfuer koennen Sie mit dem Pipe-Operator verschiedene Befehle verketten. Waehlen Sie zuerst nur die Landkreise mit einer Bevoelkerung
## ueber 250.000 aus und lassen Sie sich den Mittelwert und den Median der relativen Fallzahl ausgeben. Machen Sie dann das gleiche noch einmal fuer Landkreise
## mit einer Bevoelkerung unter 250.000 und vergleichen Sie die Werte. (Inhaltlich macht das natuerlich wenig Sinn, aber es geht hier nur ums ueben.)
## 3. Fuehren Sie die gleiche Analyse wie fuer die Fallzahlen auch fuer die Todeszahlen durch.

## 1. Aufgabe
## Es gibt verschiedene Möglichkeiten: arrange() sortiert einen Datensatz anhand einzelner Spalten, mit desc() absteigend,
## ohne aufsteigend. So sieht man, welche Landkreise den höchsten Wert haben.

arrange(fallzahlen_bevoelkerung_kombiniert, desc(fallzahl_relativ))
arrange(fallzahlen_bevoelkerung_kombiniert, fallzahl_relativ)

## Man kann auch filter() verwenden, um die Zeile anzeigen zu lassen, die den höchsten (oder niedrigsten) Wert 
## bei der relativen Fallzahl hat. Dafür verwendet man die Funktion max() oder min().

filter(fallzahlen_bevoelkerung_kombiniert, fallzahl_relativ == max(fallzahl_relativ))
filter(fallzahlen_bevoelkerung_kombiniert, fallzahl_relativ == min(fallzahl_relativ))

## 2. Aufgabe
## Auch hier gibt es verschiedene Möglichkeiten, um zum Ziel zu kommen. Man kann einerseits eine neue Variable erzeugen,
## die angibt, ob ein Landkreis groß oder klein ist. Dafür wird if_else verwendet. Wenn der Wert bei bevoelkerung_2019
## kleiner ist als 250.000 (oder gleich), wird "kleiner Landkreis" eingetragen, ansonsten "großer Landkreis".
## Dann kann man nach dem Wert der neu erzeugten Variable gruppieren und mit summarise Mittelwert und Median ausgeben lassen.

fallzahlen_bevoelkerung_kombiniert %>% mutate(kreis_groesse = if_else(bevoelkerung_2019 <= 250000, "kleiner Landkreis", "großer Landkreis")) %>% 
  group_by(kreis_groesse) %>% summarise(durchschnitt_fallzahl_relativ = mean(fallzahl_relativ), median_fallzahl_relativ = median(fallzahl_relativ))

## Oder aber man teilt es in zwei Schritte auf und verwendet jeweils filter. Dann filtert man im ersten Fall,
## ob die Bevölkerung kleiner gleich 250.000 ist und rechnet für die gefilterten Daten den Mittelwert und Median aus,
## im zweiten Schritt dann für die Landkreise mit mehr als 250.000 Einwohnern.

fallzahlen_bevoelkerung_kombiniert %>% filter(bevoelkerung_2019 <= 250000) %>% 
  summarise(durchschnitt_fallzahl_relativ = mean(fallzahl_relativ), median_fallzahl_relativ = median(fallzahl_relativ))

fallzahlen_bevoelkerung_kombiniert %>% filter(bevoelkerung_2019 > 250000) %>% 
  summarise(durchschnitt_fallzahl_relativ = mean(fallzahl_relativ), median_fallzahl_relativ = median(fallzahl_relativ))

## 3. Aufgabe
## An sich muss hier nur die gleiche Analyse durchgeführt werden, wie bei der Fallzahl. Allerdings muss berücksichtigt
## werden, dass es hier auch den Wert -9 gibt, der dafür steht, dass: "Fall ist weder in der aktuellen Publikation noch
## in der des Vortages ein Todesfall". Das sind also die Genesenen. Das heißt, für die Summe müssen nur die Zeilen summiert
## werden, in denen NeuerTodesfall 0 oder 1 ist.

todeszahlen_lk <- corona_daten_rki %>% 
  filter(NeuerTodesfall == 0 | NeuerTodesfall == 1) %>% 
  group_by(IdLandkreis) %>% 
  summarise(Todesfaelle = sum(AnzahlTodesfall))

## Wenn man sich den neu erzeugten Datensatz anschaut, fällt auf, dass er nur 397 Zeilen hat.
## Anscheinend gibt es noch vier Landkreise in Deutschland, in denen es bisher keinen Todesfall durch Corona gab.

todeszahlen_bevoelkerung_kombiniert <- left_join(bevoelkerungsdaten, todeszahlen_lk, by = c("kreis_id" = "IdLandkreis"))

## Deswegen gibt es in der Tabelle mit den kombinierten Daten jetzt 4 Zeilen, in denen bei Todesfaelle NA steht.
## Hier konnte R beim Zusammenführen nichts eintragen. Wir tragen hier jetzt 0 ein, damit die Daten sauberer sind.
## Dafür wird die Funktion replace_na() verwendet.

todeszahlen_bevoelkerung_kombiniert <- todeszahlen_bevoelkerung_kombiniert %>% replace_na(list(Todesfaelle = 0))

## Jetzt können wir wieder die relative Zahl der Todefälle pro 100.000 Einwohner berechnen.

todeszahlen_bevoelkerung_kombiniert <- todeszahlen_bevoelkerung_kombiniert %>% mutate(todeszahl_relativ = 100000*Todesfaelle/bevoelkerung_2019)
view(todeszahlen_bevoelkerung_kombiniert)

## Jetzt könnte man sich wieder die Landkreise mit den höchsten Werten anzeigen lassen etc.

arrange(todeszahlen_bevoelkerung_kombiniert, desc(todeszahl_relativ))
