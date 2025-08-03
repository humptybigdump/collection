## R-Uebung Daten-IT-Kommunikation 16.11.20
##
## Zur Uebung der Inhalte vom Samstag sollen Sie selbst eine kleine Analyse durchfuehren. Wir wollen weiter mit den
## RKI-Daten arbeiten und selbst die Inzidenz berechnen. Bitte vervollstaendigen Sie dafuer dieses Script. 
## In den Kommentaren wird jeweils erklaert, was in den einzelnen Schritten passieren soll.
## Teilweise sind schon Bestandteile des Codes vorgegeben, die Sie dann noch mit Funktionsnamen, Parametern etc. ergaenzen sollen.
## Wir nutzen in erster Linie die Funktionen, die Sie am Samstag kennen gelernt haben.

##########################################
############ 1. Vorbereitung #############

## Zuerst muessen wir sozusagen unseren Arbeitsplatz einrichten. Legen Sie dafuer zuerst fest, in welchem Ordner R arbeiten soll
## (also was ihr Working Directory ist). Im naechsten Schritt lesen wir die Pakete ein, die wir fuer unsere Analyse brauchen.
## In diesem Fall ist das nur das tidyverse.




##############################################
###### 2. Daten einlesen und bereinigen ######

## Als naechstes benoetigen wir die Daten, mit denen wir dann arbeiten wollen. In dieser Analyse kombinieren wir zwei verschiedene
## Datensaetze. Den ersten kennen Sie bereits, es ist der Datensatz mit den Corona-Fall- und Todeszahlen des RKI. Sie finden die
## aktuelle Version in Ilias. Bitte lesen Sie zuerst diesen Datensatz ein.

corona_daten_rki <- 

## Eine kleine Bereinigung muessen wir noch machen. Lars hatte am Samstag erwaehnt, dass Berlin im RKI-Datensatz in einzelne Bezirke unterteilt ist.
## Wir benoetigen jedoch die Daten fuer den gesamten Landkreis. Deswegen codieren wir die IDs der Landkreise ein wenig um.
## Bei allen Berliner Bezirken (KreisId liegt zwischen 11000 und 12000) wird die 11000 als ID eingetragen. Hier muessen Sie nichts ergaenzen.

corona_daten_rki <- corona_daten_rki %>% mutate(IdLandkreis = if_else(as.numeric(IdLandkreis) >= 11000 & as.numeric(IdLandkreis) < 12000,
                                                                      "11000",
                                                                      IdLandkreis))

## Als zweiten Datensatz benoetigen wir Angaben zu den Bevoelkerungszahlen in den Landkreisen, um damit berechnen zu koennen, wie
## die Fallzahlen im Verhaeltnis zur Bevoelkerungszahl sind. Dafuer verwenden wir Daten des Statistischen Bundeamtes.
## Die Tabelle finden Sie hier: https://www-genesis.destatis.de/genesis//online?operation=table&code=12411-0015&bypass=true&levelindex=1&levelid=1605515472033#abreadcrumb
## Wir haben Sie Ihnen aber auch in Ilias hochgeladen. Es gilt hier aehnliche Dinge zu beachten wie beim Beerendatensatz von Samstag.
## Einiges haben wir Ihnen schon eingetragen. Pruefen muessen Sie unter anderem noch, welche Zeichen hier fuer fehlende Werte stehen.

bevoelkerungsdaten <- (, skip = 5, n_max = 476, na = c(), locale = locale(encoding = ))

## Wie beim Beerendatensatz haben die ersten beiden Spalten keinen Namen und wurden deswegen von R (bzw. tidyverse) wieder X1 und X2 genannt.
## Zudem sind vier Spalten im Datensatz mit Daten aus 2015 bis 2018, die wir fuer unsere Analyse nicht brauchen.
## Im naechsten Schritt wollen wir also nur bestimmte Spalten behalten und diese umbenennen.

bevoelkerungsdaten <- (kreis_id = , landkreis = , bevoelkerung_2019 = `31.12.2019`)

## Jetzt sollte Ihr Datensatz nur noch drei Spalten haben, in denen sich die Kreis-ID, der Name des Landkreises und die Bevoelkerungszahl befindet.
## Bei genauerer Betrachtung des Datensatzes faellt noch etwas auf: in ein paar der Zeilen ist die Spalte bevoelkerung_2019 leer,
## bzw. es finden sich nur NAs. Das liegt daran, dass es im Laufe der Zeit Veraenderungen im Zuschnitt der Landkreise gegeben hat,
## durch die manche Landkreise weggefallen sind. Diese Zeilen wollen wir natuerlich nicht im Datensatz haben.
## Wir verwenden hier eine Funktion, die Sie noch nicht kennen. is.na() prueft, ob der Wert einer Spalte (der Name der Spalte steht dann in den Klammern) NA ist.
## Durch das Ausrufezeichen wird der Wert negiert, das heisst, es werden nur die Zeilen ausgewaehlt, deren Wert nicht NA ist. 

bevoelkerungsdaten <- (!is.na())

## Jetzt sollte Ihr Datensatz noch aus 401 Beobachtungen bestehen. Das koennen Sie pruefen, indem Sie die Funktion length() verwenden.
## Wenn Sie die untere Befehlszeile ausfuehren, sollte R Ihnen 401 ausgeben.

length(bevoelkerungsdaten$bevoelkerung_2019)

##############################################
## 3. Daten zusammenfuehren und analysieren ##

## Ziel der Analyse ist es wie gesagt, die Zahl der Faelle relativ zur Bevoelkerung zu berechnen.
## Dafuer muessen wir die beiden Datensaetze zusammenfuehren. Aber zuerst muessen wir den RKI-Datensatz etwas umformen,
## um die Zahl der Faelle pro Landkreis zu haben. Das haben wir am Samstag bereits gemacht.
## Zur Erinnerung: zuerst werden nur die Zeilen ausgewaehlt, in denen es keine Datenbereinigung gab, dann werden die Faelle nach Landkreisen gruppiert
## und die Fallzahlen je Landkreis summiert. Hier gibt es eine kleine Veraenderung zum Samstag, bei der Gruppierung soll die Spalte mit der ID der Landkreise
## verwendet werden und nicht der Name der Landkreise, weil wir die ID spaeter brauchen, um die Datensaetze zusammenzufuehren.
## Bitte ergaenzen Sie die fehlenden Funktionsnamen und Werte der Parameter.

fallzahlen_LK <- corona_daten_rki %>% 
  (!(NeuerFall == )) %>% 
  group_by() %>% 
  (Faelle = sum())

## Jetzt lernen Sie noch eine neue Funktion kennen, die wir brauchen, um zwei Datensaetze zusammenzufuehren: left_join.
## Wie fast immer im tidyverse heisst die Funktion so wie das, was sie macht. Sie verbindet zwei Datensaetze.
## Die Verwendung ist hier auch ganz einfach. Die Parameter der Funktion sind die Namen der beiden Datensaetze, die verbunden werden,
## sowie der Name der Spalten, die genutzt werden sollen, um die Verbindung durchzufuehren. Denn es braucht ja einen "Schluessel",
## durch den R weiss, welche Zeilen aus den beiden Datensaetzen zusammengehoeren. In unserem Fall ist dieser Schluessel die ID der Landkreise.
## Tragen Sie die Schluesselspalten beim Argument by ein.

fallzahlen_bevoelkerung_kombiniert <- left_join(bevoelkerungsdaten, fallzahlen_LK, by = c("" = ""))

## Jetzt sollten Sie einen Datensatz mit 4 Spalten haben: die ID und der Name des Landkreises, die Bevoelkerungszahl 2019 und die Zahl der Faelle.
## Und nun koennen wir zur richtigen Analyse kommen: wir wollen eine neue Spalte hinzufuegen, in der die Zahl der Faelle pro 100.000 Einwohner eines Landkreises steht.
## ueberlegen Sie sich, wie man diese Zahl berechnen kann.

fallzahlen_bevoelkerung_kombiniert <- (fallzahl_relativ = )
view(fallzahlen_bevoelkerung_kombiniert)

##########################################
############ Zusatzaufgaben ##############


## Wenn das bisher alles zu leicht fuer Sie war oder Sie noch ein wenig weiter rumprobieren wollen, gibt es noch ein paar kleine
## Zusatzaufgaben, fuer die Sie ein bisschen googeln und rumprobieren muessen. 
## 1. Versuchen Sie, sich die Zeile mit der hoechsten Fallzahl pro 100.000 Einwohner ausgeben zu lassen. Und die mit der geringsten.
## 2. Pruefen Sie, ob es einen Unterschied zwischen Landkreisen mit hoher und niedriger Bevoelkerungszahl gibt.
## Hierfuer koennen Sie mit dem Pipe-Operator verschiedene Befehle verketten. Waehlen Sie zuerst nur die Landkreise mit einer Bevoelkerung
## ueber 250.000 aus und lassen Sie sich den Mittelwert und den Median der relativen Fallzahl ausgeben. Machen Sie dann das gleiche noch einmal fuer Landkreise
## mit einer Bevoelkerung unter 250.000 und vergleichen Sie die Werte. (Inhaltlich macht das natuerlich wenig Sinn, aber es geht hier nur ums ueben.)
## 3. Fuehren Sie die gleiche Analyse wie fuer die Fallzahlen auch fuer die Todeszahlen durch.