
# Pakete laden
library(tidyverse) # Sammlung an Paketen aus dem tidyerse
library(DT) # coole interaktive Tabellen

## Daten einlesen

# viele Probleme
Strauchbeeren <- read_csv2(
  "41232-0002_de_modified.csv"
)


# encodingprobleme Die Datei ist latin1 kodiert
Beeren <-
  read_csv2(
    "41232-0002_de_modified.csv",
    skip = 7,
    n_max = 28,
    locale = locale(encoding = "latin1")
  )


# Die fehlenden Werte (NA) werden noch nicht korrekt angezeigt
Beeren <-
  read_csv2(
    "41232-0002_de_modified.csv",
    skip = 7,
    n_max = 28,
    col_names = FALSE,
    locale = locale(encoding = "latin1")
  )
view(Beeren)

# korrekte Variante
Beeren <- read_csv2("41232-0002_de_modified.csv", 
                    skip = 7, # die ersten 7 Zeilen werden uebersprungen
                    n_max = 335, # es werden nur 335 Zeilen eingelesen
                    col_names = FALSE, # Der ausgewaehlte Bereich enthaelt keine Ueberschriften
                    locale = locale(encoding = "latin1"), # Auswahl der latin1 Codierung
                    na = c("-", "x", ".")) # Bestimmte Zeichen werden als fehlende Werte festgelegt
view(Beeren)


## select (rename)
## Auswahl von Variablen
Beeren <- 
  Beeren |>
  rename(
    Jahr = X1,
    Anbauform = X2,
    Art = X3,
    Anzahl_Betriebe = X4,
    Anbauflaeche_ha = X6,
    Erntemenge_t = X8
  )

select(Beeren, "Art", "Anbauflaeche_ha")
Beeren |> select("Art", "Anbauflaeche_ha")


## mutate
## Neue Variablen und Variablen verändern

Beeren <-
  Beeren |>
  mutate(Anbauform = if_else(Anbauform == "Freiland",
                             "Freiland",
                             "Schutzabdeckung"))

Beeren <-
  Beeren |>
  mutate(Anbauform = if_else(Anbauform == "Unter hohen begehbaren Schutzabdeckungen",
                             "Schutzabdeckung",
                             Anbauform))

## pivot_wider pivot_longer

Beeren_wide <-
  Beeren |>
  select(-c(X5, X7, X9)) |> 
  pivot_wider(
    names_from = Anbauform,
    values_from = c("Anzahl_Betriebe",
                    "Anbauflaeche_ha",
                    "Erntemenge_t")
  )

Beeren_wide |>
  mutate(Anzahl_Betriebe_gesamt =
           Anzahl_Betriebe_Freiland +
           if_else(
             is.na(Anzahl_Betriebe_Schutzabdeckung),
             0,
             Anzahl_Betriebe_Schutzabdeckung
           )) |>
  view()


Beeren_long <-
  Beeren |>
  select(-c(X5, X7, X9)) |> 
  pivot_longer(-c(Art, Anbauform, Jahr),
               names_to = "Metrik",
               values_to = "Wert")


## zurück zu mutate


### Durchschnittliche Betriebsgroesse
Beeren |> 
  mutate(durchschnittliche_Betriebsgroesse_ha = Anbauflaeche_ha / Anzahl_Betriebe) |> 
  view()


### Gesamternte im wide-Datensatz
Beeren_wide |> 
  mutate(Anzahl_Betriebe = Anzahl_Betriebe_Freiland + Anzahl_Betriebe_Schutzabdeckung, 
         Anbauflaeche_ha = Anbauflaeche_ha_Freiland + Anbauflaeche_ha_Schutzabdeckung, 
         Erntemenge_t = Erntemenge_t_Freiland + Erntemenge_t_Schutzabdeckung) |> 
  select(Jahr, Art, Anzahl_Betriebe, Anbauflaeche_ha, Erntemenge_t)

Beeren_wide |> 
  mutate(across(where(is.numeric), ~if_else(is.na(.), 0, .))) |> 
  mutate(Anzahl_Betriebe = Anzahl_Betriebe_Freiland + Anzahl_Betriebe_Schutzabdeckung, 
         Anbauflaeche_ha = Anbauflaeche_ha_Freiland + Anbauflaeche_ha_Schutzabdeckung, 
         Erntemenge_t = Erntemenge_t_Freiland + Erntemenge_t_Schutzabdeckung) |> 
  select(Jahr, Art, Anzahl_Betriebe, Anbauflaeche_ha, Erntemenge_t)


## summarize
## group_by

Beeren |> 
  summarise(Gesamternte = sum(Erntemenge_t, na.rm = TRUE)) # Berechnung der Summe unter Missachtung der fehlenden Werte (na.rm = TRUE)

Beeren |> 
  group_by(Art, Jahr) |> 
  summarise(Summe = sum(Erntemenge_t, na.rm = TRUE)) |> view()

## besser: .by-Parameter
Beeren |> 
  summarise(Summe = sum(Erntemenge_t, na.rm = TRUE), .by = c("Art", "Jahr"))


## filter

Beeren |> filter(Jahr == "2018")

Beeren |> filter(Jahr == "2018" & Anbauform == "Freiland")

Beeren |> filter(Art != "Insgesamt")
Beeren |> filter(!(Art == "Insgesamt"))

## Tabellen mit datatable()

Beeren |> filter(Jahr == "2018" & Anbauform == "Freiland") |> datatable()

## Andere Datenformate

Beeren_flat <-
  read_csv2(
    "41232-0002_de_flat.csv",
    locale = locale(encoding = "latin1"),
    na = c("-", "x", ".")
  )
