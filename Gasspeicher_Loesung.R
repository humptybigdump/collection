# data from https://agsi.gie.eu/

library(tidyverse) # Sammlung an Paketen aus dem tidyerse
library(RColorBrewer) # sinnvolle Farbpaletten
library(DT) # coole interaktive Tabellen
library(SMChelpR) # Paket zum Zugriff auf SMC-Infrastruktur

if(rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

# load the data from the file Gasspeicherdaten.RData
try(load(file.path("data", "Gasspeicherdaten.RData")))
try(dir.create("data"))

# check if the data is already loaded
reload_data <- !exists("daily_country_data")

# check if the data is up to date
tmp <- try(max(daily_country_data$date) < today() - 2)

if(is.logical(tmp)){reload_data <- tmp}


# if the data is not up to date, reload the data
if(reload_data){
  daily_country_data <- # Kommentar
    GraphQL_get_table_vec(
      tabellenname = "data_agsi_gasspeicher_gas_storage_by_country",
      variablen = c("_id", "country", "country_code", "date", "gas_in_storage", 
                    "injection", "status", "withdrawal", "working_gas_volume", 
                    "withdrawal_capacity", "injection_capacity", "status"),
      datenserver = "https://data.smclab.io/v1/graphql"
    )  |> 
    mutate(date = as.Date(date),
           Jahr = year(date),
           gas_in_storage = as.numeric(gas_in_storage),
           working_gas_volume = as.numeric(working_gas_volume))
  
  save(daily_country_data, file = file.path("data", "Gasspeicherdaten.RData"))
}

Laendernamen <- 
  tibble(country = 
           c("Croatia", "Czech Republic", "Denmark", "France", 
             "Germany", "Hungary", "Ireland", "Italy", 
             "Latvia", "Netherlands", "Poland",  "Portugal", 
             "Romania", "Slovakia", "Spain", "United Kingdom (Pre-Brexit)", 
             "Belgium", "Bulgaria", "Austria", "Sweden", 
             "Serbia", "Ukraine", "United Kingdom (Post-Brexit)"), 
         Land = 
           c("Kroatien", "Tschechische Republik", "Dänemark", "Frankreich", 
             "Deutschland", "Ungarn", "Irland", "Italien", 
             "Lettland", "Niederlande", "Polen", "Portugal", 
             "Rumänien", "Slowakei", "Spanien", "Vereinigtes Königreich (Pre-Brexit)", 
             "Belgien", "Bulgarien", "Österreich", "Schweden", 
             "Serbien", "Ukraine", "Vereinigtes Königreich (Post-Brexit)"), 
         EU = c(rep(TRUE, 20), rep(FALSE, 3)))

daily_country_data <- 
  daily_country_data |> 
  rename(
    Datum = date, 
    `Gespeichertes Gas \n in TWh` = gas_in_storage, 
    `Speicherkapazität in TWh` = working_gas_volume) |> 
  left_join(Laendernamen, by = "country")


################
### Aufgaben ###
################

## Was machen die ersten 66 Zeilen dieses Skriptes?

# Z3-6 Pakete laden
# Z 8-10 Working Directory im manuellen Betrieb setzen
# Z13-22 Versuch lokale Daten zu laden und auf Aktualität zu überprüfen.
# Z26-41 Wenn lokale Daten veraltet oder nicht existent, dann ziehe neue Daten vom Server
# Z43-58 Hilfsdatensatz deutsche Ländernamen
# Z60-66 Umbenennung von Variablen und Integration des Hilfsdatensatzes

## Was ist der aktuelle Speicherstand aller deutschen Speicher zusammen?

daily_country_data |> 
  filter(Land == "Deutschland", 
         Datum == max(Datum)) |> 
  select(Land, Datum, `Gespeichertes Gas \n in TWh`) |> 
  datatable()

## Wie viel Gas war an den einzelnen Tagen des Novembers 2023 in Deutschland gespeichert?

daily_country_data |> 
  filter(Land == "Deutschland", 
         Datum >= as.Date("2023-11-01"),
         Datum < as.Date("2023-12-01")) |> 
  select(Land, Datum, `Gespeichertes Gas \n in TWh`) |> 
  datatable()

daily_country_data |> 
  filter(Land == "Deutschland", 
         Datum %in% seq.Date(as.Date("2023-11-01"), as.Date("2023-11-30"), "1 day")) |> 
  select(Land, Datum, `Gespeichertes Gas \n in TWh`) |> 
  datatable()

daily_country_data |> 
  filter(Land == "Deutschland", 
         floor_date(Datum, "month") == as.Date("2023-11-01")) |> 
  select(Land, Datum, `Gespeichertes Gas \n in TWh`) |> 
  datatable()


## Die gleiche Tabelle aber jetzt mit dem aktuellsten Datum zuerst

daily_country_data |> 
  filter(Land == "Deutschland", 
         floor_date(Datum, "month") == as.Date("2023-11-01")) |> 
  select(Land, Datum, `Gespeichertes Gas \n in TWh`) |> 
  arrange(desc(Datum)) |> 
  datatable()

## Wie viel Gas war an den Tagen im November 2023 jeweils in der EU gespeichert?

daily_country_data |> 
  filter(EU, 
         floor_date(Datum, "month") == as.Date("2023-11-01")) |> 
  summarise(`Gespeichertes Gas \n in TWh` = sum(`Gespeichertes Gas \n in TWh`, na.rm = TRUE), .by = "Datum") |> 
  select(Datum, `Gespeichertes Gas \n in TWh`) |> 
  arrange(desc(Datum)) |> 
  datatable()

## Erstelle eine Tabelle mit den Speicherständen von Deutschland, Österreich und der EU in je einer Spalte

EU_fuellstaende <- 
  daily_country_data |> 
  filter(EU, 
         floor_date(Datum, "month") == as.Date("2023-11-01")) |> 
  summarise(EU = sum(`Gespeichertes Gas \n in TWh`, na.rm = TRUE), .by = "Datum") |> 
  select(Datum, EU)

daily_country_data |> 
  filter(Land %in% c("Deutschland", "Österreich"), 
         floor_date(Datum, "month") == as.Date("2023-11-01")) |> 
  select(Land, Datum, `Gespeichertes Gas \n in TWh`) |> 
  pivot_wider(names_from = Land, values_from = `Gespeichertes Gas \n in TWh`) |> 
  full_join(EU_fuellstaende, by = "Datum") |> 
  arrange(desc(Datum)) |> 
  datatable()
