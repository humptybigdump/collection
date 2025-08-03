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
  daily_country_data <-
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

# Z3-6 
# Z 8-10 
# Z13-22 
# Z26-41 
# Z43-58 
# Z60-66 

## Was ist der aktuelle Speicherstand aller deutschen Speicher zusammen?




## Wie viel Gas war an den einzelnen Tagen des Novembers 2024 in Deutschland gespeichert?




## Die gleiche Tabelle aber jetzt mit dem aktuellsten Datum zuerst




## Wie viel Gas war an den Tagen im November 2023 jeweils in der EU gespeichert?




## Erstelle eine Tabelle mit den Speicherständen von Deutschland, Österreich und der EU in je einer Spalte



