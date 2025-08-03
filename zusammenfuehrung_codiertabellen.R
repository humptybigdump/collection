setwd("/Users/belakoch/Library/Mobile Documents/com~apple~CloudDocs/Documents/Uni/Karlsruhe/Masterarbeit/Datenerhebung/ma_koch/Auswertung/Arbeitsdateien/")

library(shiny)
library(shinyjs)
library(shinyFiles)
library(shinyWidgets)
library(bslib)
library(tidyverse)

codierung_app <- readRDS("actors_from_dpa_artikel_gesammelt.RDS")
codierung_manuell <- read.csv2("coding_sheet_artikelebene_KOCH.csv")

codierung_gesamt <- left_join(codierung_app, codierung_manuell, by = "document_id")
view(codierung_gesamt)


codierung_gesamt <- 
  codierung_gesamt %>% 
    mutate(STRUCT = if_else(STUDY == "1" & FORM == "1",
                                  "TRUE", 
                                  "FALSE"))

write_rds(codierung_gesamt,file="codierung_gesamt.RDS")
write_csv2(codierung_gesamt,file="codierung_gesamt.csv")
