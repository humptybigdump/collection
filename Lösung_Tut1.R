#Package installieren und laden
install.packages("readxl")
install.packages("readr")
library(readxl)
library(readr)

#Datensatz chartsger importieren
  #Excel
  chartsger <- read_excel("C:/Users/ueneu/Documents/Tutorium/Vorbereitung/Datensätze/chartsger.xlsx", na = "-77")
  #CSV
  chartsger <- read_csv2("C:/Users/ueneu/Documents/Tutorium/Vorbereitung/Datensätze/chartsger.CSV", trim_ws = TRUE, na = "-77")