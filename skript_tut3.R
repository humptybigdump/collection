if(rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

# Packages laden (Folie 5)
library(tidyverse)

# Datensatz laden (Folie 6)
data("starwars")
starwars

# Datensätze untersuchen (Folie 8)
tail(starwars)
str(starwars)

# Auswahl aus Datensätze (Folie 9)
starwars[80, "name"]
starwars$hair_color

# Datensatz importieren (Folie 10)
deutsche_charts <- read_csv2("chartsger.CSV", na = "-77")

# Bereinigung von Datensätzen (Folie 14)
deutsche_charts
## Filter
deutsche_charts %>% 
  mutate(height = height / 100)
## Select
deutsche_charts %>% 
  select(gender, haircol)