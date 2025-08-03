### KOSTRA DATEN

## Arbeitsverzeichnis setzen
# setwd('...')


 # Referenzen für Koordinaten
 #  https://opendata.dwd.de/climate_environment/CDC/grids_germany/return_periods/precipitation/KOSTRA/KOSTRA_DWD_2010R/asc/BESCHREIBUNG_gridsgermany_return_periods_precipitation_KOSTRA_KOSTRA_DWD_2010R_asc_de.pdf"
 
 # Rasterzelle für Karlsruhe (49.014°, 8.4043°)
 ID_KA <- 81021
 
 # Rasterzelle für Weiherbach 49.1158°, 8.7405°
 ID_WB <- 80024

 
 library(curl)

 # Zielverzeichnis
 ziel <- "Kostra"

 # ASCII Data
   url = "https://opendata.dwd.de/climate_environment/CDC/grids_germany/return_periods/precipitation/KOSTRA/KOSTRA_DWD_2010R/asc/"
 # GIS Data
 #  url = "https://opendata.dwd.de/climate_environment/CDC/grids_germany/return_periods/precipitation/KOSTRA/KOSTRA_DWD_2010R/gis/"
   h = new_handle(dirlistonly=TRUE)
   con = curl(url, "r", h)
   tbl = read.table(con,  fill=TRUE)     #   stringsAsFactors=TRUE,
   head(tbl)
   close(con)

   # Prepare filelist  - zip files for each event duration
    filelist <- grep("csv",tbl[,2], value=T)   # ASCII
     #  filelist <- grep("zip",tbl[,2], value=T)   # GIS
    filelist <- sapply(strsplit(filelist, '"'), function(x) x[2])

  # download
 erg <- sapply(filelist, function(x) curl_download(paste0(url, x), file.path(ziel,x))   )

 # auspacken
 erg <- sapply(file.path(ziel,filelist), unzip, exdir=ziel)
 # AUfräumen: DElete all downloaded zip files
  unlink(file.path(ziel,filelist))

 ###
 #  access data (ASCII)
 filelist <- dir(ziel, pattern="csv")

 kostradat <-  lapply(file.path(ziel,filelist), read.csv, na.strings="-99.9", sep=";")

 # Daten ziehen für einzelne Zellen
 kadat <- sapply(kostradat, function(x) x[x$"INDEX_RC"==ID_KA,-1])
 wbdat <- sapply(kostradat, function(x) x[x$"INDEX_RC"==ID_WB,-1])

 dauerstufen <- as.numeric(substr( filelist, 26,29))/ 60 # Minuten -> Stunden


# Grafiken
  matplot(dauerstufen,t(kadat),  col=1:4, t="l", xlab="N-Dauer /h", ylab="N-Summe /mm",
          main="KOSTRA-2010R für Karlsruhe")
   legend("bottomright", paste(as.numeric(substr(rownames(kadat), 4,6)) ,"a"),
                         col=1:4, lty= 1:5, bty="n", title=expression("Jährlichkeiten T"[n]), ncol=3)

  x11()
  matplot(dauerstufen,t(wbdat),  col=1:4, t="l", xlab="N-Dauer /h", ylab="N-Summe /mm",
          main="KOSTRA-2010R für Weiherbach")
   legend("bottomright", paste(as.numeric(substr(rownames(wbdat), 4,6)) ,"a"),
                         col=1:4, lty= 1:5, bty="n", title=expression("Jährlichkeiten T"[n]), ncol=3)
  