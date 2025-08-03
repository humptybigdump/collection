
## Standortauswertung Klimatologische Messverfahren
## Beispiel Graben-Station

#install required library and load library
install.packages("readxl")
library("readxl")

#set working directory
setwd("C:/Users/Mauder-M/Documents/lectures/KIT/IfGG/SS2020/KlimaMessverfahren/Daten")
getwd()

#read data from file
my_data <- read_excel("LoeP-UGK-Graben.xlsx", sheet = 4, skip = 1)
#my_data <- read_excel("LoeP-UGK-NO_Mh.xlsx", sheet = 4, skip = 1)

#select observation period of one year (1.7.2019-31.06.2020)
my_data <- my_data[(51343:104046),]

#calculate yearly averages 
T2m_y <- mean(my_data$`T(2m), °C`, na.rm = TRUE)
T2m_sd <- sd(my_data$`T(2m), °C`, na.rm = TRUE)

#calculate monthly averages
my_data$Month <- months(my_data$`Datum Zeit`) #  get months

T2m_m <- aggregate( `T(2m), °C` ~ Month, my_data , mean )
T2m_sd <- aggregate( `T(2m), °C` ~ Month, my_data , sd )

#calculate vapor from rH using the Magnus formula after Sonntag
my_data$e <- my_data$`RH(2m), %`/100 * 6.1078 * exp(17.08085*my_data$`T(2m), °C`/(234.175+my_data$`T(2m), °C`))

#calculate daily averages, min, max
my_data$Date <- as.Date(my_data$`Datum Zeit`) # get date

T2m_day_m <- aggregate( `T(2m), °C` ~ Date, my_data , mean )
T2m_day_min <- aggregate( `T(2m), °C` ~ Date, my_data , min )
T2m_day_max <- aggregate( `T(2m), °C` ~ Date, my_data , max )

# determine special climatological days
Eistage <- which(T2m_day_max$`T(2m), °C` < 0)
kalte_Tage <- which(T2m_day_max$`T(2m), °C` < 10)
Sommertage <- which(T2m_day_max$`T(2m), °C` >= 25)
heisse_Tage <- which(T2m_day_max$`T(2m), °C` >= 30)
  
Heiztage <- which(T2m_day_m$`T(2m), °C` < 15)
Vegetationsperiode <- which(T2m_day_m$`T(2m), °C` >= 5)
  
Frosttage <- which(T2m_day_min$`T(2m), °C` < 0)

length(Eistage)
length(kalte_Tage)
length(Sommertage)
lengt(heisse_Tage)
length(Heiztage)
length(Vegetationsperiode)
length(Frosttage)
