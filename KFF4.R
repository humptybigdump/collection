
setwd("~/Lio Dokumente/Studium/Master Karlsruhe/2. Semester/Klimafolgenforschung 4/Daten_2019/Graswang/",sep ="")
library(lubridate)

Graswang_19 <- read.csv2(file = "Graswang_TK2_result_2019.csv", sep = ",", dec = ".")
headers <- read.csv2("Graswang_TK2_result_header.csv", sep = ",", colClasses = "character", check.names = F)
colnames(Graswang_19)[1:61] <- colnames(headers)
Graswang_19$T_begin <- strptime(Graswang_19[,1],"%d.%m.%Y %H:%M","GMT")
time <- Graswang_19$T_begin
#gc(T)

Slow_Response <- read.csv2("Gra_M_2019.csv", sep = ",", dec = ".", header = F)[,c(1,11,59)]
SR <- Slow_Response
colnames(SR) <- c("time","rH","Rad")

SR$time <- strptime(SR$time,format='%Y-%m-%d %H:%M:%S',tz = 'GMT')
slow_response30 <- aggregate.data.frame(SR,
                                        list('slow30min' = cut(SR$time,breaks = '30 mins')),
                                        mean , na.rm = T)
sr30 <- slow_response30[,c(1,3,4)]
#c(-1,-nrow(slow_response30))


 rm.na <- function(data){
   data[data == -9999.9003906] <- NA
   data
 }
 
 NEE <- rm.na(Graswang_19$`NEE[mmol/(m2*s)]`)
 Temp <- rm.na(Fendt_19[,6])
 Lat <- rm.na(Fendt_19$LvE.W.m2.)
 


# plot(time,NEE, type = "l")
# plot(time,Temp,type = "l")
# plot(time,Lat,type = "l")
# 
# install.packages("REddyProc")
# library(REddyProc)

station <- Graswang_19


Year <- year(station$T_begin)
DoY <- yday(station$T_begin)
Hour <- hour(station$T_begin)+as.numeric(minute(station$T_begin)/60)
NEE <- station$`NEE[mmol/(m2*s)]`*1000
LE <- station$`LvE[W/m2]`
H <- station$`HTs[W/m2]`
Rg <- sr30$Rad ###
Tair <- station$`Ts[deg C]`
#soils <- data.frame(slow_response30$soiltemp02mid,slow_response30$soiltemp06mid,slow_response30$soiltemp12mid,slow_response30$soiltemp25mid,slow_response30$soiltemp35mid,slow_response30$soiltemp50mid)
#Tsoil <- rowMeans(soils,na.rm = T)

rH <- sr30$rH
VP <- (station$`a[g/m3]` * (station$`Ts[deg C]`+273.15))/0.21667
VPD <- 6.112*VP^((17.62*station$`Ts[deg C]`)/(243.12+station$`Ts[deg C]`))
Ustar <- station$`ustar[m/s]`

#gap_filling <- data.frame(Year,DoY,Hour,NEE,LE,H,Rg,Tair,Tsoil,rH,VPD,Ustar)
gap_filling <- data.frame(Year,DoY,Hour,NEE,LE,H,Rg,Tair,rH,VPD,Ustar)

gap_filling[gap_filling < - 9000] <- -9999
units <- c('-','-','-','umol-2s-1','Wm-2','Wm-2','Wm-2','degC','degC','%','hPa','ms-1')
gap_filling <- rbind(units,gap_filling)

write.table(gap_filling,"Gapfilling_Graswang_19_input.txt",sep='\t',quote=F,row.names = F)


