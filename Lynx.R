######################################
# Annual number of lynx trappings    #
# MacKenzie river, Canada, 1821-1934 #
# R package datasets: "lynx"         #
######################################

# rm( list=ls() )
# source("Lynx")

# pdf()

# par(mfrow = c(3,1))

ts.plot(lynx, main="Annual Number of Lynx Trappings")
lynx <- log(lynx)
ts.plot(lynx, main="Log Annual Number of Lynx Trappings")
acf(lynx, lag.max=30)

# par(mfrow=c(2,2))

spectrum(lynx)                  # raw periodogram
spectrum(lynx,spans=3)          # smoothed 
spectrum(lynx,spans=c(3,3))     # smoothed 
spectrum(lynx,spans=c(3,5))     # smoothed 

# dev.off()
