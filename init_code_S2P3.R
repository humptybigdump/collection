# set your working directory using
# setwd("C:/.../.../...")
# or make sure to put the files containing the data in the default directory
# getwd()

#### load 2003 data
# speed data is in miles per hour
# directional data is in clockwise degrees, where 0 indicates north 
# (dir <= 180 means easterly winds, dir > 180 means westerly winds)

vs.data <- matrix(scan("Vansycle03.hr.dat",sep=""), ncol=4, byrow=T)
kw.data <- matrix(scan("Kennewick03.hr.dat",sep=""), ncol=4, byrow=T)
gh.data <- matrix(scan("Goodnoe_Hills03.hr.dat",sep=""), ncol=4, byrow=T)

# dates
dates <- as.Date("2002-12-31") + vs.data[, 1]

# hours
hr <- as.integer(vs.data[, 2])

# converts from miles per hour to meters per second 
const <- 1609/3600

# station names repeated for each hour in a year
# Vansycle, Kennewick, Goodnoe Hills
station.names <- c("VS", "KW", "GH")
station <- factor(rep(station.names, each = 365*24), levels = station.names)


## Location data
# Vansycle
vs.data <- data.frame(date = dates, hr = hr,
                      speed = vs.data[, 3] * const, dir = vs.data[, 4])
# Kennewick
kw.data <- data.frame(date = dates, hr = hr,
                      speed = kw.data[, 3] * const, dir = kw.data[, 4])
# Goodnoe Hills
gh.data <- data.frame(date = dates, hr = hr,
                      speed = gh.data[, 3] * const, dir = gh.data[, 4])


## Variable data
# speed
spd.data <- data.frame(date = dates, hr = hr,
                       VS = vs.data$speed, KW = kw.data$speed, GH = gh.data$speed)

# direction
dir.data <- data.frame(date = dates, hr = hr,
                       VS = vs.data$dir, KW = kw.data$dir, GH = gh.data$dir)


## Complete data
all.data <- data.frame(station = station, date = dates, hr = hr,
                       speed = c(spd.data$VS, spd.data$KW, spd.data$GH),
                       dir = c(dir.data$VS, dir.data$KW, dir.data$GH))

# display the first few lines of a data.frame
head(vs.data)
# get a statistical summary of a data.frame
summary(vs.data)
# get the structure of the R object
str(vs.data)

