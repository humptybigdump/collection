#Set your working directory using
#setwd("C:/.../.../...")
#or make sure to put the files containing the data in the default directory
#getwd()

#Load data after having downloaded the files from the Johns Hopkins Universities GitHub-Repo
#Note: You need to download the entire repository at https://github.com/CSSEGISandData/COVID-19
# to access individual files. The time series files are located in the subfolder 
#/csse_covid_19_data/csse_covid_19_time_series/
data = read.csv("time_series_covid19_deaths_global.csv") #Note: Contains time series for the US as well!

#Take a look at the datasets
View(data)

#Some variables you might want to use
col_metadata = 1:4
col_tsd = (1:ncol(data))[-col_metadata]
col_country = 2

n_days = length(col_tsd)
dates = as.Date("2020-01-21") + 1:n_days 

#Hint: For some countries you may need to sum over all regions belonging to that country
#e.g. for China
# colSums(data[data[col_country] == "China",col_tsd])

##############################################################################
# Proposed Solution

options(scipen=999) # avoid scientific notation on plot axes

#install.package("RColorBrewer)
library(RColorBrewer) # Offers color palettes
# par(mar=c(3,4,2,2))
# display.brewer.all() # Shows available color palettes
# par(mar = c(5, 4, 4, 2) + 0.1) # default

plot.xyz = function(countries,labels = NA,write_to_pdf = FALSE){ 
  #input must be lists of vectors 
  #allows to accumulate deaths within multiple countries, see use with global/EU below
  
  if(write_to_pdf) pdf(file = "plot_S1P4.pdf",width = 15,height = (3*length(countries))) 
  
  if(is.na(labels[[1]])) labels = countries
  
  par(mfrow = c(length(countries),3),mar=c(3,4,2,2))
  
  trafo_y = function(x) x - c(0,x)[1:n_days]
  trafo_z = function(x) log(x/c(0,x)[1:n_days])
  
  colors = brewer.pal(9,"Set1")
  
  for(i in 1:length(countries)){
    country = countries[[i]]
    color = colors[(i-1) %% length(colors) + 1]
    x = colSums(data[data$Country.Region %in% country,-col_metadata])
    y = trafo_y(x)
    z = trafo_z(x)
    
    for(ts in c("x","y","z")){
      plot(dates,get(ts),type = "l",col = color,xlab = "Date",ylab = ts)
      if(ts == "x") legend("topleft",
                          legend = labels[[i]],
                          col = c(color,rep("white",length(country)-1)),lwd=1,lty=1)
    }
  }
  
  if(write_to_pdf) dev.off()
  else par(mfrow = c(1,1),mar=c(5, 4, 4, 2) + 0.1) #restores default values
}

global = levels(data$Country.Region)
global
plot.xyz(list(global),list("global"))

countries = list("China","Germany","US")
plot.xyz(countries)

plot.xyz(list("Austria","France","Italy"))
plot.xyz(list("Spain","Switzerland","United Kingdom"))

EU = list("Austria","Belgium","Bulgaria","Croatia","Cyprus","Czechia","Denmark","Estonia","Finland",
       "France","Germany","Greece","Hungary","Ireland","Italy","Latvia","Lithuania",
       "Luxembourg","Malta","Netherlands","Poland","Portugal","Romania","Slovakia",
       "Slovenia","Spain","Sweden")

plot.xyz(list(unlist(EU),c(unlist(EU),"United Kingdom","Switzerland")),
         list("EU","EU+UK+CH"))

# use following lines for writting all plots to pdf
plot.xyz(list(unlist(global),"China","Germany","US","Austria","France","Italy","Spain","Switzerland",
              "United Kingdom",unlist(EU),c(unlist(EU),"Switzerland","United Kingdom")),
         list("global","China","Germany","US","Austria","France","Italy","Spain","Switzerland",
              "United Kingdom","EU","EU+UK+CH"),write_to_pdf = TRUE)


