#Set your working directory using
#setwd("C:/.../.../...")
#or make sure to put the files containing the data in the default directory
#getwd()

#Load data after having downloaded the files from the Johns Hopkins Universities GitHub-Repo
#Note: You need to download the entire repository at https://github.com/CSSEGISandData/COVID-19
# to access individual files. The files are located in the subfolder /csse_covid_19_data/csse_covid_19_time_series/
data_us = read.csv("time_series_covid19_deaths_US.csv")
data_global = read.csv("time_series_covid19_deaths_global.csv")

#Take a look at the datasets
View(data_us)
View(data_global)

#Some variables you might want to use
col_metadata_us = 1:12
col_tsd_us = (1:ncol(data_us))[-col_metadata_us]

col_metadata_global = 1:4
col_tsd_global = (1:ncol(data_global))[-col_metadata_global]
col_country = 2

n_days = length(col_tsd_us)
dates = as.Date("2020-01-21") + 1:n_days 

#Hint: For some countries you may need to sum over all regions belonging to that country
#for the US
colSums(data_us[col_tsd_us])
#for China
colSums(data_global[data_global[col_country] == "China",col_tsd_global])
