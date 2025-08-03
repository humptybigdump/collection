# Material for Forecast Challenge 2021
# Script illustrating the construction of an exemplary forecast

# Loading the data
# Install package RCurl to load data directly from GitHub
# install.packages("RCurl")
library(RCurl)
# Load raw data from JHU GitHub https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv
data_raw = read.csv(text = getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"))

# The counries of interest with names used by JHU
countries = c("Argentina", "Bangladesh", "Brazil", "Canada", "Germany", "India", "Japan",
              "Mexico", "Russia", "Sweden", "Switzerland", "Turkey", "US")

col_metadata = 1:4 # Columns containing meta data
col_tsd = (1:ncol(data_raw))[-col_metadata] # Columns containing time series data (cumulative case numbers)
col_country = 2 # Column containing country names

n_days = length(col_tsd) # Number of data points
dates = as.Date("2020-01-21") + 1:n_days # Dates pertaining to case numbers

data_countries = aggregate(x = data_raw[,col_tsd],by = list(data_raw[,col_country]),FUN = sum) # Aggregate data from various regions of a country
names(data_countries) = c("country",as.character(dates))
data_countries = subset(data_countries,is.element(country,countries)) # extract countries of interest
# data_countries[,1:10] # Show first 10 columns

data_daily = data_countries
data_daily[,-(1:2)] = data_countries[,-(1:2)] - data_countries[,-c(1,n_days+1)] # Daily increase in cases
data_daily = data.frame(t(data_daily[,-1])) # Transpose data frame to make time series data for each country a column. This makes it easier to work with.
names(data_daily) = data_countries$country # Country names as column names
head(data_daily) # First couple rows of the daily case numbers per country
tail(data_daily) # Last couple rows of the daily case numbers per country

# If you want to export the preprocessed data (e.g., for use with another programming language), you can write it to a csv file with
# write.csv(data_daily,file = paste0("daily_cases_", format(Sys.Date(), "%Y%m%d"), ".csv"))
# or
# write.csv(t(data_daily),file = paste0("daily_cases_", format(Sys.Date(), "%Y%m%d"), ".csv"))
# if you prefer having a row for each time series

# Data exploration
# We will conduct some preliminary data analysis as part of the problem class.
matplot(data_daily,type = "l",lty = 1)

# Exemplary forecast: Persistence/no-change forecast of order n.
# For each country forecast empirical distribution of last n days.
# Use empirical quantiles as quantile forecasts.
persistence_fcast = function(country,n = 30){
  # Fix country and n to run the function line by line:
  # country = "Germany"; n = 30
  ts_country = ts(data_daily[,country])
  # plot(ts_country)
  fcast_sample = tail(ts_country,n)
  levels = c(0.05,0.1,0.25,0.5,0.75,0.9,0.95)
  fcast_quantiles = quantile(fcast_sample,probs = levels)
  return(fcast_quantiles)
}

# Apply forecast method for each country
predictions = t(sapply(countries,persistence_fcast))
predictions

# Write to file
name = "JaneDoe"
today = Sys.Date()
write.csv(predictions, file = paste0(name,"_", format(today + 1, "%Y%m%d"), ".csv"))
# or use the following and add your name and the correct date manually
# write.csv(predictions,file = "MyName_yyyymmdd.csv")
# The output of write.csv() has the column names as first line.
# It does not matter what the column names are in your submitted files.
# In particular, your column names do NOT need to match the names in the exemplary csv file.

# Analysis and Evaluation
# Evaluation and analysis of the exemplary forecast will be part of the problem class.


