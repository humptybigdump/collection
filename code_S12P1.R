# Material for Forecast Challenge 2021 + Solution to Problem 1 of Set 12
# Script illustrating the construction of an exemplary forecast 
# + Data exploration and evaluation of exemplary forecast

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
# if you prefer having a row for each time series.

# Data exploration
matplot(data_daily,type = "l",ylim = c(0,400000))
legend("topleft",countries,col = rep(1:6,times = length(countries)/6),
       lty = rep(1:5,times = length(countries)/5),cex = 0.8,ncol = 3)
matplot(log(data_daily+1),type = "l",ylim = c(0,16))
legend("topleft",countries,col = rep(1:6,times = length(countries)/6),
       lty = rep(1:5,times = length(countries)/5),cex = 0.8,ncol = 3)

# Exploring data for a specific country
country = "Germany"
country2 = "Switzerland"

matplot(data_daily[,country],type = "l",lty = 1)

acf(data_daily[,country])
pacf(data_daily[,country])

acf(data_daily[,c(country,country2)])
ccf(data_daily[,country],data_daily[,country2])

decomp = decompose(ts(data_daily$Germany,frequency = 7),type = "additive")
plot(decomp)
decomp = decompose(ts(data_daily$Germany,frequency = 7),type = "multiplicative")
plot(decomp)

acf(decomp$random,na.action = na.pass)
pacf(decomp$random,na.action = na.pass)

acf(decomp$trend,na.action = na.pass)
pacf(decomp$trend,na.action = na.pass)

decomp2 = decompose(ts(data_daily[,country2],frequency = 7),type = "multiplicative")
plot(decomp2)

acf(decomp2$random,na.action = na.pass)
pacf(decomp2$random,na.action = na.pass)

acf(decomp2$trend,na.action = na.pass)
pacf(decomp2$trend,na.action = na.pass)

ccf(decomp$random,decomp2$random,na.action = na.pass)
ccf(decomp$trend,decomp2$trend,na.action = na.pass)

# Exemplary forecast: Persistence/no-change forecast of order n.
# For each country forecast empirical distribution of last n days.
# Use empirical quantiles as quantile forecasts.
# persistence_fcast = function(country,n = 30){
#   # Fix country and n to run the function line by line:
#   # country = "Germany"; n = 30
#   ts_country = ts(data_daily[,country])
#   # plot(ts_country)
#   fcast_sample = tail(ts_country,n)
#   levels = c(0.05,0.1,0.25,0.5,0.75,0.9,0.95)
#   fcast_quantiles = quantile(fcast_sample,probs = levels)
#   return(fcast_quantiles)
# }
# 
# # Apply forecast method for each country
# predictions = t(sapply(countries,persistence_fcast))
# predictions
# 
# # Write to file
# name = "JaneDoe"
# today = Sys.Date()
# write.csv(predictions, file = paste0(name,"_", format(today + 1, "%Y%m%d"), ".csv"))
# or use the following and add your name and the correct date manually
# write.csv(predictions,file = "MyName_yyyymmdd.csv")
# The output of write.csv() has the column names as first line.
# It does not matter what the column names are in your submitted files.
# In particular, your column names do NOT need to match the names in the exemplary csv file.

# Analysis and Evaluation
# Evaluation and analysis of the exemplary forecast will be part of the problem class.
levels = c(0.05,0.1,0.25,0.5,0.75,0.9,0.95)

scoring_function = function(q,y){
  score = sum(((q >= y) - levels)*(log(q+1) - log(y+1)))
  return(score/7)
}

persistence_fcast_for = function(day,country,n = 30){
  ind = which(dates == day)
  ts_country = ts(data_daily[1:(ind-2),country])
  fcast_sample = tail(ts_country,n)
  levels = c(0.05,0.1,0.25,0.5,0.75,0.9,0.95)
  fcast_quantiles = quantile(fcast_sample,probs = levels)
  return(fcast_quantiles)
}

evaluate_dates = function(days, order = 30){
  scores = rep(NA,length(countries))
  for(i in 1:length(countries)){
    country = countries[i]
    scores[i] = mean(sapply(days,
                            function(day,country,order){
                              q = persistence_fcast_for(day,country,n = order)
                              y = data_daily[which(dates == day),country]
                              return(scoring_function(q,y))
                              },
                            country = country,
                            order = order))
  }
  return(scores)
}

april_dates = seq(as.Date("2021-04-01"),by = 1,length.out = 30)
may_dates = seq(as.Date("2021-05-01"),by = 1,length.out = 31)

scores_april = evaluate_dates(april_dates)
scores_may = evaluate_dates(may_dates)

data.frame(Country = c(countries,"Overall"),
           April = c(scores_april,mean(scores_april)),
           May = c(scores_may,mean(scores_may)))

# Finding the best order
orders = 1:90
scores_april = sapply(orders,function(order) evaluate_dates(april_dates,
                                                            order = order))
scores_may = sapply(orders,function(order) evaluate_dates(may_dates,
                                                          order = order))
which.min(colSums(scores_april)); which.min(colSums(scores_may))

matplot(orders,t(scores_may),type = "l",ylim = c(0,1.2))
legend("topright",countries,col = rep(1:6,times = length(countries)/6),
       lty = rep(1:5,times = length(countries)/5),cex = 0.8,ncol = 4)
points(orders,colSums(scores_may)/length(countries),type = "l",col = "red")
legend("bottomright","Overall",col = "red",lty = 1,cex = 0.8)
matplot(orders,t(scores_april),type = "l",ylim = c(0,1.2))
legend("topright",countries,col = rep(1:6,times = length(countries)/6),
       lty = rep(1:5,times = length(countries)/5),cex = 0.8,ncol = 4)
points(orders,colSums(scores_april)/length(countries),type = "l",col = "red")
legend("bottomright","Overall",col = "red",lty = 1,cex = 0.8)



