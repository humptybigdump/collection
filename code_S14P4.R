# Loading the data
# Load raw data from JHU GitHub https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv
data_raw = read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

# The countries of interest with names used by JHU
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

# Save processed data
# today = Sys.Date()
# save(dates, countries, data_daily, file = paste0("data_", format(today, "%Y%m%d"), ".Rdata"))

#####################################################################################
# Model-based data analysis:
# We use data from the first half of 2021 only
dates_2021 = seq(as.Date("2021-01-01"),by = 1,length.out = 181)
ind_2021 = which(dates == dates_2021[1]) + 0:180

country = "Germany" # Try other countries!
ts_country = ts(data_daily[ind_2021,country],frequency = 7)

# Fitting a sARIMA model
# Find orders, which result in good (= low) AIC values,
# while keeping the orders small (= less parameters -> avoids overfitting)
arima(ts_country,order = c(1,0,0)) # AR(1) without seasonal component
arima(ts_country,order = c(0,0,1)) # MA(1) without seasonal component
arima(ts_country,order = c(1,1,0)) # AR(1) + differencing without seasonal component
arima(ts_country,order = c(0,1,1)) # MA(1) + differencing without seasonal component

# Differencing helps when modeling non-stationary time series
# (differenced time series often become stationary)
plot(ts_country)
plot(diff(ts_country))
plot(diff(ts_country,lag = 7)) # seasonal differencing
plot(diff(diff(ts_country),lag = 7)) # combined
acf(diff(ts_country))
acf(diff(diff(ts_country),lag = 7))
pacf(diff(ts_country))
pacf(diff(diff(ts_country),lag = 7))

arima(ts_country,order = c(1,0,0),seasonal = list(order = c(1,0,0),period = 7)) # AR(1) with seasonal component
arima(ts_country,order = c(0,0,1),seasonal = list(order = c(0,0,1),period = 7)) # MA(1) with seasonal component
arima(ts_country,order = c(1,1,0),seasonal = list(order = c(1,1,0),period = 7)) # AR(1) + differencing with seasonal component
arima(ts_country,order = c(0,1,1),seasonal = list(order = c(0,1,1),period = 7)) # MA(1) + differencing with seasonal component

# Last one seems best, experiment with components, to see if we can further improve

# Try: Add (seasonal) AR component, additional differencing, higher MA order...
arima(ts_country,order = c(1,1,1),seasonal = list(order = c(0,1,1),period = 7))
arima(ts_country,order = c(0,1,1),seasonal = list(order = c(1,1,1),period = 7)) 
arima(ts_country,order = c(0,2,1),seasonal = list(order = c(0,1,1),period = 7)) 
arima(ts_country,order = c(0,1,1),seasonal = list(order = c(0,2,1),period = 7))
arima(ts_country,order = c(0,1,2),seasonal = list(order = c(0,1,1),period = 7))
arima(ts_country,order = c(0,1,1),seasonal = list(order = c(0,1,2),period = 7)) 

# Increasing (non-seasonal) MA-order + differencing once more in seasonal part seems to improve the model (in terms of AIC; depends on the country!)
arima(ts_country,order = c(0,1,2),seasonal = list(order = c(0,2,1),period = 7))

#####################################################################################
# Analysis and Evaluation of the sARIMA forecast
library(truncnorm)

levels = c(0.05,0.1,0.25,0.5,0.75,0.9,0.95)

scoring_function = function(q,y){
  score = sum(((q >= y) - levels)*(log(q+1) - log(y+1)))
  return(score/7)
}

n_weeks_train = data.frame(t(rep(10,13))) # Use past 10 weeks as training data for each country
names(n_weeks_train) = countries
# Optimal training period for each country will be determined below using June data...

# Model implementation
sarima_fcast_for = function(day,country,n_weeks = NA){
  if(is.na(n_weeks)) n_weeks = n_weeks_train[,country]
  ind = which(dates == day)
  ts_country = ts(data_daily[1:(ind-2),country])
  
  # Rolling training period: The model is fitted on the most recent data to produce predictions for a given day.
  model = arima(tail(ts_country,n_weeks*7),order = c(0,1,1),seasonal = list(order = c(0,1,1),period = 7))
  # Robust model, which seems to produce good predictions without causing technical problems (e.g., errors caused by optimization troubles)
  # Setting method = "CSS" in arima() speeds up computations, but seems to result in slightly less optimal predictions
  
  # Try this model instead (Warning: This seems to require at least 4 weeks of training data!)
  # model = arima(tail(ts_country,n_weeks*7),order = c(0,1,2),seasonal = list(order = c(0,2,1),period = 7) ,optim.method = "Nelder-Mead" ,method = "CSS")
  # It seems that the shorter training period of only 3 weeks that can be used with the simpler model is advantageous...
  
  pred = predict(model,2)
  mean = pred$pred[2]
  sd = pred$se[2]

  # Use truncated normal to produce probabilistic predictions with predicted mean and standard deviation:
  quantiles = qtruncnorm(levels,a = 0,mean=mean,sd=sd)
  # Adjustment for recurring zeros (improvised ad hoc approach; improves predictions for Switzerland and Sweden greatly!):
  n_zeros = sum(ts_country[ind - 7*(1:n_weeks)] == 0)
  if(n_zeros > 0){ 
    # replace lower tail of the predictive distribution having n_zeros/n_weeks
    # probability mass with a point mass in 0 and shift the remaining upper tail down to 0:
    # quantiles = qtruncnorm(levels,a = 0,mean=mean,sd=sd) - qtruncnorm(n_zeros/n_weeks,a = 0,mean=mean,sd=sd)
    # quantiles = ifelse(quantiles < 0,0,quantiles)
    
    # Better: replace upper tail with point mass of n_zero/n_weeks at 0 (this results in sharper distributions!):
    quantiles = qtruncnorm(levels - n_zeros/n_weeks,a = 0,mean=mean,sd=sd)
    quantiles = ifelse(is.nan(quantiles),0,quantiles)
  }
  
  # Try predicting normal distribution instead:
  # quantiles = qnorm(levels,mean,sd)
  # quantiles = pmax(0,quantiles) # avoid negative predictions

  return(quantiles)
}

evaluate_dates = function(days,fcast_for,param = list(),plot_country = "No"){
  scores = rep(NA,length(countries))
  for(i in 1:length(countries)){
    country = countries[i]
    scores_daily = sapply(days,
                          function(day,country){
                            q = do.call(fcast_for,c(list(day = day,country = country),param))
                            y = data_daily[which(dates == day),country]
                            return(scoring_function(q,y))
                          },
                          country = country)
    if(country == plot_country){
      ind = which(dates == days[1]) + 1:length(days) - 1
      plot(scores_daily,type = "l",ylim = c(0,12))
      points(log(data_daily[ind,plot_country]+1),lty = 2)
    } 
    scores[i] = mean(scores_daily)
  }
  return(scores)
}

evaluate_dates_coverage = function(days,fcast_for,param = list()){
  exceedance_freq = matrix(NA,nrow = length(countries),ncol = length(levels))
  for(i in 1:length(countries)){
    country = countries[i]
    exceedance_freq[i,] = rowSums(sapply(days,
                                         function(day,country){
                                           q = do.call(fcast_for,c(list(day = day,country = country),param))
                                           y = data_daily[which(dates == day),country]
                                           return(y <= q)
                                         },
                                         country = country))/length(days)
  }
  return(exceedance_freq)
}

dates_2021 = seq(as.Date("2021-01-01"),by = 1,length.out = 181)
dates_June = seq(as.Date("2021-06-01"),by = 1,length.out = 30)
# To optimize our model for the forecast challenge at the beginning of July,
# We focus on data from June (Reasoning: the data generating process continuously changes 
# and we hope that the June data is more indicative of what will happen at the beginning of July.)

scores_2021 = evaluate_dates(dates_2021,sarima_fcast_for)
scores_June = evaluate_dates(dates_June,sarima_fcast_for,plot_country = "Mexico")
# zeros produce score outliers (-> detecting recurring zeros is vital to optimize score!)

coverage_2021 = evaluate_dates_coverage(dates_2021,sarima_fcast_for)
coverage_June = evaluate_dates_coverage(dates_June,sarima_fcast_for)
# coverage should be greater or equal to the specified level for calibrated quantile forecasts
# (in fact, this notion of calibration of quantiles is closely related to probabilistic calibration 
# in that probabilistically calibrated forecasts produce calibrated quantile forecasts in terms of coverage)

data.frame(Country = c(countries,"Overall"),
           round(cbind(
             JanJun = c(scores_2021,mean(scores_2021)),
             rbind(coverage_2021,colSums(coverage_2021)/13),
             June = c(scores_June,mean(scores_June)),
             rbind(coverage_June,colSums(coverage_June)/13)
           ),3))
# Coverage of the quantiles (averaged across all countries) seems fine

#####################################################################################
# Finding the best training period for each country in June
ns = 3:30
# ns = 4:30 # use this with the other model!

scores_June_n = sapply(ns,function(n) evaluate_dates(dates_June,sarima_fcast_for,param = list(n_weeks = n)))

# best overall training period
which.min(colSums(scores_June_n)) + min(ns) - 1 

# best training period per country
apply(scores_June_n,1,which.min) + min(ns) - 1 
# Without method = "CSS": 26  5 28  7  3  3  3 16 30  5 22  3 10
# With method = "CSS": 28, 3, 27,  3,  3,  3,  3, 14, 29,  5, 22,  3,  3
# Interesting: While for many countries short training windows seem to perform well,
# there are some countries for which large training windows are preferable.

matplot(ns,t(scores_June_n),type = "l",ylim = c(0,1.2))
legend("topright",countries,col = rep(1:6,times = length(countries)/6),
       lty = rep(1:5,times = length(countries)/5),cex = 0.8,ncol = 4)
points(ns,colSums(scores_June_n)/length(countries),type = "l",col = "red")
legend("bottomright","Overall",col = "red",lty = 1,cex = 0.8)

# compute scores and coverage with the optimal rolling training window for each country
n_weeks_train = data.frame(t(apply(scores_June_n,1,which.min)+min(ns) - 1))
names(n_weeks_train) = countries

scores_2021 = evaluate_dates(dates_2021,sarima_fcast_for)
scores_June = evaluate_dates(dates_June,sarima_fcast_for)

coverage_2021 = evaluate_dates_coverage(dates_2021,sarima_fcast_for)
coverage_June = evaluate_dates_coverage(dates_June,sarima_fcast_for)

data.frame(Country = c(countries,"Overall"),
           round(cbind(
             JanJun = c(scores_2021,mean(scores_2021)),
             rbind(coverage_2021,colSums(coverage_2021)/13),
             June = c(scores_June,mean(scores_June)),
             rbind(coverage_June,colSums(coverage_June)/13)
           ),3))
# While the scores have improved, the coverage at large levels (90 and 95%) 
# seems insufficient (= too low).
# The forecasts might be improved by resorting to predictive distributions with heavier upper tails.
# However, it seems more promising to resort to heteroskedastic models, which 
# might remedy this while further improving forecast accuracy.




