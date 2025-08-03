# b) Simulate and plot a series of 200 observations from each process. Add plots of the differenced series.
par(mfrow = c(2,1))

# i)
ts1 = arima.sim(list(order = c(0,1,1),ma = -0.6),200,sd = 1) # ,sd = 20)
plot(ts1)
plot(diff(ts1))

# ii)
ts2 = ts1 + cumsum(rep(5,201))
plot(ts2) # Change sd (the white-noise standard deviation) in simulation ts1 to see a stronger effect of the random component
plot(diff(ts2))

# iii)
ts3 = arima.sim(list(order = c(1,1,0),ar = 0.9),200)
plot(ts3)
plot(diff(ts3))

# iv)
ts4 = arima.sim(list(order = c(1,1,1),ar = 0.9,ma = 0.5),200)
plot(ts4)
plot(diff(ts4))

# c) Compute and plot the sample autocorrelation function and the sample partial autocorrelation function 
# for each simulated series, both before and after differencing. Comment on the results.

# i)
acf(ts1)
pacf(ts1)

acf(diff(ts1))
pacf(diff(ts1))

# ii)
acf(ts2)
pacf(ts2)

acf(diff(ts2))
pacf(diff(ts2))

# iii)
acf(ts3)
pacf(ts3)

acf(diff(ts3))
pacf(diff(ts3))

#iv)
acf(ts4)
pacf(ts4)

acf(diff(ts4))
pacf(diff(ts4))
