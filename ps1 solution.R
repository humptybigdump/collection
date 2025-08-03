# = = = = = = = = = = = = = = = = = = = = = = = = = #
#                                                   #
#         Financial Econometrics 1 (WS23/24)        #
#                   Problem Set 1                   #
#                                                   #
# = = = = = = = = = = = = = = = = = = = = = = = = = #

# = = = = = = = = = = = = = = = = = = = = = = = = = =
# Problem 1 ####
# = = = = = = = = = = = = = = = = = = = = = = = = = =

# R functions used

# as.Date()			coerce to Date object
# as.numeric()		coerce to numeric object
# class()			return or set class of object
# colnames()		extract column names
# format()			format output
# head()			show fist few rows of data object
# read.csv()		read comma separated file into R
# rownames()
# seq()			    create sequence
# tail()			show last few rows of data object

# set output options to show only 4 significant digits
options(digits = 4)

# = = = = = = = = = = = = = = = = = = = = = = = = = =
## i - import data ####
# = = = = = = = = = = = = = = = = = = = = = = = = = =

# read the sbux (starbucks) prices into a data.frame object. 
# First look at the online help file for read.csv
?read.csv

# now read in the data - make sure to change path to where data is on your system
# Insert the Directory (i.e. folder) of the file here

getwd() # shows current working directory
mydir<- "..."
setwd(mydir)

sbux.df = read.csv(file="SBUX.csv", 
                   header=TRUE, stringsAsFactors=FALSE)

# sbux.df is a data.frame object. Data.frames are rectangular data objects 
# typically with observations in rows and variables in columns

class(sbux.df)
str(sbux.df)
head(sbux.df)
tail(sbux.df)
colnames(sbux.df)
class(sbux.df$Date)
class(sbux.df$Adj.Close)

# - - - - - - - - - - - - - - - - - - - - - - - - - 
# look at the data and how to manipulate it ie subsetting operations

# extract the first 5 rows of the price data. 
sbux.df[1:5, "Adj.Close"]
sbux.df[1:5, 6]
sbux.df$Adj.Close[1:5]

# in the above operations, the output became a numeric vector
# To preserve the dataframe format and column names use drop=FALSE

sbux.df[1:5, "Adj.Close", drop=FALSE]
sbux.df[1:5, 5, drop=FALSE]
sbux.df$Adj.Close[1:5, drop=FALSE] 
# here, drop=FALSE does not work since $ already extracts the column as a numeric vector

# find indices associated with the dates 3/1/1994 and 3/1/1995
which(sbux.df$Date == "1994-03-01")
which(sbux.df == "1995-03-01")

# extract prices between 3/1/1994 and 3/1/1995
sbux.df[13:25, ] # comma because we want all columns

# Note:
# closing price = last price of the trading day
# adjusted closing price is adjusted by dividends, stock splits etc.

# - - - - - - - - - - - - - - - - - - - - - - - - - 
# create a new data.frame containing the price data with the dates as the row names
sbuxPrices.df = sbux.df[, "Adj.Close", drop=FALSE]
rownames(sbuxPrices.df) = sbux.df$Date
head(sbuxPrices.df)
                       
# with Dates as rownames, you can subset directly on the dates
# find indices associated with the dates 3/1/1994 and 3/1/1995
sbuxPrices.df["1994-03-01", 1]
sbuxPrices.df["1995-03-01", 1]

# to show the rownames use drop=FALSE
sbuxPrices.df["1994-03-01", 1, drop=FALSE]

# = = = = = = = = = = = = = = = = = = = = = = = = = =
## ii - plot the data ####
# = = = = = = = = = = = = = = = = = = = = = = = = = =

# note: the default plot is a "points" plot
plot(sbux.df$Adj.Close)

# type="l"                  specifies a line plot
# col="blue"                specifies blue line color
# lwd=2                     doubles the line thickness
# ylab="Adjusted close"     adds a y axis label
# main="Monthly ... "       adds a title

plot(sbux.df$Adj.Close, type="l", col="blue", 
     lwd=2, ylab="Adjusted close",
     main="Monthly closing price of SBUX")

# now add a legend
legend(x="topleft", legend="SBUX", 
       lty=1, lwd=2, col="blue")

# = = = = = = = = = = = = = = = = = = = = = = = = = =
## iii - compute returns ####
# = = = = = = = = = = = = = = = = = = = = = = = = = =

## simple 1-month returns
n = nrow(sbuxPrices.df)
sbux.ret = (sbuxPrices.df[2:n,1] - sbuxPrices.df[1:(n-1),1])/sbuxPrices.df[1:(n-1),1]
# Note: the R function diff automatically calculates returns from prices

# notice that sbux.ret is not a data.frame object
class(sbux.ret)

# now add dates as names to the vector. 
names(sbux.ret) = rownames(sbuxPrices.df)[2:n]
head(sbux.ret)

# Note: to ensure that sbux.ret is a data.frame use drop=FALSE when computing returns
sbux.ret.df = (sbuxPrices.df[2:n,1,drop=FALSE]
               - sbuxPrices.df[1:(n-1),1,drop=FALSE]) / sbuxPrices.df[1:(n-1),1,drop=FALSE]


## continuously compounded 1-month returns
sbux.ccret = log(1 + sbux.ret)

# alternatively
sbux.ccret = log(sbuxPrices.df[2:n,1]) - log(sbuxPrices.df[1:(n-1),1])
names(sbux.ccret) = rownames(sbuxPrices.df)[2:n]
head(sbux.ccret)

# compare the simple and cc returns
head(cbind(sbux.ret, sbux.ccret))

# - - - - - - - - - - - - - - - - - - - - - - - - - 
# plot the simple and cc returns in separate graphs

# split screen into 2 rows and 1 column
par(mfrow=c(2,1))

# plot simple returns first
plot(sbux.ret, type="l", col="blue", lwd=2, ylab="Return",
     main="Monthly Simple Returns on SBUX")
abline(h=0)     

# next plot the cc returns
plot(sbux.ccret, type="l", col="blue", lwd=2, ylab="Return",
     main="Monthly Continuously Compounded Returns on SBUX")
abline(h=0)

# reset the screen to 1 row and 1 column
par(mfrow=c(1,1))     

# plot the returns on the same graph
plot(sbux.ret, type="l", col="blue", lwd=2, ylab="Return",
     main="Monthly Returns on SBUX")

# add horizontal line at zero
abline(h=0)

# add the cc returns
lines(sbux.ccret, col="red", lwd=2)

# add a legend
legend(x="bottomright", legend=c("Simple", "CC"), 
       lty=1, lwd=2, col=c("blue","red"))

# - - - - - - - - - - - - - - - - - - - - - - - - - 
# calculate growth of $1 invested in SBUX

# compute gross returns
sbux.gret = 1 + sbux.ret

# compute future values
sbux.fv = cumprod(sbux.gret)
plot(sbux.fv, type="l", col="blue", lwd=2, ylab="Dollars", 
     main="FV of $1 invested in SBUX")

# = = = = = = = = = = = = = = = = = = = = = = = = = =
# Alternatively, you can also work with ts objects

# ii)
sbux.ts<-ts(sbux.df$Adj.Close,start=c(1993,3),end=c(2008,3),frequency = 12)
plot(sbux.ts)

# iii)
sbux.ret.ts<-diff(sbux.ts)/lag(sbux.ts,k=-1)
sbux.ccret.ts<-diff(log(sbux.ts))
plot(cbind(sbux.ret.ts,sbux.ccret.ts))
plot(cbind(sbux.ret.ts,sbux.ccret.ts),plot.type = "single",col=c("blue","red"))

# = = = = = = = = = = = = = = = = = = = = = = = = = =
# Problem 2 ####
# = = = = = = = = = = = = = = = = = = = = = = = = = =

#	Core R functions used:

#	apply			apply function to rows or columns of matrix
#	args			determine agruments of a function
#	cbind			combine data objects vertically
#	class			determine class of object
#	colIds			get column names from object
#	density			compute smoothed histogram
#   ecdf            compute empirical CDF
#	end				get end date of time series
#	help			invoke help system
#	hist			compute histogram
#	legend			add legend to graph
#	length			compute column length of matrix
#	library			load R package
#	mean			compute sample mean
#	names	    	show names of object
#	par				set graphics parameters
#	plot			generic plot function
#	points			add points to a graph
#	pnorm			compute normal CDF
#	seq				generate sequence of numbers
#	ts.plot			time series plot
#	?				invoke help system

# R packages used

#   PerformanceAnalytics
#   tseries			        Time series and computational finance
#	get.hist.quote	        load data from Yahoo!
#	zoo
#	plot.zoo		        plot zoo object

# = = = = = = = = = = = = = = = = = = = = = = = = = =
## i - load data ####
# = = = = = = = = = = = = = = = = = = = = = = = = = =

# load packages
library("PerformanceAnalytics")
library("tseries")
library("zoo")

# get monthly adjusted closing price data on MSFT and SP500 from Yahoo
# using the tseries function get.hist.quote. Set sample to Jan 1998 through
# May 2012. Note: if you are not careful with the start and end dates
# or if you set the retclass to "ts" then results might look weird

MSFT.prices = get.hist.quote(instrument="msft", start="1998-01-01",
                             end="2012-05-31", quote="Adjusted",
                             origin="1970-01-01", provider="yahoo",
                             compression="m", retclass="zoo")

class(MSFT.prices)
colnames(MSFT.prices)
start(MSFT.prices)
end(MSFT.prices)
head(MSFT.prices)

# change date index class to yearmon
# index(MSFT.prices) = as.yearmon(index(MSFT.prices))
# note: x-axis on graphs do not look good if this is done

SP500.prices = get.hist.quote(instrument="^gspc", start="1998-01-01",
                             end="2012-05-31", quote="AdjClose",
                             provider="yahoo", origin="1970-01-01",
                             compression="m", retclass="zoo")

# change date index class to yearmon
# index(SP500.prices) = as.yearmon(index(SP500.prices))

# add column names 
colnames(MSFT.prices) = "MSFT"
colnames(SP500.prices) = "SP500"

# = = = = = = = = = = = = = = = = = = = = = = = = = =
## ii - zoo object ####
# = = = = = = = = = = = = = = = = = = = = = = = = = =

# create zoo object with both prices
MSFTSP500.prices = merge(MSFT.prices, SP500.prices) 
# or use cbind (column bind) here

head(MSFTSP500.prices, n=3)

# compute cc returns 
MSFT.ret = diff(log(MSFT.prices))
SP500.ret = diff(log(SP500.prices))
MSFTSP500.ret = merge(MSFT.ret,SP500.ret)
colnames(MSFTSP500.ret)
head(MSFTSP500.ret, n=3)

class(MSFT.ret)
class(SP500.ret)
class(MSFTSP500.ret)

# convert zoo data to matrix data for non time series analysis
# many R functions do not have methods implemented for zoo objects
# and results may be unpredictable
MSFT.ret.mat = coredata(MSFT.ret)
colnames(MSFT.ret.mat) = "MSFT"
rownames(MSFT.ret.mat) = as.character(index(MSFT.ret))

SP500.ret.mat = coredata(SP500.ret)
colnames(SP500.ret.mat) = "SP500"
rownames(SP500.ret.mat) = as.character(index(SP500.ret))

MSFTSP500.ret.mat = coredata(MSFTSP500.ret)
colnames(MSFTSP500.ret.mat) = c("MSFT","SP500")
rownames(MSFTSP500.ret.mat) = as.character(index(MSFTSP500.ret))

# = = = = = = = = = = = = = = = = = = = = = = = = = =
## iii - time plots ####
# = = = = = = = = = = = = = = = = = = = = = = = = = =

# look at help file for plot method for zoo objects
?plot.zoo
# when you plot zoo objects, R will automatically use plot.zoo instead of base R plot

# - - - - - - - - - - - - - - - - - - - - - - - - - 
## a
# plot individual prices in separate graphs

plot(MSFT.prices,main="Monthly closing price of MSFT",
     ylab="Price", lwd=2, col="blue")
plot(SP500.prices,main="Monthly closing price of SP500",
     ylab="Price", lwd=2, col="blue")

# plot individual prices in two panel graph
plot(MSFTSP500.prices, main="Adjusted Closing Prices", 
     lwd=2, col="blue")

# - - - - - - - - - - - - - - - - - - - - - - - - - 
## b
# put returns on the same plot in separate panels

# panel function for plot.zoo to add horizontal line at zero in each panel
my.panel <- function(...) {
  lines(...)
  abline(h=0)
}

plot(MSFTSP500.ret, main="Monthly cc returns on MSFT and SP500", 
     panel=my.panel, lwd=2, col="blue")

# - - - - - - - - - - - - - - - - - - - - - - - - - 
## c
# put returns on same plot in one panel and add a horizontal line

plot(MSFTSP500.ret, plot.type="single", 
     main="Monthly cc returns on MSFT and SP500",
     col = c("red", "blue"), lty=c("dashed", "solid"), 
     lwd=2, ylab="Returns")

abline(h=0)

legend(x="bottomright", legend=c("MSFT","SP500"), 
       lty=c("dashed", "solid"), lwd=2, 
       col=c("red","blue"))

# = = = = = = = = = = = = = = = = = = = = = = = = = =
## iv ####
# = = = = = = = = = = = = = = = = = = = = = = = = = =

# use PerformanceAnalytics function chart.TimeSeries for nicer time series graphs
chart.TimeSeries(MSFT.ret)
chart.TimeSeries(MSFTSP500.ret)
par(mfrow=c(2,1))
  chart.TimeSeries(MSFT.ret)
  chart.TimeSeries(SP500.ret)
par(mfrow=c(1,1))

#we can also plot cumulative returns
#important here: what kind of returns are we measuring (log or simple)
#geometric=TRUE for input x: cumprod(x+1)-1
#geometric=FALSE for input x: cumsum(x)
#Here, for our log returns, we use geometric=FALSE
chart.CumReturns(MSFTSP500.ret, geometric=FALSE, main="",
                 legend.loc="topright")

#or in values
Return.cumulative(MSFTSP500.ret,geometric = FALSE)

# = = = = = = = = = = = = = = = = = = = = = = = = = =
## v ####
# create simulated iid Gaussian data with same mean and SD as MSFT
# = = = = = = = = = = = = = = = = = = = = = = = = = =

set.seed(123)
gwn = rnorm(length(MSFT.ret),mean=mean(MSFT.ret),sd=sd(MSFT.ret))
gwn.zoo = zoo(gwn, index(MSFT.ret))

#basic plot
par(mfrow=c(2,1))
  plot(MSFT.ret,main="Monthly cc returns on MSFT", lwd=2, col="blue")
  abline(h=0)
  ts.plot(gwn,main="Gaussian data with same mean and sd as MSFT", lwd=2, col="blue")
  abline(h=0)
par(mfrow=c(1,1))

# we observe that financial data has time varying variance
# whereas Gaussian data has constant variance

#plot using zoo
dataToPlot = merge(MSFT.ret, gwn.zoo)
colnames(dataToPlot) = c("MSFT", "GWN")
plot(dataToPlot, main="", 
     col="blue", lwd=2, panel=my.panel)

# = = = = = = = = = = = = = = = = = = = = = = = = = =
## vi, vii, viii - histograms ####
# = = = = = = = = = = = = = = = = = = = = = = = = = =

# use hist() command
# note: hist() does not have a method for objects of class zoo
# must use coredata to extract data prior to using hist

args(hist)
?hist
hist(MSFT.ret.mat,main="Histogram of MSFT monthly cc returns", 
     col="slateblue1")

# scale histogram so that total area = 1
hist(MSFT.ret.mat,main="Histogram of MSFT monthly cc returns",
     probability=TRUE, col="slateblue1")

# histogram of simulated Gaussian data
hist(gwn,main="Histogram of simulated Gaussian data", col="slateblue1")

# histogram of S&P 500 data
hist(SP500.ret.mat,main="Histogram of SP500 monthly cc returns", 
     col="slateblue1",breaks=15)

# = = = = = = = = = = = = = = = = = = = = = = = = = =
## vii - plot both histograms on same page
# = = = = = = = = = = = = = = = = = = = = = = = = = =

# note different scales
par(mfrow=c(1,2))
  hist(MSFT.ret.mat,main="", xlab="Microsoft Monthly cc Returns", col="slateblue1")
  hist(SP500.ret.mat,main="", xlab="S&P 500 Monthly cc Returns", col="slateblue1")
par(mfrow=c(1,1))

par(mfrow=c(2,1))
  hist(MSFT.ret.mat,main="Histogram of MSFT monthly returns", col="slateblue1")
  hist(SP500.ret.mat,main="Histogram of SP500 monthly returns", col="slateblue1")
par(mfrow=c(1,1))

# try different layout
par(mfrow=c(1,2))
  hist(MSFT.ret.mat,main="MSFT")
  hist(SP500.ret.mat,main="SP500")
par(mfrow=c(1,1))

# use same breakpoints for both histograms
MSFT.hist = hist(MSFT.ret.mat,plot=F,breaks=15)
class(MSFT.hist)
names(MSFT.hist)

par(mfrow=c(2,1))
  hist(MSFT.ret.mat,main="MSFT", col="slateblue1", xlab="returns")
  hist(SP500.ret.mat,main="SP500", col="slateblue1", xlab="returns",
       breaks=MSFT.hist$breaks)
par(mfrow=c(1,1))

# Use PerformanceAnalytics function chart.Histogram
chart.Histogram(MSFT.ret)

# = = = = = = = = = = = = = = = = = = = = = = = = = =
## viii - smoothed histograms
# = = = = = = = = = = = = = = = = = = = = = = = = = =

# use density() command
?density
args(density)

# - - - - - - - - - - - - - - - - - - - - - - - - - 
# estimate density for MSFT
MSFT.density = density(MSFT.ret.mat)
class(MSFT.density)
names(MSFT.density)
MSFT.density
plot(MSFT.density,type="l",xlab="monthly cc return", col="orange", lwd=2,
     ylab="density estimate",main="Smoothed histogram")

# put histogram and density plot on same graph
hist(MSFT.ret.mat,main="", xlab="Microsoft Monthly cc Returns", 
     col="slateblue1", probability=T, ylim=c(0,5))
lines(MSFT.density,type="l", col="orange", lwd=2)

# - - - - - - - - - - - - - - - - - - - - - - - - - 
# Same for SP500
SP500.density = density(SP500.ret)
plot(SP500.density,type="l",xlab="monthly return", col="orange", lwd=2,
     ylab="density estimate",main="Smoothed histogram: SP500")

# combine density plots on one graph
hist(SP500.ret.mat,main="Histogram and smoothed density",
     probability=T, ylim=c(0,10), col="slateblue1")
points(SP500.density,type="l", lwd=2, col="orange")

# - - - - - - - - - - - - - - - - - - - - - - - - - 
# shows time plots and histograms together for Microsoft and GWN
par(mfrow=c(2,2))
plot(MSFT.ret,main="Monthly cc returns on MSFT", lwd=2, col="blue",
     ylim=c(-0.4, 0.4))
abline(h=0)
plot(gwn.zoo,main="Simulated Normal Returns", lwd=2, col="blue",
     ylim=c(-0.4, 0.4))
abline(h=0)
hist(MSFT.ret.mat,main="", xlab="Microsoft Monthly cc Returns", 
     col="slateblue1", probability=T, ylim=c(0,5),xlim = c(-0.4,0.4),breaks=15)

# get empirical density
gwn.density <- density(gwn)
lines(gwn.density,type="l", col="orange", lwd=2)

# overlay normal distribution
hist(gwn,main="", xlab="Normally Distributed cc Returns", 
     col="slateblue1", probability=T, ylim=c(0,5),xlim = c(-0.4,0.4))
lines(MSFT.density,type="l", col="orange", lwd=2)
par(mfrow=c(1,1))

# = = = = = = = = = = = = = = = = = = = = = = = = = =
## ix - Jarque-Bera Test ####
# = = = = = = = = = = = = = = = = = = = = = = = = = =

jarque.bera.test(MSFT.ret.mat) # reject H0 ... most likely not normal
jarque.bera.test(SP500.ret.mat) # reject H0 ... most likely not normal
jarque.bera.test(gwn) # cannot reject H0 ... could be normal

