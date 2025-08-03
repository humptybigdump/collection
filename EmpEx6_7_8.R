#Financial Econometrics II Empirical Exercises

#install.packages("vars")
#install.packages("highfrequency")

library(vars)
library(highfrequency)


#############################
#Problem 6

#Reading in the data

#Data cleaning after
# Barndorff-Nielsen, O. E., Hansen, P. R., Lunde, A. and Shephard, N.
# Realised Kernels in Practice: Trades and Quotes
# Econometrics Journal, 2008, Vol. 4, pp. 1-32


##########
#(i)
high_freq_eur<-sampleTDataEurope

high_freq_us_raw<-sampleTDataRaw

#first look at data
summary(high_freq_us_raw) #seems to be from 2 days and different exchanges
#what exchange do we want to take?
table(high_freq_us_raw$EX)
#take exchange D, most data--> NASD TRF (Nasdaq Trade Reporting Facility)
#or take N, NYSE
hf_d<-high_freq_us_raw[which(high_freq_us_raw$EX=="D"),]
hf_n<-high_freq_us_raw[which(high_freq_us_raw$EX=="N"),]#some really large values!

plot(hf_d$DT,hf_d$PRICE,type="l") #trade period! High price jumps?
plot(hf_d$DT,hf_d$SIZE,type="l") #Some really high trading volumes!

plot(hf_n$DT,hf_n$PRICE,type="l")
plot(hf_n$DT,hf_n$SIZE,type="l")  #some really large values!
hf_n<-high_freq_us_raw[which(high_freq_us_raw$EX=="N" & high_freq_us_raw$SIZE<1000),]

##########
#(ii)

#clean from all non-valid sales conditions: we basically only allow normal trades
dim(hf_n)
hf_n_clean<-tradesCondition(hf_n)
dim(hf_n_clean) #cleaned up some non-valid trades, 
#i.e. some interventions and special trades that do not contain classic information

#remove observations with same time stamp, i.e. via median aggregation
#check if there are some

#quick dirty check
len_trades<-dim(hf_n_clean)[1]
#just check how often the next date is equal to the one before
sum(hf_n_clean$DT[1:(len_trades-1)]==hf_n_clean$DT[2:len_trades])
#can you write a function that does that manually?

hf_n_final<-mergeTradesSameTimestamp(hf_n_clean)[,c("DT","PRICE","SIZE")]
plot(hf_n_final$DT,hf_n_final$SIZE,type="l") 
plot(hf_n_final$DT,hf_n_final$PRICE,type="l") 

##########
#(iii)

#Aggregation scheme last tick aggregation and sampling frequency
#basically make equispaced sequence over time and take the last point that you observed
#the last observed point before your sequence point is taken as your price/observation
hf_n_5_secs<-aggregateTrades(hf_n_final,alignBy = "seconds",alignPeriod = 5)
hf_n_5_mins<-aggregateTrades(hf_n_final,alignBy = "minutes",alignPeriod = 5)
hf_n_1_hour<-aggregateTrades(hf_n_final,alignBy = "hours",alignPeriod = 1)

plot(hf_n_5_secs$DT,hf_n_5_secs$PRICE,type="l")
lines(hf_n_5_mins$DT,hf_n_5_mins$PRICE,col="red",type="l")
lines(hf_n_1_hour$DT,hf_n_1_hour$PRICE,col="darkblue",type="l",lwd=2)

#Automatic: know what you do with this!
first_clean<-tradesCleanup(tDataRaw = high_freq_us_raw)
first_clean_quotes<-quotesCleanup(qDataRaw =  sampleQDataRaw)

##########
#(iv)

#some estimators from price data
rv_lr<-function(price)
{
  logret<-diff(log(price))
  return(sum(logret^2))
}
logret_sec<-diff(log(hf_n_5_secs$PRICE))
plot(logret_sec,type="l")
rvol_sec<-rv_lr(hf_n_5_secs$PRICE)
rvol_min<-rv_lr(hf_n_5_mins$PRICE)
rvol_hour<-rv_lr(hf_n_1_hour$PRICE)


###########################

#Now lets look at some real data
#############################
#Problem 7
# Working with the LOBSTER data

#we have two data files: The message file and the orderbook
####1) 
# Message file information:
# ----------------------------------------------------------
#
#   - Dimension:    (NumberEvents x 6)
#
#   - Structure:    Each row:
#                   Time stamp (sec after midnight with decimal
#                   precision of at least milliseconds and
#                   up to nanoseconds depending on the period),
#                   Event type, Order ID, Size (# of shares),
#                   Price, Direction
#
#                   Event types:
#                       - '1'   Submission new limit order
#                       - '2'   Cancellation (partial)
#                       - '3'   Deletion (total order)
#                       - '4'   Execution of a visible limit order
#                       - '5'   Execution of a hidden limit order
# 			- '7'   Trading Halt (Detailed 
#                               information below)
#
#                   Direction:
#                       - '-1'  Sell limit order
#                       - '+1'  Buy limit order
#                       - NOTE: Execution of a sell (buy)
#                               limit order corresponds to
#                               a buyer-(seller-) initiated
#                               trade, i.e. a BUY (SELL) trade.
#
# ----------------------------------------------------------

#2) Orderbook:
# Orderbook file information:
# ----------------------------------------------------------
#
#   - Dimension:    (NumberEvents x (NumberLevels*4))
#
#   - Structure:    Each row:
#                   Ask price 1, Ask volume 1, Bid price 1,
#                   Bid volume 1, Ask price 2, Ask volume 2,
#                   Bid price 2, Bid volume 2, ...
#
#   - Note:         Unoccupied bid (ask) price levels are
#                   set to -9999999999 (9999999999) with volume 0.
#				      
# ----------------------------------------------------------



# Note: As the rows of the message and orderbook file correspond to each other, the time index of
# the message file can also be used to 'cut' the orderbook file.

##########
#(i)


#let's start with the message file that contains
#Time stamp, Event type, Order ID, Size (# of shares),Price, Direction
library(data.table)
ibm_raw_msg<-fread("IBM_2016-07-05_msg.csv",data.table = FALSE)

colnames(ibm_raw_msg)<-c("Time","Type","ID","Size","Price","Direction")

head(ibm_raw_msg)

no_traders<-unique(ibm_raw_msg$ID)
head(order(table(ibm_raw_msg$ID)))

#fwrite(ibm_raw_msg,"IBM_msg.csv")

#first remove trades we do not want

# Trading hours (start & end)
startTrad   = 9.5*60*60;       # 9:30:00.000 in seconds after midnight
endTrad     = 16*60*60;        # 16:00:00.000 in seconds after midnight

time_trading<-ibm_raw_msg$Time>=startTrad & ibm_raw_msg$Time<=endTrad

ibm_msg_trading<-ibm_raw_msg[time_trading,] #in our case, data is already cleaned

#you can check for trading halts (not present here)
tradehaltIdx = which(ibm_msg_trading[,2] == 7 & ibm_msg_trading[,5] == -1 );
tradequoteIdx = which(ibm_msg_trading[,2] == 7 & ibm_msg_trading[,5] == 0 );
traderesumeIdx = which(ibm_msg_trading[,2] == 7 & ibm_msg_trading[,5] == 1 );

if(length(tradehaltIdx)==0 & length(tradequoteIdx)==0  & length(traderesumeIdx)==0 )
  print("No trading halts detected.")

if(length(tradehaltIdx) !=0)
  cat("Data contains trading halt! at time stamp(s)", ibm_msg_trading[tradehaltIdx,1],"\n" )

if(length(tradequoteIdx) !=0)
  cat(" Data contains quoting message! at time stamp(s)", ibm_msg_trading[tradequoteIdx,1], "\n")

if(length(traderesumeIdx) !=0)
  cat(" Data resumes trading! at time stamp(s) ", ibm_msg_trading[traderesumeIdx,1],"\n")

#
#		When trading halts, a message of type '7' is written into the 
#		'message' file. The corresponding price and trade direction 
#		are set to '-1' and all other properties are set to '0'. 
#		Should the resume of quoting be indicated, another 
#		message of type '7' with price '0' is added to the 'message' 
#		file. Again, the trade direction is set to '-1' and all other 
#		fields are set to '0'. 
#		When trading resumes a message of type '7' and 
#		price '1' (Trade direction '-1' and all other 
#		entries '0') is written to the 'message' file. For messages 
#		of type '7', the corresponding order book rows contain a 
#		duplication of the preceding order book state. The reason 
#		for the trading halt is not included in the output.
#						
#		Example: Stylized trading halt messages in 'message' file.				
#	
#		Halt: 			36023	| 7 | 0 | 0 | -1 | -1
#											...
#		Quoting: 		36323 	| 7 | 0 | 0 | 0  | -1
#											...
#		Resume Trading:		36723   | 7 | 0 | 0 | 1  | -1
#											...
#		The vertical bars indicate the different columns in the  
#		message file.

##################
##########
#(ii)

#Now we want to plot some data on Executions and Trade volume

#We can distinguish between hidden and visible orders
#Hidden orders do not appear in the limit order book with their actual size,
#e.g.if you do not want your order to show up because you want to buy/sell a large
# amount of stocks


###Info from LOBSTER data base
#_____________________________________________________________________________
#   
# Plot - Number of Executions and Trade Volume by Interval
#_____________________________________________________________________________

# Note: Difference between trades and executions
#
#    The LOBSTER output records limit order executions
#    and not what one might intuitively consider trades.
#
#    Imagine a volume of 1000 is posted at the best ask
#    price. Further, an incoming market buy order of
#    volume 1000 is executed against the quote.
#
#    The LOBSTER output of this trade depends on the
#    composition of the volume at the best ask price.
#    Take the following two scenarios with the best ask
#  	 volume consisting of ...
#    	(a) 1 sell limit order with volume 1000
#    	(b) 5 sell limit orders with volume 200 each
#       	(ordered according to time of submission)
#
#     The LOBSTER output for case ...
#       (a) shows one execution of volume 1000. If the
#           incoming market order is matched with one
#           standing limit order, execution and trade
#           coincide.
#       (b) shows 5 executions of volume 200 each with the
#           same time stamp. The incoming order is matched
#           with 5 standing limit orders and triggers 5
#           executions.
#
#       Bottom line:
#       LOBSTER records the exact limit orders against
#       which incoming market orders are executed. What
#       might be called 'economic' trade size has to be
#       inferred from the executions.



# Set Bounds for Intraday Intervals
# ----------------------------------------------------------

# Define interval length
freq = 5*60;   # Interval length in seconds 5 minutes

# Number of intervals from 9:30 to 4:00
noint= (endTrad-startTrad)/freq
ibm_msg_trading$index = seq(from=1,to=dim(ibm_msg_trading)[1])

# Variables for 'for' loop
j= 0
l =0
bound =0               	  # Variable for inverval bound
visible_count = 0         # visible_count calculates the number of visible trades in an interval of 5 min
hidden_count = 0          # hidden_count calculates the number of hidden trades in an interval of 5 min
visible_size = 0          # Total volume of visible trades in an interval of 5 minutes
hidden_size = 0           # Total volume of hidden trades in an interval of 5 minutes

# Set Bounds for Intraday Intervals
for(j in c(1:noint)) {
  
  bound[j+1] = startTrad + j * freq
}
bound[1] = startTrad

bound

for(l in c(1:noint)) 
{
  visible_count[l] = nrow(ibm_msg_trading[ibm_msg_trading$Time > bound[l] & ibm_msg_trading$Time < bound[l+1] & ibm_msg_trading$Type == 4,])
  visible_size[l] = sum(as.numeric(unlist(ibm_msg_trading[ibm_msg_trading$Time > bound[l] & ibm_msg_trading$Time < bound[l+1] & ibm_msg_trading$Type == 4,4])))/100
  
  hidden_count[l] = nrow(ibm_msg_trading[ibm_msg_trading$Time > bound[l] & ibm_msg_trading$Time < bound[l+1] & ibm_msg_trading$Type == 5,])
  hidden_size[l] = sum(as.numeric(unlist(ibm_msg_trading[ibm_msg_trading$Time > bound[l] & ibm_msg_trading$Time < bound[l+1] & ibm_msg_trading$Type == 5,4])))/100
  
}

# Divide plot area into two windows
par(mfrow=c(1,2) )

# Plot no of visible trades
plot(c(1:noint),visible_count , type ='h' , lwd = 5 , col = 'red' , ylim = c(-max(hidden_count), max(visible_count)) ,ylab ="Number of Executions" ,xlab = "Interval" )
title(sprintf("Number of Executions by Interval for %s " ,"IBM" ,cex = 0.8 ) )

# No of hidden trades in an invterval
lines(c(1:noint),-hidden_count, type ='h' , lwd = 5 , col = 'blue' )

# Legend
legend("top", c('Hidden' ,'Visible'),  col=c('blue','red'), horiz = TRUE , lty = 1 ,  inset = .05)

# Second plot of visible volume in an interval
plot(c(1:noint),visible_size, type ='h' , lwd = 5 , col = 'red' , ylim = c(-max(hidden_size), max(visible_size)) , ylab ="Volume of Trades(x100 shares)" ,xlab ="Interval")
title( sprintf("Trade Volume by Interval for %s " ,"IBM" ,cex = 0.8 ))

# Hidden volume in an interval
lines(c(1:noint),-hidden_size, type ='h' , lwd = 5 , col = 'blue' )

# Legend to second plot
legend("top", c('Hidden' ,'Visible'),  col=c('blue','red'), horiz = TRUE , lty = 1 ,  inset = .05)

par(mfrow=c(1,1))


##########
#(iii)

######################
#Now look at order book data
#read in quote data from IBM on 05/07/2016, i.e. one single day

ibm_raw_ob<-fread("IBM_2016-07-05_orderbook.csv",data.table = FALSE)

columns2 <- c("ASKp1" , "ASKs1" , "BIDp1",  "BIDs1")
no_levels<-5 #we have 5 levels here of the  limit order book, i.e. 5 limit orders in each direction

# naming the columns of data frame                                          
if (no_levels > 1)
{
  for ( i in 2:no_levels )
  { 
    columns2 <- c (columns2,paste("ASKp",i,sep=""), paste("ASKs",i,sep=""),paste("BIDp",i,sep=""),paste("BIDs",i,sep="")) 
  }
}

colnames ( ibm_raw_ob ) <- columns2

head(ibm_raw_ob)
#fwrite(ibm_raw_ob,"IBM_ob.csv")

#since trades are all in time window and no illegal trades are present, no cleaning is necessary here

# Convert prices into dollars
#    Note: LOBSTER stores prices in dollar price times 10000

for ( i in c(seq(from = 1, length=2*no_levels, by = 2)) ) 
{ 
  ibm_raw_ob[,i] <- ibm_raw_ob[,i]/10000 
}

#Lets get first level midprices and combine time-stamp to it

#mid-price as the middle between best ask and best bid
mid_price_level1<-(ibm_raw_ob$ASKp1+ibm_raw_ob$BIDp1)/2

ibm_final<-data.table(DT=ibm_msg_trading$Time,PRICE=mid_price_level1)

#for correct dates, we need to convert stuff, e.g. with the helpful functions
#from the lubridate package
library(lubridate)
ibm_final$DT<-as.Date(seconds_to_period(ibm_final$DT),origin="2016-07-05")
#we lose the milliseconds here

#could use base R if we needed it (does not work as well with highfrequency package)
?as.POSIXct #counts from 2 AM 
ibm_final$Time<-format(as.POSIXct(ibm_final$DT-7200, origin = "2016-07-05"), "%Y-%m-%d %H:%M:%OS2")

#compare the two:
head(ibm_final,100)

##########
#(iv)

#aggregate prices to equispaced interval
ibm_sec<-aggregatePrice(ibm_final,alignBy = "seconds",alignPeriod = 5)
ibm_min1<-aggregatePrice(ibm_final,alignBy = "minutes",alignPeriod = 1)
ibm_min5<-aggregatePrice(ibm_final,alignBy = "minutes",alignPeriod = 5)


plot(ibm_sec$DT,ibm_sec$PRICE,type="l")
plot(ibm_min1$DT,ibm_min1$PRICE,type="l")
plot(ibm_min5$DT,ibm_min5$PRICE,type="l")

#get return, just look at 5min for now

ibm_ret<-diff(ibm_min5$PRICE)/ibm_min5$PRICE[-length(ibm_min5$PRICE)]
plot(ibm_min5$DT[1:(dim(ibm_min5)[1]-1)],ibm_ret,type="l")
acf(ibm_ret)
pacf(ibm_ret)

#do the same for log-return yourself

##########
#(v)
##get realized Vola measures

#automatic
auto_kd<-spotVol(ibm_final,method="kernel") #kernel density
auto_rv<-spotVol(ibm_final,method="RM",RM=c("rv")) 
auto_rk<-rKernelCov(ibm_final[,1:2],alignBy = "minutes",alignPeriod = 5)


#let's do the classic realized vola manually for different sampling frequencies
# and do a volatility

#simple realized vola for different deltas
library(xts)
#for 5 second interval and 20 sec
delta_seq<-seq(10,60*10,by=5)
rv_time<-data.frame(rv=rep(NA,length(delta_seq)),freq=delta_seq)
rk_time<-data.frame(rv=rep(NA,length(delta_seq)),freq=delta_seq)
#IBM Returns

for(i in 1:length(delta_seq))
{
  #get prices in sequence
  agg_prices<-aggregatePrice(ibm_final,alignBy = "seconds",alignPeriod = delta_seq[i])
  #logret_temp<-diff(log(agg_prices$PRICE)) #get log returns
  ret_temp<- diff(agg_prices$PRICE)/agg_prices$PRICE[-length(agg_prices$PRICE)]#get normal returns
  xts_ret<-xts(ret_temp,agg_prices$DT[-length(agg_prices$DT)]) #use xts for better compatibility
  rv_time[i,1]<-sum(ret_temp^2) #get realized vola
  #get realized kernel vola
  rk_time[i,1]<-rKernelCov(xts_ret,makeReturns = FALSE,alignBy = NULL)
  if(i%%50==0) print(i)
}



rv_5sec<-list(RV=rv_time,RK=rk_time)

#for 20 second interval 
delta_seq<-seq(10,60*10,by=20)
rv_time<-data.frame(rv=rep(NA,length(delta_seq)),freq=delta_seq)
rk_time<-data.frame(rv=rep(NA,length(delta_seq)),freq=delta_seq)
#IBM Returns

for(i in 1:length(delta_seq))
{
  #get prices in sequence
  agg_prices<-aggregatePrice(ibm_final,alignBy = "seconds",alignPeriod = delta_seq[i])
  #logret_temp<-diff(log(agg_prices$PRICE)) #get log returns
  ret_temp<- diff(agg_prices$PRICE)/agg_prices$PRICE[-length(agg_prices$PRICE)]#get normal returns
  xts_ret<-xts(ret_temp,agg_prices$DT[-length(agg_prices$DT)]) #use xts for better compatibility
  rv_time[i,1]<-sum(ret_temp^2) #get realized vola
  #get realized kernel vola
  rk_time[i,1]<-rKernelCov(xts_ret,makeReturns = FALSE,alignBy = NULL)
  if(i%%50==0) print(i)
}

rv_20sec<-list(RV=rv_time,RK=rk_time)

#plot RV for 20 sec interval and for 5 sec interval
plot(rv_5sec$RV$freq/60,rv_5sec$RV$rv*1000,type="l",ylim=c(0.04,0.11),
     main="RV Vol. Signature Plot for Returns",xlab="frequency in minutes",ylab="RV times 1000")
lines(rv_20sec$RV$freq/60,rv_20sec$RV$rv*1000,col="red",lwd=2)

plot(rv_5sec$RK$freq/60,rv_5sec$RK$rv*1000,type="l",ylim=c(0.04,0.11),
     main="RK Vol. Signature Plot for Returns",xlab="frequency in minutes",ylab="RV times 1000")
lines(rv_20sec$RK$freq/60,rv_20sec$RK$rv*1000,col="red",lwd=2)

#how to get smoother plots
#longer time interval, larger bandwidth in parameters
#always think about these when looking in plots in papers

#######################################
############
#Problem 8
# Data of The National Income and Product Accounts of the United States from Q1 1947
#y1 - Fixed investment,
#y2 - first differences of change in business inventories,

##########
#(i)

inv_data<-read.table("US_investment.txt",header=TRUE)
inv_data$date<-seq(as.Date("1947/01/01"),by="quarter",length.out = dim(inv_data)[1])

#plot the time series
par(mfrow=c(1,2))
plot(inv_data$date, inv_data$y1,type="l",main="Fixed Investment")
plot(inv_data$date,inv_data$y2,type="l",main="Change in Business Inventories")
par(mfrow=c(1,1))

library(vars)
##########
#(ii)
#estimate different models
var_const<-VAR(inv_data[,1:2],p=1)
var_trend<-VAR(inv_data[,1:2],p=1,type="trend")
var_both<-VAR(inv_data[,1:2],p=1,type="both")
var_const
var_trend
var_both
plot(var_const)
plot(var_trend)
plot(var_both)
#MA representation
phi_const<-Phi(var_const,nstep=500)
phi_const[,,c(1,2,501)] #diverge
phi_trend<-Phi(var_trend,nstep=500)
phi_trend[,,c(1,2,501)] #not going to zero
phi_both<-Phi(var_both,nstep=500)
phi_both[,,c(1,2,501)] #going to zero
