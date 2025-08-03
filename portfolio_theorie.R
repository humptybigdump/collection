# Major  parts of the code are from:
# https://www.r-bloggers.com/2016/05/a-gentle-introduction-to-finance-using-r-efficient-frontier-and-capm-part-1/
# It's broadly adapted to the notation we use in the lecture.
# R-Bloggers is generally pretty cool spot for keeping up to date with R related stuff, als well as learning R from scratch.

# In case you are new to R, you have to install the following packages first.
# First remove the "#" symbol at the beginning of each install.package line and then run all of these lines via (strg + enter or command + enter).
# After having installed the packages once, you don't need to re-install for a potential future execution.
# Hence, after installation you can put back the "#" symbol at the beginning of each of those installation lines.
#install.packages("data.table")
#install.packages("scales")
#install.packages("ggplot2")
#install.packages("quantmod")
#install.packages("tseries")
#install.packages("tidyfast")

library(data.table)
library(scales)
library(ggplot2)
library(quantmod)
library(tseries)
library(tidyfast)
library(tidyverse)

# In case you are getting an error linked to the qunatmod package in the line:
#
# stock_x_dt=as.data.table(getSymbols(ticker_x,from=start_date,to=end_date,auto.assign = F),keep.rownames = TRUE)
#
# First go to "Session --> "Restart R" and then try to directly execute the following lines
# (you need to remove the hash tag at the beginning of the line first and then run the line).
# Select 1 --> "All" when it asks you which packeges you want to install/update.
# install.packages("remotes")
# library(remotes)
# remotes::install_github("joshuaulrich/quantmod@358-getsymbols-new.session")

# After having executed these lines, put back the hashtag symbol at the beginning of the lines. Then you should be able to re-run
# the entire other code.


# Note 1 : To get a list of all available tickers you can run the following lines of code:

#install.packages("TTR")
#library(TTR)
#all_stock_symbols <- stockSymbols()
#View(all_stock_symbols)

# Note 2: If you are looking for specific stocks, you can simply google for the Company name + Stock Ticker
# e.g.: "Apple Stock Ticker" --> "AAPL".

# Note 3: Warnings can be safely ignored :-).

###################
# Start of the real ;-)  code
###################

# First we want to download some stock data and visualize it. We choose two distinct tickers for this
# initial task.
# 1.Visualize price developments

# Insert stock tickers

# Stocks 1: Technologieunternehmen, Gesundheitswesen, Konsumgüter
ticker_x <- "AAPL"
ticker_y <- "PFE"
ticker_z <- "KO"
# ticker_z <- "TSLA" # comment this line in to use a different third stock

# Stocks 2: Unterhaltung, Finanzinstitute, Energieunternehmen
ticker_x <- "NFLX"
ticker_y <- "JPM"
ticker_z <- "BP"

# Insert start and end date of the wished period
start_date <- as.Date("2017-05-19")
# end_date <- as.Date("2023-05-20")
end_date <- Sys.Date() 

# Retrieve stock data
stock_x_dt=as.data.table(getSymbols(ticker_x,from=start_date,to=end_date,auto.assign = FALSE),keep.rownames = TRUE)
stock_x_dt$ticker <- ticker_x
names(stock_x_dt) <- c("date", "open","high","low","close","volume","adjusted_price","ticker")

stock_y_dt=as.data.table(getSymbols(ticker_y,from=start_date,to=end_date,auto.assign = FALSE),keep.rownames = TRUE)
stock_y_dt$ticker <- ticker_y
names(stock_y_dt) <- c("date", "open","high","low","close","volume","adjusted_price","ticker")

stock_z_dt=as.data.table(getSymbols(ticker_z,from=start_date,to=end_date,auto.assign = FALSE),keep.rownames = TRUE)
stock_z_dt$ticker <- ticker_z
names(stock_z_dt) <- c("date", "open","high","low","close","volume","adjusted_price","ticker")


# Put everything together in a data.table
dt <- rbindlist(list(stock_x_dt,stock_y_dt, stock_z_dt))
# Parse the date column into a "real" date
dt[, date := as.Date(as.character(date))]

# View(dt) # Uncomment this line to show the table you have just generated.

# Create indexed values
# Hint: We call it index since it's a normalized version of the price + dividend return of the stock (--> total return).
# It's normalized to the value of 1 a the beginning of our observation time frame. So basically, this is just one way
# of expressing the total return of a stock.
dt[, idx_price := adjusted_price/adjusted_price[1], by = ticker]

#Plot the indexed values
ggplot(dt, aes(x = date, y = idx_price, color = ticker)) +
  geom_line() +
  # Formatting
  theme_bw() + ggtitle("Price Development") +
  xlab("Date") + ylab("Normalized Total Return Index") +
  scale_color_discrete(name = "Company") +
  theme(plot.title = element_text(hjust = 0.5)) 


# 2. Calculate risk-return rradeoff for single stocks

# Calculate the returns
dt[, ret := adjusted_price / shift(adjusted_price, 1) - 1, by = ticker]

# Now fokus on the two assets case. Examine ticker_x and ticker_y data:
sum <- dt[!is.na(ret) & ticker %in% c(ticker_x, ticker_y), .(ticker, ret)]

#Calculate the expected returns (historical mean of returns) and volatility (standard deviation of returns)
sum <- sum[, .(er = round(mean(ret), 4),
               sd = round(sd(ret), 4)),
           by = "ticker"]

# 3. Visualize risk-return tradeoff for the two stocks (this is the mu-sigma diagram)
ggplot(sum, aes(x = sd, y = er, color = ticker)) +
  geom_point(size = 5) +
  # Formatting
  theme_bw() + ggtitle("Risk-Return Tradeoff") +
  xlab("Volatility") + ylab("Expected Return")+
  expand_limits(x = 0, y = 0) + 
  theme(plot.title = element_text(hjust = 0.5)) 

# 4. Calculate the risk-return tradeoff of a portfolio

# 4.1 Two assets case

# I) Expected returns 
mu_x <- mean(dt[ticker==ticker_x,]$ret, na.rm = TRUE)
mu_y <- mean(dt[ticker==ticker_y,]$ret, na.rm= TRUE)

# II) Standard deviations
sigma_x <- sd(dt[ticker==ticker_x,]$ret, na.rm = TRUE)
sigma_y <- sd(dt[ticker==ticker_y,]$ret, na.rm = TRUE)

# III) Covariance
# In order to calculate the covariance we have to make sure that the returns are aligned by the
# correct date.
dt_wide <- dt_pivot_wider(dt[,.(date,ticker,ret)], names_from=ticker, values_from=ret)
cov_xy <- cov(dt_wide[,get(ticker_x)], dt_wide[,get(ticker_y)], use="pairwise.complete.obs") 

# Actually we only need either cov or cor, but just in case, here is cor, as well:
cor_xy <- cov_xy / (sigma_x * sigma_y)

#Create nstep portfolio weights
nstep <-100
start_w <- -1
end_w <- 2

x_weights <- seq(from = start_w, to = end_w, length.out = nstep)

#Create a data.table that contains the weights for the two assets
two_assets <- data.table(w_x = x_weights,
                         w_y = 1 - x_weights)

#Calculate the expected returns and standard deviations for the nstep possible portfolios
two_assets[, ':=' (mu_p = w_x * mu_x + w_y * mu_y,
                   sigma_p = sqrt(w_x^2 * sigma_x^2 +
                                 w_y^2 * sigma_y^2 +
                                 2 * w_x * (1 - w_x) * cov_xy))]

# Plot the possible portfolios 
ggplot() +
  
  #Data
  geom_point(data = two_assets, aes(x = sigma_p, y = mu_p, color = w_x)) +
  geom_point(data = data.table(sd = c(sigma_x, sigma_y), mean = c(mu_x, mu_y)),
             aes(x = sd, y = mean), color = "red", size = 3, shape = 18) +
  # Add the name of the stocks
  geom_text(data= data.table(sd = c(sigma_x, sigma_y), mean = c(mu_x, mu_y)), aes(x = sd, y = mean, label = c(ticker_x, ticker_y)), vjust = 0, hjust=-0.2) +
  
   #Formatting
  theme_bw() + ggtitle("Possible Portfolios with Two Risky Assets") +
  xlab("Volatility") + ylab("Expected Return") +
  scale_y_continuous(label = percent, limits = c(0, max(two_assets$mu_p) * 1.2)) +
  scale_x_continuous(label = percent, limits = c(0, max(two_assets$sigma_p) * 1.2)) +
  scale_color_continuous(name = expression(w[x]), labels = percent) +
  theme(plot.title = element_text(hjust = 0.5)) 

# Now kick in the third stock with ticker_z from here on
# 4.2 Three Assets Case

# I) Expected return
mu_z <- mean(dt[ticker==ticker_z,]$ret, na.rm= TRUE)

# II) Standard deviation
sigma_z <- sd(dt[ticker==ticker_z,]$ret, na.rm= TRUE)

# III) Covariance
# We know the first one from above, alredy.
# cov_xy <- cov(dt_wide[,get(ticker_x)], dt_wide[,get(ticker_y)], use="pairwise.complete.obs") 
cov_xz <- cov(dt_wide[,get(ticker_x)], dt_wide[,get(ticker_z)], use="pairwise.complete.obs") 
cov_yz <- cov(dt_wide[,get(ticker_y)], dt_wide[,get(ticker_z)], use="pairwise.complete.obs") 


#Create portfolio weights
start_wn <- 0
end_wn <- 1
x_weights <- seq(from = start_wn, to = end_wn, length.out = 100)

#Create a data.table that contains the weights for the three assets
three_assets <- data.table(w_x = rep(x_weights, each = length(x_weights)),
                           w_y = rep(x_weights, length(x_weights)))

three_assets[, w_z := 1 - w_x - w_y]

# Uncomment to view the table which you have produced here.
# View(three_assets)

#Calculate the expected returns and standard deviations for the possible portfolios
three_assets[, ':=' (mu_p = w_x * mu_x + w_y * mu_y + w_z * mu_z,
                     sigma_p = sqrt(w_x^2 * sigma_x^2 +
                                   w_y^2 * sigma_y^2 +
                                   w_z^2 * sigma_z^2 +
                                   2 * w_x * w_y * cov_xy +
                                   2 * w_x * w_z * cov_xz +
                                   2 * w_y * w_z * cov_yz))]

#Take out cases where we have negative weights (shortselling)
three_assets_no_short <- three_assets[w_x >= 0 & w_y >= 0 & w_z >= 0]

# Just for the sake of it, here is the cor matrix, as well.
#cor <- matrix(rbind(c(1,cov_xy/(sigma_x*sigma_y),cov_xz/(sigma_x*sigma_z)),
#              c(cov_xy/(sigma_x*sigma_y),1,cov_yz/(sigma_y*sigma_z)),
#              c(cov_xz/(sigma_x*sigma_z),cov_yz/(sigma_y*sigma_z),1)),nrow=3, ncol = 3)

# Plot the values
ggplot() +
  
  # Data
  geom_point(data = three_assets_no_short, aes(x = sigma_p, y = mu_p, color = w_x - w_z)) +
  geom_point(data = data.table(sd = c(sigma_x, sigma_y, sigma_z), mean = c(mu_x, mu_y, mu_z)),
             aes(x = sd, y = mean), color = "red", size = 3, shape = 18) +
  
  # Add the name of the stocks
  geom_text(data = data.table(sd = c(sigma_x, sigma_y, sigma_z), mean = c(mu_x, mu_y, mu_z)), aes(x = sd, y = mean, label = c(ticker_x, ticker_y, ticker_z)), hjust = -0.2) +
  
  # Formatting
  theme_bw() + ggtitle("Possible Portfolios With Three Risky Assets") +
  xlab("Volatility") + ylab("Expected Return") + 
  scale_y_continuous(label = percent, limits = c(0, max(three_assets$mu_p) * 1.2)) +
  scale_x_continuous(label = percent, limits = c(0, max(three_assets$sigma_p) * 1.2)) +
  scale_color_gradientn(colors = c("red", "blue", "yellow"),
                       name = (w[x] ~ "-" ~ w[z]), labels = percent) +
  theme(plot.title = element_text(hjust = 0.5)) 




# Now from here on we implicitly switch to the matrix notation

#5.Calculating Efficient Frontier 

#5.1 With short-selling

# This function calculates abcd (alpha beta gamma delta)
calcEFParams <- function(rets) {
  
  retbar <- colMeans(rets, na.rm = TRUE)
  
#Calculate the covariance of the returns
  covs <- var(rets, na.rm = TRUE) 
  invS <- solve(covs)
  i <- matrix(1, nrow = length(retbar))
  
  alpha <- t(i) %*% invS %*% i
  beta <- t(i) %*% invS %*% retbar
  gamma <- t(retbar) %*% invS %*% retbar
  delta <- alpha * gamma - beta * beta
  
  retlist <- list(alpha = as.numeric(alpha),
                  beta = as.numeric(beta),
                  gamma = as.numeric(gamma),
                  delta = as.numeric(delta))
  
  return(retlist)
}

abcds <- calcEFParams(dt_wide[,-c("date")])

calcEFValues <- function(x, abcd, upper = TRUE) {
  alpha <- abcd$alpha
  beta <- abcd$beta
  gamma <- abcd$gamma
  delta <- abcd$delta
  
  if (upper) {
    retval <- beta / alpha + sqrt((beta / alpha) ^ 2 - (gamma - delta * x ^ 2) / (alpha))
  } else {
    retval <- beta / alpha - sqrt((beta / alpha) ^ 2 - (gamma - delta * x ^ 2) / (alpha))
  }
  
  return(retval)
}

#Calculate the risk-return tradeoff the two assets (for plotting the points)
df_table <- dt_wide %>% select(-date) %>%
  pivot_longer(cols=everything(), names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  summarise(er=mean(value, na.rm = TRUE), sd=sd(value, na.rm = TRUE))


# Plot the values
ggplot(df_table, aes(x = sd, y = er)) +

  #Add the stocks
  geom_point(size = 4, color = "red", shape = 18) +
  
  # Add the name of the stocks
  geom_text(data = df_table, aes(x = sd, y = er, label = df_table$variable), hjust = -0.3) +

  
  #Add the upper efficient frontier
  stat_function(fun = calcEFValues, args = list(abcd = abcds, upper = TRUE), n = 10000,
                color = "red", size = 1) +
  
  #Add the lower efficient frontier
  stat_function(fun = calcEFValues, args = list(abcd = abcds, upper = FALSE), n = 10000,
                color = "blue", size = 1) +
  
  #Formatting
  theme_bw() + ggtitle("Efficient Frontier with Short-Selling") +
  xlab("Volatility") + ylab("Expected Return") +
  scale_y_continuous(label = percent, limits = c(0, max(df_table$er) * 1.2)) +
  scale_x_continuous(label = percent, limits = c(0, max(df_table$sd) * 1.2)) +
  theme(plot.title = element_text(hjust = 0.5)) 

# 5.2 Without short-selling
dh_table <- dt_wide %>% select(-date) %>%
  pivot_longer(cols=everything(), names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  summarise(er=mean(value, na.rm = TRUE), sd=sd(value, na.rm = TRUE))
 
er_vals <- seq(from = min(dh_table$er), to = max(dh_table$er), length.out = 1000)

# Find an optimal portfolio for each possible possible expected return
sd_vals <- sapply(er_vals, function(er) {
  op <- portfolio.optim(as.matrix(na.omit(dt_wide[,-c("date")])), er)
  return(op$ps)
})

plot_dt <- data.table(sd = sd_vals, er = er_vals)

#Find the lower and the upper frontier
minsd <- min(plot_dt$sd)
minsd_er <- plot_dt[sd == minsd, er]
plot_dt[, efficient := er >= minsd_er]

# Plot the values
ggplot() +
  # Add Data
  geom_point(data = plot_dt[efficient == FALSE], aes(x = sd, y = er), size = 0.5, color = "blue") +
  geom_point(data = plot_dt[efficient == TRUE], aes(x = sd, y = er), size = 0.5, color = "red") +
  geom_point(data = df_table, aes(x = sd, y = er), size = 4, color = "red", shape = 18) +
  
  # Add the name of the stocks
  geom_text(data = df_table, aes(x = sd, y = er, label = c(ticker_x, ticker_y, ticker_z)), vjust = -1) +
  
  # Formatting
  theme_bw() + ggtitle("Efficient Frontier without Short-Selling") +
  xlab("Volatility") + ylab("Expected Return") +
  scale_y_continuous(label = percent, limits = c(0, max(df_table$er) * 1.2)) +
  scale_x_continuous(label = percent, limits = c(0, max(df_table$sd) * 1.2)) + 
  theme(plot.title = element_text(hjust = 0.5)) 

