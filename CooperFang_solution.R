# Problem Set 1 - Cooper & Fang data on 2nd price auction

# Load data and "attach" so that you can directly access the variables
# Note "/" has to be used
data <- read.table('H:/KIT/PMD/PMD/data/CooperFang.txt', header=TRUE, sep="\t")

?read.table
attach(data)
#Note that attach might sometimes cause problems -> Simply restart the session in that case

#Each row in the data set is one subject in a single round
# Important variables:
# value: own value
# ovalue: opponent's value
# bid: ownbid
# opbid: opponent's bid
# dwin: 1 if subject won, 0 otherwise
# matchid: numbering each individual interaction



# Let's start with some descriptive analysis

# Do actual valuations resemble distribution in the experimental design?
hist(value)

#How do bids look like? 
# You can kick out some extremely high bids to make the graph more readable
hist(bid[bid<15000])
hist(bid)
#Your turn!

# Is there strong over- or underbidding? 
plot(value[bid<12500],bid[bid<12500],abline(0,1))

# abline(yintercept,slope)


#Start with plotting bids on valuations in a scatter plot. You can again restrict the sample
# (using abline you can include a 45-degree line)

# Is there over or under-bidding? How high is the share of bids according to theory?

data$overunder[bid<value] <- "under"
data$overunder[bid>value] <- "over"
data$overunder[bid==value] <- "correct"

table <- table(data$overunder)
table 
prop.table(table)

# Calculate the average efficiency rate
# Hint: use "pmax" not "max". 
#?max() for explanation

data$eff <- value/pmax(value,ovalue)
hist(data$eff[dwin==1])
mean(data$eff[dwin==1])
