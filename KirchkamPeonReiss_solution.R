data <- read.table('H:/KIT/PMD/PMD/PS2/KirchkampPeonReissSPA-Data.txt', header=TRUE, sep="\t")

attach(data)

# bid_50, ... bid_100 bids according to strategy method for avlues of 50,...,100
# b1,..., b5 realized bids in the five selected auctions
# m_b1,...,m_b5 bid of competitor
# p1,...p5 payoff
# v1, ..., v5 values 
# w1, ... w5 win auction:1 lose:0 

# average price 

data$price1[b1< m_b1] <-   b1
data$price1[b1>=m_b1] <- m_b1

data$price2[b2< m_b2] <-   b2
data$price2[b2>=m_b2] <- m_b2

data$price3[b3< m_b3] <-   b3
data$price3[b3>=m_b3] <- m_b3

data$price4[b4< m_b4] <-   b4
data$price4[b4>=m_b4] <- m_b4

data$price5[b5< m_b5] <-   b5
data$price5[b5>=m_b5] <- m_b5



#data$avgpriceman <- (data$price1+data$price2+data$price3+data$price4+data$price5)/5
# calculates mean row-by-row and stores in avgprice
data$avgprice <- rowMeans(data[ ,c("price1","price2","price3","price4","price5")], na.rm=TRUE)

mean(data$avgprice)



### Average total surplus
# Equal to value of winner

#remove NA row

#data[,]

# m_v1 -> other player's valuation 

# kick out rows 1,2 since we have missing
#data[rows, columns] c is vector

data2 <- data[c(-1,-2), ]

rownames(data2) <- NULL  # resets row names

detach(data)
attach(data2)

data2$s1[w1==1] <- v1 
data2$s1[w1==0] <- m_v1 

data2$s2[w1==1] <- v2
data2$s2[w1==0] <- m_v2

data2$s3[w1==1] <- v3
data2$s3[w1==0] <- m_v3

data2$s4[w1==1] <- v4
data2$s4[w1==0] <- m_v4

data2$s5[w1==1] <- v5
data2$s5[w1==0] <- m_v5

data2$avgsurplus <- rowMeans(data2[ ,c("s1","s2","s3","s4","s5")], na.rm=TRUE)

mean(data2$avgsurplus,na.rm=TRUE)


#Is there over- or underbidding on average?
# average bidding strategy.

y=c(mean(bid_50),mean(bid_60),mean(bid_70),mean(bid_80),mean(bid_90),mean(bid_100))
x=seq(50,100,10) #this means (starting number, ending number, interval)

plot(x,y, abline(0,1), ylim=c(0,100))


#-> overbidding on average

#  Identify at least one subject that consistently over- or underbids and show that subject's bidding strategy.
# find consistent over- or underbidders
data2$overbidder <-0
data2$overbidder[bid_50>50 & bid_60>60 & bid_70>70 & bid_80>80 & bid_90>90 & bid_100>100] <-1

#rowname 37
yo=c(bid_50[37],bid_60[37],bid_70[37],bid_80[37],bid_90[37],bid_100[37])
xo=seq(50,100,10) #this means (starting number, ending number, interval)

plot(xo,yo, abline(0,1), ylim=c(0,120))

data2$underbidder <-0
data2$underbidder[bid_50<50&bid_60<60&bid_70<70&bid_80<80&bid_90<90&bid_100<100] <-1


yu=c(bid_50[4],bid_60[4],bid_70[4],bid_80[4],bid_90[4],bid_100[4])
xu=seq(50,100,10) #this means (starting number, ending number, interval)

plot(xu,yu, abline(0,1), ylim=c(0,100))


# asymmetric equilibria?
# - long format


wide <- data[ ,c("bid_50","bid_60","bid_70","bid_80","bid_90","bid_100")]

#seq.int integer sequence - nrow - number of rows
# -> creates an integer sequence up to nrow
wide$id <-  seq.int(nrow(wide))

#reshape(data, idvariable, varying variables, values over long format )
long <- reshape(data=wide,idvar="id",
                         varying = c("bid_50","bid_60","bid_70","bid_80","bid_90","bid_100"),
                         v.name=c("bid"),
                         times=seq(50,100,10),
                         direction="long")
ordereddata <- long[order(long$id, long$time),]

#time is condition here

attach(ordereddata)
cor(time,bid)
plot(time, bid,abline(0,1))

#high correlation as indication for no-asymmetric equilibria - would actually have to look at each individual to fully answer question


