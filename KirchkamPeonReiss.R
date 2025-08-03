#data <- read.table('yourpath/KirchkampPeonReissSPA-Data.txt', header=TRUE, sep="\t")

attach(data)

# bid_50, ... bid_100 bids according to strategy method for avlues of 50,...,100
# b1,..., b5 realized bids in the five selected auctions
# m_b1,...,m_b5 bid of competitor
# p1,...p5 payoff in each round
# v1, ..., v5 values 
# w1, ... w5 win auction:1 lose:0 