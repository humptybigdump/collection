## Example 1, Binomial distribution ##
## X ~ Bin(1000,0.5)

factorial(500)
log( factorial(500) )
lfactorial(500) # ln(500!)

exp( lfactorial(1000) - 2*lfactorial(500) )
choose(1000, 500)

choose(1000, 500)* 0.5^1000  # or:
dbinom(500,1000,0.5)  # P(X=500)

# plots
barplot( dbinom(0:1000, 1000, 0.5), names.arg = c(0:1000), 
         col = "red", space = 1, ylab="probability mass function")
barplot( dbinom(430:570, 1000, 0.5), names.arg = c(430:570), 
         col = "red", space = 1, ylab="probability mass function")
barplot( dbinom(470:530, 1000, 0.5), names.arg = c(470:530), 
         col = "red", space = 1, ylab="probability mass function")

dbinom(500,1000,0.5)  # P(X=500)
####################################################################

# Example 2, Binomial distribution #
# X ~ Bin(20,0.8)

dbinom(20,20,0.8)
dbinom(16,20,0.8)
dbinom(16:20,20,0.8)
sum( dbinom(16:20,20,0.8) )
# or use P(X=16)+...+P(X=20) = 1-P(X=0)-...-P(X=15) = 1-P(X<=15)
1 - pbinom(15,20,0.8) 

pbinom(9,20,0.8) # P(X<=9) = 0.06%
barplot( dbinom(0:20, 20, 0.8), names.arg = c(0:20), 
         col = "red", space = 1, ylab="probability mass function")

## random number generation
x = rbinom(10, 20, 0.8) # generate 10 pseudo random numbers
x

par(mfrow=c(2,2), mar=c(2,3,1,1))
set.seed = 999
barplot( dbinom(0:20, 20, 0.8), names.arg = c(0:20), ylim=c(0,0.25), 
         col = "black", space = 1, ylab="")
for (i in 1:3) {
  n = 50
  x = rbinom(n, 20, 0.8) # generate n pseudo random numbers
  barplot( table( factor( x, levels=0:20) ) / n, ylim=c(0,0.25),
           col = i+1, space = 1, ylab="barplot")
}
