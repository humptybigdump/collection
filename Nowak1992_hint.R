# The evolution of strategies in the prisoner's dilemma in Nowak & May (1992)
# Citation: Nowak, Martin & May, Robert. (1992). Evolutionary Games and Spatial Chaos. Nature. 359. 826-829. 10.1038/359826a0. 
# Strategies: C=0; D=1
# payoffs: CC=1; DC=b (b>1); CD=0; DD=0
# Rules: In each round, every player plays with its 9 immediate neighbours (including self). Player with the highest sum of payoffs occupies that cell.
# Color: C->C blue; C->D yellow; D->C green; D->D red

#Structuring the data like "Brown2016_short.R" and use the "plot.matrix" package is probably easier than structuring the data like "Brown2016.R". This piece of hint correspond to the "plot.matrix" approach.
library(plot.matrix)


#define variables and matrices here

neighbors <- function(mat, i,j) {
   rows <- subset((i-1):(i+1),(i-1):(i+1)>=1 & (i-1):(i+1)<=99)
   columns <- subset((j-1):(j+1),(j-1):(j+1)>=1 & (j-1):(j+1)<=99)
   mat[rows,columns]
}




