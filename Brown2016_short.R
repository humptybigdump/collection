library(plot.matrix)

board <- matrix(data=0, nrow=99, ncol=99)

#i: sample size; j: decimal place
for (i in 1:99) {
  consistent <- round(1:i/i,2)
  for (j in 1:99) {
    board[j,i] <- ifelse(is.element(j/100, consistent), 1, 0)
  }
}

plot(board, col=c("black", "white"),border=NA)
