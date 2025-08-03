#this program approximates Figure 3A of Hahn & Warren (2009) with Monte Carlo simulation
#you can try to do Figure 3B yourself as a practice. The logic is quite similar

#install R package
install.packages("cquad")
#load R package
library(cquad)

#read this from the most inside loop and working outward (i -> j -> k)
wait_mean_vec <- c()
#k corresponds to the 16 possible 0/1 sequences with length 4
for (k in 1:16) {
  wait_vec <- c()
  #j corresponds to the number of experiments/samplings
  for (j in 1:10000) {
    # n is the length of the sequence. This needs to be sufficiently long so that all subsequences appear, but not too long that the program becomes too slow.
    n <- 1000
    sequence <- sample(c(0,1), replace=TRUE, size = n)
    wait <- 0
    #i is the moving window inside n
    for (i in 4:n) {
      if (all(sequence[(i-3):i] == sq(4)[k,])) {
        wait <- i
        #terminate the loop once a match is found
        break
      }
    }
    #write the result of each loop into a vector
    wait_vec[j] <- wait
  }
  #store the average waiting time into variables with names like "wait1010"
  name_suffix <- paste(as.character(sq(4)[k,]),collapse = "")
  name <- paste("wait", name_suffix, sep = "")
  assign(name,mean(wait_vec))
  #combine the average waiting times for each subsequence into a vector for the purpose of graphing
  wait_mean_vec[k] <- mean(wait_vec)
}

#draw the figure
plot(wait_mean_vec, type="l")
