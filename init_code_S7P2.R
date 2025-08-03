#####################################################################
# From code_S3P3:

# Load data
load("obsforc.Rdata")

for(i in 1:17){
  if(i == 1) Y <- obsforc[,i]
  if(i > 1){
    # Even column index: mu_i; odd column index: sd_i
    if(i%%2 == 0){
      assign(x = paste0("mu",i/2), value = obsforc[,i])
    }else{
      assign(x = paste0("sd",(i-1)/2), value = obsforc[,i])
    }
  }
}

# Marginal calibration
# Empirical average forecast distributions
Fn <- function(mu, sigma){
  return(function(x) sapply(x,function(x) mean(pnorm(x,mu,sigma))))
}
# Empirical marginal distribution of observations
FnY <- ecdf(x = Y)
# Differences in marginal distribution
par(mfrow = c(2,4))
for(i in 1:8){
  assign(x = paste0("Fn",i), value = Fn(get(paste0("mu",i)),get(paste0("sd",i))))
  plot(x = function(x) get(paste0("Fn",i))(x) - FnY(x), col = "red", main = paste0("F",i),ylab = paste0("Fn",i,"(x) - FnY(x)"),xlim = c(-7,7),ylim = c(-0.1,0.1))
  abline(h = 0,lty = 2,col = "grey")
}
marg = c(T,F,T,T,T,T,F,T)

# Probabilistic calibration
# PIT values
PIT.values <- function(Y, mu, sigma){
  y.norm    <- (Y-mu)/sigma
  PIT.value <- pnorm(y.norm)
  return(PIT.value)
}
# PIT histograms
for(i in 1:8){
  assign(x = paste0("PIT",i), value = PIT.values(Y = Y, 
                                                 mu = get(paste0("mu",i)), sigma = get(paste0("sd",i))))
  hist(get(paste0("PIT",i)), freq = FALSE, col = grey(0.8), 
       main = paste0("PIT",i), xlab = paste0("Forc ",i))
  abline(h = 1, lty = 2)
}
prob = c(F,F,T,T,T,T,F,F)

# Sharpness + Ranking
# e.g. look at variance (constant for each forecaster!)
sd = rep(NA,8)
for(i in 1:8){
  sd[i] = get(paste0("sd",i))[1]
}
data.frame(marg,prob,sd)
ranking = c(3,5,4,6,1,8,2,7) #exemplary ranking by sharpness subject to calibration
data.frame(marg,prob,sd)[ranking,]

#####################################################################
# From code_S5P2.R

# Evaluation by CRPS
crps.normal <- function(Y, mu, sigma){
  y.norm <- (Y-mu)/sigma
  crps.normalforc <- sigma*(y.norm*(2*pnorm(y.norm)-1)+2*dnorm(y.norm)-1/sqrt(pi))
  return(crps.normalforc)
}
meanCRPS <- rep(NA, 8)
for(i in 1:8){
  meanCRPS[i] <- mean(crps.normal(Y = Y,
                                  mu = get(paste0("mu",i)),
                                  sigma = get(paste0("sd",i))))
}

old_ranks = match(1:8,ranking)
data.frame(marg,prob,sd,meanCRPS,old_ranks)
new_ranking = order(meanCRPS)
data.frame(marg,prob,sd,meanCRPS,old_ranks)[new_ranking,]

#####################################################################
# Toy example illustrating the use of the optim function
# The function optim optimizes a given function based on a set of 
# initial parameters par over some data. 

# Here, the task is to maximize the volume of a cuboid with fixed
# total length of edges equal to 12. Clearly the volume is maximal
# when all edges have equal length, leaving a volume of 1.
# We start with side a = 0.05 of length 0.05, and similarly
# b = 0.05, leaving side c of length (12-4*a-4*b)/4. 
# Then the function to maximize is a*b*c and the parameters we 
# need are a and b.

pars <- c(a = 0.05, b = 0.05)
get.vol <- function(pars){
  c    <- (12-4*pars[1]-4*pars[2])/4
  vol  <- pars[1]*pars[2]*c
  return(vol)
}

# Inital value
get.vol(pars = pars)

# Optimization

# fnscale = -1 is necessary to maximize the volume instead
# of minimizing it
# trace shows the single steps
# If additional data (e.g. training data) are necessary for
# the evaluation of the function, these can be added as 
# additional entry
# e.g. if get.vol <- function(pars, data){...} then one
# would add the line "data = data," to the optimization

est <- optim(
  par = pars,
  fn  = get.vol,
  control = list(fnscale = -1, trace = TRUE)
)

# est contains now the new estimated parameters
# and shows you additional information
print(est)

# Estimated volume is one and parameters very close to one
get.vol(pars = est$par)

#####################################################################
# Ensemble generation and postprocessing 


