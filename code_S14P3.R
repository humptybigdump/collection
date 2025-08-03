library(scoringRules)
# Loading Data
load("HDwind.Rdata")

# Get to know the data
head(dates)
str(dates)
dates[1]
dates[1] + 10
dates[1] + 100
range(dates)

head(obs)
str(obs)
range(obs)

# head(ensfc)
str(ensfc)
dim(ensfc)
range(ensfc)

# Split data
# Training data (2015)
training_ind <- which(dates <= "2015-12-31")
training_ensfc <- ensfc[training_ind, ]
training_obs <- obs[training_ind]

#Test data (2016)
eval_ind <- which(dates >= "2016-01-01")
eval_ensfc <- ensfc[eval_ind, ]
eval_obs <- obs[eval_ind]

# a)

# Rank histogram
rhist <- function(ensfc, obs, ...) {
  ranks <- apply(ensfc < obs, 1, sum) + 1
  counts <- tabulate(ranks)
  barplot(counts, names.arg = seq_along(counts), ...)
}

rhist(training_ensfc, training_obs)

# b)

# Training EMOS
# Target function that we want to minimize
target_fun <- function(par, ens_mean, obs){
  m <- cbind(1, ens_mean) %*% par[1:2] # mu = a + b ensmean
  s <- sqrt(par[3]) # sigma^2 = c
  score <- sum(crps_norm(y = obs, mean = m, sd = s))
  return(score)
}

# Optimization
optim_out <- optim(par = c(1,1,1), # starting values
                   fn = target_fun, # objective fct
                   ens_mean = apply(training_ensfc, 1, mean),
                   obs = training_obs)

optim_out
opt_par <- optim_out$par

# Apply model to evaluation data
eval_ensmean <- apply(ensfc[eval_ind,], 1, mean)

eval_mu <- as.vector(cbind(1, eval_ensmean) %*%  opt_par[1:2])
eval_sigma <- sqrt(opt_par[3])

# Evaluation
# Rank (ensemble) and PIT (EMOS) histograms
par(mfrow = c(1, 2))
rhist(eval_ensfc, eval_obs, ylim = c(0, 100))

PIT <- pnorm(eval_obs, eval_mu, eval_sigma)
hist(PIT,breaks = 50)

# CRPS
scores <- cbind(ens = crps_sample(eval_obs, dat = eval_ensfc),
                norm = crps_norm(eval_obs, mean = eval_mu, sd = eval_sigma))
colMeans(scores)

# c)

#The normal distribution assigns some positive probability to negative wind speeds, e.g.
mu = 1
sd = 1
pnorm(0,mu,sd)

#This can be remedied by using a truncated normal distribution, 
#which only allows for non-negative outcomes:

# Training EMOS using truncated normal distribution
# Target function that we want to minimize
target_fun_tnorm <- function(par, ens_mean, obs){
  m <- cbind(1, ens_mean) %*% par[1:2] # mu = a + b ensmean
  s <- sqrt(par[3]) # sigma^2 = c
  score <- sum(crps_tnorm(y = obs, location = m, scale = s,lower = 0))
  return(score)
}

# Optimization
optim_out_tnorm <- optim(par = c(1,1,1), # starting values
                         fn = target_fun_tnorm, # objective fct
                         ens_mean = apply(training_ensfc, 1, mean),
                         obs = training_obs)

optim_out_tnorm
opt_par_tnorm <- optim_out_tnorm$par

# Apply model to evaluation data
eval_mu_tnorm <- as.vector(cbind(1, eval_ensmean) %*%  opt_par_tnorm[1:2])
eval_sigma_tnorm <- sqrt(opt_par_tnorm[3])

# Evaluation
# CRPS
scores <- cbind(ens = crps_sample(eval_obs, dat = eval_ensfc),
                norm = crps_norm(eval_obs, mean = eval_mu, sd = eval_sigma),
                tnorm = crps_tnorm(eval_obs,eval_mu_tnorm,eval_sigma_tnorm,0))
colMeans(scores)
