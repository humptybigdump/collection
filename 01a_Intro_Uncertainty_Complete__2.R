########## Risk and uncertainty in the economic evaluation of mining projects ##############

############################### Introduction to R ##########################################


## Getting help 
help(pnorm) # <- put function name in brackets instead of "pnorm"
help.search("normal distribution") # <- if you don't know the name of the function


## Working directories
getwd() # <- Shows current working directory
setwd() # <- Allows you to set working directory
attach() # <- Attaches database to R search path 


## Importing data
read.csv("Rb_Sr.csv", sep=";", header = TRUE)


## Creating data
c(1,2,3) # vector
matrix(c(1,2,3,4,5,6),2,3,byrow=F) # matrix, no. of rows = 2
list() # list
data.frame(x=c(1,2,3)) # data.frame = table similar to excel spreadsheet with headers
1:3 # sequence from 1 to 3
seq(1,3,by=1) # more general sequence generator


## Assignment operation
x <- seq(1,3,by=0.1) # creates an object "x" in the work space, and defines it as a vector c(1,2,3)
data <- read.csv("Rb_Sr.csv", sep=";", header = TRUE) # creates object "data"

typeof(x)
class(x)

## Probability functions
# d - density, p - cumulative probability, q - quantile, r - random number generation
#normal distribution:
dnorm(1, mean = 1, sd = 1) 
pnorm(1, mean = 1, sd = 1)
qnorm(1, mean = 1, sd = 1)
rnorm(100, mean = 1, sd = 1) 
x -> rnorm(100, mean = 1, sd = 1) # overwrite x with 100 random numbers from normal dist.
hist(x, col = "grey") # plot histogram of x


## Component extraction
data[1,] # 1st row of object "data"
data[,2] # 1st column of object "data"
data[1,1] # entry in row r and column c of object "data"
length(x) # length of "data"
data[data<20] # extract all elements of "data" that are smaller than 20
data[c("Rb","Sr")] # extract column “Rb” from object "data"
data$Rb # extract column "Rb" from data.frame "data"; or object "age" from a list, if "data" is a list

## Plots 
plot(data$Rb,data$Sr) # both 1D and 2D plots
hist(x) # histograms
qqnorm(x) # normal qq plot
boxplot(data)

# -> Plots can be saved through "Export" icon on plot window to the right!


## Basic statistics
mean(x)
summary(x)
sd(x)
t.test(x,y)


y <- rnorm(100,mean=-1,sd=1)

## Regression analysis
# Always through lm() function ("linear model"); requires data in vectors or as data.frame
LM <- lm(y~x)
summary(LM)

plot(y~x)
abline(LM)

#################### Exercise - Error propagation for isochron dating ##############################

# In this exercise, we want to run a Monte-Carlo simulation to propagate
# errors on individual isotope ratio measurements to the final estimated 
# isochron age. 

### Step 1: Fit an isochron to the Rb-Sr data in object "data" using lm(), 
# and save the results in an object called "Age"

Age <- lm(X87Sr_86Sr ~ X87Rb_86Sr, data=data)

summary(Age)

plot(X87Sr_86Sr ~ X87Rb_86Sr, data=data)
abline(7.11e-1, 5.32e-4)

data

### Step 2: Explore object "Age". What type of object is it? What does it contain?

typeof() # tells you the type
class() # tells you the class of an object (numeric, categorical)
names() # provides you with a list of the contents of an object
summary() # shows you a summary of the fitting results
plot() # allows you to plot some diagnostic plots for the model


### Step 3: What is the age error of the best fit?

# Check what you can extract with summary() for the slope of the fitted line
  # and error of the slope. Use this to calculate the minimum and maximum age
  # using the formula given in the lecture notes. You can get the uncertainty
  # from the difference between min and max.

summary(Age)

summary(Age)$coefficients[2,2]

# Median age

log(summary(Age)$coefficients[2,1]+1)*48.8e9/log(2) # -> 37.4 Ma

# Max age

log(summary(Age)$coefficients[2,1]+summary(Age)$coefficients[2,2]*2+1)*48.8e9/log(2) # -> 38.2 Ma

# Min age

log(summary(Age)$coefficients[2,1]-summary(Age)$coefficients[2,2]*2+1)*48.8e9/log(2) # -> 36.7 Ma

# Uncertainty: around +/- 0.8 Ma --> a little less than paper

### Step 4: Create a list of length 1000, called "Age_Simulation"

Age_Simulation <- list()

length(Age_Simulation) <- 1000


### Step 5: Fill list with simulated datasets

for (i in 1:1000) {
  
    Age_Simulation[[i]] <- data
    
    Age_Simulation[[i]]$X87Sr_86Sr <- rnorm(n=6,mean=data$X87Sr_86Sr,sd=data$X87Sr_86Sr_2SE/2)

    Age_Simulation[[i]]$X87Rb_86Sr <- rnorm(n=6,mean=data$X87Rb_86Sr,sd=data$X87Rb_86Sr_2SE/2)
      
}

Age_Simulation[[5]]

# This is called a "for" loop. It repeats the same operation 1000 times, changing index i from
# 1 through 1000, and thus successively fills in the respective list entries.



### Step 6: Fit linear models to each simulated dataset,
# and save these in the list

# Use a "for" loop to replace each list entry Age_Simulation[[i]] with a linear model of the data
# which is currently saved in this list entry.



for (i in 1:1000) {
  
  Age_Simulation[[i]] <- lm(X87Sr_86Sr ~ X87Rb_86Sr, data=Age_Simulation[[i]])  
  
}

summary(Age_Simulation[[20]])

log(summary(Age_Simulation[[24]])$coefficients[2,1]+1)*48.8e9/log(2) # -> Median ages highly variable! 

## Step 7: Make data.frame to collect and extract 
# results from simulations

Results_age <- data.frame(age=1:1000, min_age=1:1000, max_age=1:1000)

## Step 8: Calculate best estimates of age and errors
# for overall data

# Use a for-loop again to extract the relevant information (best-fit age,
# min age, max age) from the simulation list and enter these into the 
# correct place in the data.frame Results_age. 


for (i in 1:1000) {
  
  Results_age$age[i] <- log(summary(Age_Simulation[[i]])$coefficients[2,1]+1)*48.8e9/log(2)
  
  Results_age$min_age[i] <- log(summary(Age_Simulation[[i]])$coefficients[2,1]-
                                  summary(Age_Simulation[[i]])$coefficients[2,2]*2+1)*48.8e9/log(2)
  
  Results_age$max_age[i] <- log(summary(Age_Simulation[[i]])$coefficients[2,1]+
                                  summary(Age_Simulation[[i]])$coefficients[2,2]*2+1)*48.8e9/log(2)
  
}


# Then estimate the errors again, using the same method as above (i.e., 
# comparing maximum and minimum estimates)


Results_age$age_2SE <- (Results_age$max_age - Results_age$min_age)/2 


### Step 9: Evaluate the results
  
# Plot a histrogram for the best-fit estimates:


hist(Results_age$age)

hist(Results_age$age_2SE)

# Look at a summary of the best-fit estimates:


summary(Results_age$age)

summary(Results_age$age_2SE)

# What is the error? (use 2.5th and 97.5th quantiles to compute)

quantile(Results_age$age, probs=c(0.025,0.5, 0.975))

(40-34.8)/2

# What is the best estimate of overall uncertainty?

# The compound error of two independent errors a and b is √(a^2 + b^2)

sqrt(2.6^2 + 2.5^2)

# This could be considered a "safe" estimate of the overall uncertainty on the estimated age.