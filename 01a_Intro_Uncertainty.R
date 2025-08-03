######## Risk and uncertainty in the economic evaluation of mining projects ####################################

############################# Introduction to R ################################################################


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
matrix(c(1,2,3,4,5,6),2,3,byrow=T) # matrix, no. of rows = 2
list() # list
data.frame() # data.frame = table similar to excel spreadsheet with headers
1:3 # sequence from 1 to 3
seq(1,3,by=1) # more general sequence generator


## Assignment operation
x <- 1:3 # creates an object "x" in the work space, and defines it as a vector c(1,2,3)
data <- read.csv("Rb_Sr.csv", sep=";", header = TRUE) # creates object "data"


## Probability functions
# d - density, p - cumulative probability, q - quantile, r - random number generation
#normal distribution:
dnorm(1, mean = 1, sd = 1) 
pnorm(1, mean = 1, sd = 1)
qnorm(1, mean = 1, sd = 1)
rnorm(100, mean = 1, sd = 1) 
x <- rnorm(100, mean = 1, sd = 1) # overwrite x with 100 random numbers from normal dist.
hist(x, col = "grey") # plot histogram of x


## Component extraction
data[1,] # 1st row of object "data"
data[,1] # 1st column of object "data"
data[1,1] # entry in row r and column c of object "data"
length(data) # length of "data"
data[data<20] # extract all elements of "data" that are smaller than 20
data["Rb"] # extract column “Rb” from object "data"
data$Rb # extract column "Rb" from data.frame "data"; or object "age" from a list, if "data" is a list


## Plots 
plot() # both 1D and 2D plots
hist() # histograms
qqnorm() # normal qq plot

# -> Plots can be saved through "Export" icon on plot window to the right!
# -> Try it out!


## Basic statistics
mean()
summary()
sd()
t.test()
boxplot()


## Regression analysis
# Always through lm() function ("linear model"); requires data in vectors or as data.frame
LM <- lm()
summary(LM)


#################### Exercise - Error propagation for isochron dating ##############################

# In this exercise, we want to run a Monte-Carlo simulation to propagate
# errors on individual isotope ratio measurements to the final estimated 
# isochron age. 

### Step 1: Fit an isochron to the Rb-Sr data in object "data" using lm(), 
# and save the results in an object called "Age"

Age <- lm(X87Sr_86Sr ~ X87Rb_86Sr, data=data)



### Step 2: Explore object "Age". What type of object is it? What does it contain?

typeof() # tells you the type
names() # provides you with a list of the contents of an object
summary() # shows you a summary of the fitting results
plot() # allows you to plot some diagnostic plots for the model



### Step 3: What is the age error of the best fit?

# Check what you can extract with summary() for the slope of the fitted line
  # and error of the slope. Use this to calculate the minimum and maximum age
  # using the formula given in the lecture notes. You can get the uncertainty
  # from the difference between min and max ages.



### Step 4: Create a list of length 1000, called "Age_Simulation"

Age_Simulation <- list()

length(Age_Simulation) <- 1000



### Step 5: Fill list with simulated datasets

for (i in 1:1000) {
  
    Age_Simulation[[i]] <- data
    
    Age_Simulation[[i]]$X87Sr_86Sr <- rnorm(n=6,mean=data$X87Sr_86Sr,sd=data$X87Sr_86Sr_2SE/2)

    Age_Simulation[[i]]$X87Rb_86Sr <- rnorm(n=6,mean=data$X87Rb_86Sr,sd=data$X87Rb_86Sr_2SE/2)
      
}

# This is called a "for" loop. It repeats the same operation 1000 times, changing index i from
# 1 through 1000, thus successively filling in the respective list entries.



### Step 6: Fit linear models to each simulated dataset,
# and save these in the list

# Use a "for" loop to replace each list entry Age_Simulation[[i]] with a linear model of 
# the data currently saved in this list entry.



## Step 7: Make data.frame to collect and extract 
# results from simulations

Results_age <- data.frame(age=1:1000, min_age=1:1000, max_age=1:1000)



## Step 8: Calculate best estimates of age and errors
# for overall data

# Use a for-loop again to extract the relevant information (best-fit age,
# min age, max age) from the simulation list and enter these into the 
# correct place in the data.frame Results_age. 

for (i in 1:1000) {
  
  Results_age$age[i] <- 
  
  Results_age$min_age[i] <- 
  
  Results_age$max_age[i] <- 
  
}

# Then estimate the errors again, using the same method as above (i.e., 
# comparing maximum and minimum estimates)

Results_age$age_2SE <- 

  

### Step 9: Evaluate the results
  
# Plot a histrogram for the best-fit estimates:


  
# Look at a summary of the best-fit estimates:
  

  
# What is the error? (use 2.5th and 97.5th quantiles to compute)
  


# Look at a summary of the errors:
  


# What is the best estimate of overall uncertainty?


