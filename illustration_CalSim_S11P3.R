# Set your working directory using
# setwd("C:/.../.../...")

# or make sure to put the files containing the data in the default directory
# getwd()

# Install the CalSim-package using
# install.packages("CalSim")

# Load the package
library(CalSim)

# The package contains exemplary data
# see also documentation of sample data
# ?ternary_forecast_example
data = ternary_forecast_example
head(data)
# data$p1 contains forecast probabilities for outcome 1
# data$p3 contains forecast probabilities for outcome 3
# data$obs0, data$obs1, data$obs2,data$obs3 contain
# outcomes encode as 1,2, and 3 illustrating distinctive forecast behavior

# Example 1: Calibrated forecast
# Generate an object of class calibration_simplex using
calsim0 = calibration_simplex(p1 = data$p1, p3 = data$p3, obs = data$obs0)
# Only the forecast probabilities for two outcomes and the respective observations have to be specified
# the input parameter p1 denotes the probabilities for outcome 1
# the input parameter p2 denotes the probabilities for outcome 2
# the input parameter p3 denotes the probabilities for outcome 3

# Plot the calibration simplex using
plot(calsim0,use_pvals = TRUE) # with color-coded multinomial p-values

#The bottom-left vertex corresponds to outcome 1
#The vertex to the right corresponds to outcome 2
#The top-left vertex corresponds to outcome 3

#This forecast is well-calibrated.

# Example 2: Overconfident forecast
calsim1 = calibration_simplex(p1 = data$p1, p3 = data$p3, obs = data$obs1)
plot(calsim1,use_pvals = TRUE)

# This forecast is overconfident, i.e., the forecast probabilities are too extreme,
# meaning that they deviate to much from the center (1/3,1/3,1/3). Therefore,
# the dots are shifted towards the center.

# Example 3: Underconfident forecast
calsim2 = calibration_simplex(p1 = data$p1, p3 = data$p3, obs = data$obs2)
plot(calsim2,use_pvals = TRUE) # with multinomial p-values

# This forecast is underconfident, i.e., the forecast probabilities are not extreme enough,
# meaning that they deviate to little from the center (1/3,1/3,1/3). Therefore,
# the dots are shifted away from the center.

# Example 4: Unconditionally biased forecast
calsim3 = calibration_simplex(p1 = data$p1, p3 = data$p3, obs = data$obs3)
plot(calsim3,use_pvals = TRUE)

# This forecast is unconditionally biased in that the probabilities assigned to outcome 3 are to large,
# therefore the circles move away from the top-left corner.
