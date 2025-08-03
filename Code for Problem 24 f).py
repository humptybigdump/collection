# -*- coding: utf-8 -*-
"""
Created on Wed Nov 27 07:27:01 2019
"""



from scipy import stats
import matplotlib.pyplot as plt
import numpy as np
 

###Definition of the domain in which we simulate the process
x_min = -25
x_max = 25

y_min = -25
y_max = 25

z_min = -25
z_max = 25

area = (x_max - x_min) * (y_max - y_min) * (z_max - z_min)

###Simulation-specific variables
#Intensity of the Poisson process
intensity = 0.005
#Number of runs for the visual verification of the normal approximation
runs = 1000
#Two values for the radius of the ball-shaped observation window
rads = [5, 25]

###Perform the main simulation
#Collect the values for the realizations of the standardized estimators in lists
estimator_realizations = [[], []]
 
for i in range(runs):
    ###Simulation of the Poisson process
    #The number of points follows a Poisson distribution
    number_points = stats.poisson.rvs(mu = intensity * area, size = 1)
    #In each coordinate, distribute the points uniformly
    x_coord = stats.uniform.rvs(loc = x_min, scale = x_max - x_min, size = number_points)
    y_coord = stats.uniform.rvs(loc = y_min, scale = y_max - y_min, size = number_points)
    z_coord = stats.uniform.rvs(loc = z_min, scale = z_max - z_min, size = number_points)
    
    ###Parameter estimation
    #Calculate the standardized estimator in the observation window B(0, n) for n in rads
    for n in rads:
        #Calculate Phi(B(0, n)), that is, the number of points in the observation window
        observed_points = 0
        for k in range(int(number_points)):
            #Check for each point in the process if it falls into the ball
            if np.sqrt(x_coord[k]**2 + y_coord[k]**2 + z_coord[k]**2) <= n:
                observed_points += 1
        #Calculate the standardized estimator
        volume_ball = 4 * np.pi * n**3 / 3
        gamma = observed_points / volume_ball
        gamma_standard = np.sqrt(volume_ball) * (gamma - intensity) / np.sqrt(intensity)
        #Append the value to the lists
        estimator_realizations[rads.index(n)].append(gamma_standard)

###Plot a histogram (of relative frequencies) with the resulting realization for each n in rads and compare with the standard Gaussian density
#Initiate the plot
fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(10, 5))
#fig.suptitle("Based on " + str(runs) + " realizations")

#Plot the histograms
counts, bins = np.histogram(estimator_realizations[0], bins = 30, density = True)
ax1.hist(bins[:-1], bins, weights = counts)
ax1.set_title("Observation window: B(0, " + str(rads[0]) + ")", fontdict = {'fontsize': 10, 'fontweight': 'medium'})

counts, bins = np.histogram(estimator_realizations[1], bins = 30, density = True)
ax2.hist(bins[:-1], bins, weights = counts)
ax2.set_title("Observation window: B(0, " + str(rads[1]) + ")", fontdict = {'fontsize': 10, 'fontweight': 'medium'})

#Add the Gaussian density function
abs_max_value_estimator_1 = max(abs(max(estimator_realizations[0])), abs(min(estimator_realizations[0])))
x_axis_1 = np.arange(-abs_max_value_estimator_1, abs_max_value_estimator_1, 0.001)
ax1.plot(x_axis_1, stats.norm.pdf(x_axis_1, 0, 1), color = "r", linewidth = 1)

abs_max_value_estimator_2 = max(abs(max(estimator_realizations[1])), abs(min(estimator_realizations[1])))
x_axis_2 = np.arange(-abs_max_value_estimator_2, abs_max_value_estimator_2, 0.001)
ax2.plot(x_axis_2, stats.norm.pdf(x_axis_2, 0, 1), color = "r", linewidth = 1)


#Uncomment the following command if you wish to save the resulting picture
#fig.savefig("Picture24f.png", bbox_inches='tight')
    
    
    
    
    
    
    
