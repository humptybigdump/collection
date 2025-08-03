# -*- coding: utf-8 -*-
"""
Created on Wed Nov 27 07:27:01 2019
"""




#################################################################
# run   %matplotlib   in the console to get an interactive plot #
#################################################################

from mpl_toolkits.mplot3d import Axes3D
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



###Simulation of the Poisson process
#The number of points follows a Poisson distribution
intensity = 0.005
number_points = stats.poisson.rvs(mu = intensity * area, size = 1)
print("\n" + "The number of points is: " + str(int(number_points)) + "\n")

#In each coordinate, distribute the points uniformly
x_coord = stats.uniform.rvs(loc = x_min, scale = x_max - x_min, size = number_points)
y_coord = stats.uniform.rvs(loc = y_min, scale = y_max - y_min, size = number_points)
z_coord = stats.uniform.rvs(loc = z_min, scale = z_max - z_min, size = number_points)



###Plot the points and visualize the balls with radii 5, 10, and 20
#Creating the figure
fig = plt.figure(figsize = (21, 7))
#Creating three subplots
axes = [fig.add_subplot(131, projection='3d'), fig.add_subplot(132, projection='3d'), fig.add_subplot(133, projection='3d')]

#Plotting the scattered points three times. Adding to each of the plots one of the balls
radii = [5, 10, 20]
for i in range(3):
    #Scatter plots
    title = "Observation window: B(0, " + str(radii[i]) + ")"
    axes[i].scatter(x_coord, y_coord, z_coord, edgecolor = 'b', facecolor = 'b', alpha = 0.7, s = 3) #Note: the 's' parameter corresponds to the size of the points
    axes[i].set_title(title + "\n", fontdict = {'fontsize': 15, 'fontweight': 'medium'})

    #Plot spheres into the scatterplots (using spherical coordinates to parametrize them)
    theta = np.linspace(0, 2 * np.pi, 100)
    phi = np.linspace(0, np.pi, 100)
    
    x = radii[i] * np.outer(np.cos(theta), np.sin(phi)) #Note: np.outer takes two vectors and provides a 2d array which contains all products of entries from the first vector with entries from the second vector
    y = radii[i] * np.outer(np.sin(theta), np.sin(phi))
    z = radii[i] * np.outer(np.ones(np.size(phi)), np.cos(phi)) #Note: np.ones gives a vector with '1' in every entry

    axes[i].plot_surface(x, y, z, color = 'r', alpha = 0.3) #Note: the 'alpha' parameter is used to make the sphere transparent



###Parameter estimation
#Calculate the estimator in each observation window B(0, n), n = 1, 3, 5, 10, 15, 20, 25
for n in [1, 3, 5, 10, 15, 20, 25]:
    #Calculate Phi(B(0, n)), that is, the number of points in the observation window
    observed_points = 0
    for k in range(int(number_points)):
        #Check for each point in the process if it falls into the ball
        if np.sqrt(x_coord[k]**2 + y_coord[k]**2 + z_coord[k]**2) <= n:
            observed_points += 1
    print("Number of points in " + " B(0, " + str(n) + "):    " + str(observed_points) + "\n")
    #Calculate the estimator
    volume_ball = 4 * np.pi * n**3 / 3
    gamma = observed_points / volume_ball
    print("Value of the estimator:    " + str(gamma) + "\n\n")
    
    
    
################################################################################################
### Notice: Depending on the resolution of your screen you may have to change the parameters ###
### 'figsize' in line 48, 's' in line 57, and 'fontsize' in line 58 to get a decent plot!    ###
################################################################################################
    
    
    
    
    
    
    
    
    
    
