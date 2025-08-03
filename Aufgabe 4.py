# -*- coding: utf-8 -*-
"""
Created on Tue Nov  5 13:32:21 2019

"""


from scipy import stats
import matplotlib.pyplot as plt
import numpy as np


###################################
###Define the window [-10, 10]^2###
###################################
x_min = -10
x_max = 10

y_min = -10
y_max = 10

area = (x_max - x_min) * (y_max - y_min)


##########################################################
###Simulation of the location (or parent) process Phi_0###
##########################################################
#Intensity parameter
theta_0 = 0.02
#The number of points follows a Poisson distribution
number_points = stats.poisson.rvs(theta_0 * area)
print("\n" + "The number of clusters is: " + str(number_points) + "\n")
#In each coordinate, distribute the points uniformly
x_coord = stats.uniform.rvs(loc = x_min, scale = x_max - x_min, size = number_points)
y_coord = stats.uniform.rvs(loc = y_min, scale = y_max - y_min, size = number_points)


######################################################
###Simulation of the secondary processes (children)###
######################################################
#Create a list which carries the number of points each cluster has (this number of points is given by a Poisson distribution)
number_points_clusters = stats.poisson.rvs(size = number_points, mu = 4*np.pi)
#For each point of the location process generate a vectors which will carry the coordinates of the corresponding cluster points
x_coord_clusters = [[]] * number_points
y_coord_clusters = [[]] * number_points

for i in range(number_points):
    #To simulate the bivariate standard normal distribution, we simulate a univarite standard Gaussian variable for each component
    x_coord_clusters[i] = stats.norm.rvs(loc = 0, scale = 1, size = number_points_clusters[i])
    y_coord_clusters[i] = stats.norm.rvs(loc = 0, scale = 1, size = number_points_clusters[i])
    #To obtain the points as they appear in the Neyman-Scott process, each cluster has to be shifted into the corresponding
    #point of the location process
    for k in range(number_points_clusters[i]):
        x_coord_clusters[i][k] += x_coord[i]
        y_coord_clusters[i][k] += y_coord[i]

#Print the total number of points in the Neyman-Scott process
total_number = sum(number_points_clusters[i] for i in range(number_points))
print("\n" + "The total number of points in the cluster process is: " + str(total_number))


##############################
###Plot the cluster process###
##############################
plt.figure(figsize = (12, 12))
#Visualize the centers of the clusters
plt.scatter(x_coord, y_coord, edgecolor = 'r', facecolor = 'r', alpha = 0.3, s = 100)
for i in range(number_points):
    plt.scatter(x_coord_clusters[i], y_coord_clusters[i], edgecolor = 'b', facecolor = 'b', alpha = 0.8, s = 15)
plt.show()    
#Use the following prompt if you wish to save the image (in the same folder as the Python script)    
#Note: Before you try to safe the figure, disable the "plt.show()" prompt!
#plt.savefig('Neyman-Scott1.png')   
    




