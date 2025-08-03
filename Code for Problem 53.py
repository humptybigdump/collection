# -*- coding: utf-8 -*-
"""
Created on Wed Jul  8 09:55:03 2020

"""

from scipy import stats
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
import numpy as np




######Part a)#####

###Oberservation window
x_min = 0 
x_max = 1

y_min = 0 
y_max = 1

area = (x_max - x_min) * (y_max - y_min)

###Simulation of the homogeneous Poisson process underlying the Boolean model
#Intensity
gamma = 50
#The number of points follows a Poisson distribution
number_points_po = stats.poisson.rvs(gamma * area)
print("\n" + "The number of points of the Poisson process is: " + str(number_points_po) + "\n")
#In each coordinate, distribute the points uniformly
x_coord_po = stats.uniform.rvs(loc = x_min, scale = x_max - x_min, size = number_points_po)
y_coord_po = stats.uniform.rvs(loc = y_min, scale = y_max - y_min, size = number_points_po)


###Plot the Boolean model
fig, ax = plt.subplots(1, 1, figsize = (16, 8))
#Around each point of the Poisson process draw a disk of radius 0.05
for i in range(number_points_po):
    circle = plt.Circle((x_coord_po[i], y_coord_po[i]), 0.05, color = 'r')
    ax.set_aspect(1)
    ax.add_artist(circle)
    
plt.show()    

#Uncomment the following command if you wish to save the resulting picture
#fig.savefig("Sheet13Prob53a.png", bbox_inches = 'tight')




#####Part b)#####

#Use the code from Part a) above and increase the intensity parameter 'gamma' until the desired effects occur




######Part c)#####

###Oberservation window
x_min = 0 
x_max = 10

y_min = 0 
y_max = 5

area = (x_max - x_min) * (y_max - y_min)

###Simulation of the homogeneous Poisson process underlying the Boolean model
#Intensity
gamma = 3
#The number of points follows a Poisson distribution
number_points_po = stats.poisson.rvs(gamma * area)
print("\n" + "The number of points of the Poisson process is: " + str(number_points_po) + "\n")
#In each coordinate, distribute the points uniformly
x_coord_po = stats.uniform.rvs(loc = x_min, scale = x_max - x_min, size = number_points_po)
y_coord_po = stats.uniform.rvs(loc = y_min, scale = y_max - y_min, size = number_points_po)


###Plot the Boolean model
fig, ax = plt.subplots(1, 1, figsize = (16, 8))
plt.xlim(x_min, x_max)
plt.ylim(y_min, y_max)
#For each point in the Poisson process draw a square with random side length which is centered at the given point
for i in range(number_points_po):
    random_sidelength = stats.uniform.rvs(loc = 0, scale = 0.5)
    #Specify a rectangle by its bottom left coordinate and side lengths
    square = plt.Rectangle((x_coord_po[i] - random_sidelength / 2, y_coord_po[i] - random_sidelength / 2), random_sidelength, random_sidelength, linewidth = 1, edgecolor = 'r', facecolor = 'r')

    ax.set_aspect(1)
    ax.add_artist(square)
    
plt.show()    

#Uncomment the following command if you wish to save the resulting picture
#fig.savefig("Sheet13Prob53c.png", bbox_inches = 'tight')




######Part d)#####

###Oberservation window
x_min = 0
x_max = 5

y_min = 0
y_max = 5

z_min = 0
z_max = 5

area = (x_max - x_min) * (y_max - y_min) * (z_max - z_min)

###Simulation of the homogeneous Poisson process underlying the Boolean model
#The number of points follows a Poisson distribution
gamma = 0.25
number_points_po = stats.poisson.rvs(gamma * area)
print("\n" + "The number of points of the Poisson process is: " + str(number_points_po) + "\n")
#In each coordinate, distribute the points uniformly
x_coord = stats.uniform.rvs(loc = x_min, scale = x_max - x_min, size = number_points_po)
y_coord = stats.uniform.rvs(loc = y_min, scale = y_max - y_min, size = number_points_po)
z_coord = stats.uniform.rvs(loc = z_min, scale = z_max - z_min, size = number_points_po)


###Plot the Boolean model
fig = plt.figure(1, figsize = (13, 13))    
ax = fig.add_subplot(111, projection = '3d')
plt.xlim(x_min, x_max)
plt.ylim(y_min, y_max)

#For each point of the Poisson process generate an exponential random variable (note: rate = 1 / scale) 
#and use that value as the radius of the ball that is plotted around the given point
for i in range(number_points_po):
    random_radius = stats.expon.rvs(scale = 0.3)
    #Plot spheres into the scatterplots (using spherical coordinates to parametrize them)
    theta = np.linspace(0, 2 * np.pi, 100)
    phi = np.linspace(0, np.pi, 100)
    
    x = random_radius * np.outer(np.cos(theta), np.sin(phi)) + x_coord[i] #Note: np.outer takes two vectors and provides a 2d array which contains all products of entries from the first vector with entries from the second vector
    y = random_radius * np.outer(np.sin(theta), np.sin(phi)) + y_coord[i]
    z = random_radius * np.outer(np.ones(np.size(phi)), np.cos(phi)) + z_coord[i] #Note: np.ones gives a vector with '1' in every entry

    ax.plot_surface(x, y, z, color = 'r')





#################################################################################
### Notice: Depending on the resolution of your screen you may have to change ###
### the parameter 'figsize' in lines 38, 83, and 129 to get a decent plot!    ###
#################################################################################