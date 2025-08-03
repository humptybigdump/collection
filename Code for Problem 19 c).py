# -*- coding: utf-8 -*-
"""
Created on Tue Nov  5 13:46:46 2019

"""
#################################################################
# run   %matplotlib   in the console to get an interactive plot #
#################################################################

from mpl_toolkits.mplot3d import Axes3D
from scipy import stats
import matplotlib.pyplot as plt
 

###Oberservation window
x_min = 0 
x_max = 1

y_min = 0 
y_max = 1

z_min = 0
z_max = 1


###Simulation of the Poisson process

#The number of points follows a Poisson distribution
number_points = stats.poisson.rvs(100)
print("\n" + "The number of points is: " + str(number_points) + "\n")

#In each coordinate, the points follow some beta distribution
x_coord = stats.beta.rvs(a = 0.5, b = 0.5, size = number_points)
y_coord = stats.beta.rvs(a = 1, b = 1.5, size = number_points)
z_coord = stats.beta.rvs(a = 2, b = 1, size = number_points)


###Plot the points
fig = plt.figure(figsize = (12, 12))
ax = fig.add_subplot(111, projection='3d')

ax.scatter(x_coord, y_coord, z_coord, edgecolor = 'b', facecolor = 'b', alpha = 0.7, s = 35)
ax.set_title("Poisson process \n", fontdict = {'fontsize': 35, 'fontweight': 'medium'})

#Uncomment the following command if you wish to save the resulting picture
#fig.savefig("problem1c.png", bbox_inches='tight')
