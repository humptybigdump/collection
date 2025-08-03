# -*- coding: utf-8 -*-
"""
Created on Wed Jul  8 09:55:03 2020

"""

from scipy import stats
import matplotlib.pyplot as plt




###Oberservation window
x_min = 0 
x_max = 1

y_min = 0 
y_max = 1

area = (x_max - x_min) * (y_max - y_min)

###Simulation of the homogeneous Poisson process underlying the Boolean model
#Intensity
gamma = 65
#The number of points follows a Poisson distribution
number_points_po = stats.poisson.rvs(gamma * area)
print("\n" + "The number of points of the Poisson process is: " + str(number_points_po) + "\n")
#In each coordinate, distribute the points uniformly
x_coord_po = stats.uniform.rvs(loc = x_min, scale = x_max - x_min, size = number_points_po)
y_coord_po = stats.uniform.rvs(loc = y_min, scale = y_max - y_min, size = number_points_po)


###Plot the Boolean model
fig, ax = plt.subplots(1, 1, figsize = (9.25, 9.25))
#Around each point of the Poisson process draw a disk of random radius
for i in range(number_points_po):
    random_radius = stats.expon.rvs(scale = 0.03)
    
    circle = plt.Circle((x_coord_po[i], y_coord_po[i]), random_radius, color = 'k')
    ax.set_aspect(1)
    ax.add_artist(circle)

plt.axis('off')
plt.show()    

#Uncomment the following command if you wish to save the resulting picture
#fig.savefig("Sheet13Prob54.png", bbox_inches = 'tight')

