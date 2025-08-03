# -*- coding: utf-8 -*-
"""
Created on Tue Nov  5 13:32:21 2019

"""


from scipy import stats
import matplotlib.pyplot as plt
 
###Oberservation window
x_min = 1
x_max = 3

y_min = 1 
y_max = 2.2

area = (x_max - x_min) * (y_max - y_min)


###Simulation of the binomial process
#The number of points is fixed to m = 50
number_points_bin = 50
print("\n" + "The number of points of the binomial process is: " + str(number_points_bin))
#In each coordinate, distribute the points uniformly
x_coord_bin = stats.uniform.rvs(loc = x_min, scale = x_max - x_min, size = number_points_bin)
y_coord_bin = stats.uniform.rvs(loc = y_min, scale = y_max - y_min, size = number_points_bin)


###Simulation of the homogeneous Poisson process
#Intensity
gamma = 30
#The number of points follows a Poisson distribution
number_points_po = stats.poisson.rvs(gamma * area)
print("\n" + "The number of points of the Poisson process is: " + str(number_points_po) + "\n")
#In each coordinate, distribute the points uniformly
x_coord_po = stats.uniform.rvs(loc = x_min, scale = x_max - x_min, size = number_points_po)
y_coord_po = stats.uniform.rvs(loc = y_min, scale = y_max - y_min, size = number_points_po)


###Plot the points
fig, axs = plt.subplots(1, 2, figsize = (16, 8))

axs[0].scatter(x_coord_bin, y_coord_bin, edgecolor = 'b', facecolor = 'b', alpha = 0.7, s = 30)
axs[0].set_xlabel("x")
axs[0].set_title("Binomial process", fontdict = {'fontsize': 25, 'fontweight': 'medium'})

axs[1].scatter(x_coord_po, y_coord_po, edgecolor = 'r', facecolor = 'r', alpha = 0.7, s = 30)
axs[1].set_xlabel("x")
axs[1].set_title("Hom. Poisson process", fontdict = {'fontsize': 25, 'fontweight': 'medium'})

#Uncomment the following command if you wish to save the resulting picture
#fig.savefig("problem1ab.png", bbox_inches='tight')


for i in range(0, number_points_po):
    print(x_coord_po[i], "   ", y_coord_po[i])