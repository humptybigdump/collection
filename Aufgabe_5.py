# -*- coding: utf-8 -*-
"""
Created on Tue Nov  5 13:32:21 2019

"""

from scipy import stats
import matplotlib.pyplot as plt
 
###Oberservation window
x_min = 0 
x_max = 1

y_min = 0 
y_max = 1

area = (x_max - x_min) * (y_max - y_min)

###Simmulation einzelner Punkt
number_points_1 = 1
#In each coordinate, distribute the points uniformly
x_coord_1 = stats.uniform.rvs(loc = x_min, scale = x_max - x_min, size = number_points_1)
y_coord_1 = stats.uniform.rvs(loc = y_min, scale = y_max - y_min, size = number_points_1)

fig, axs = plt.subplots(figsize = (8, 8))

axs.scatter(x_coord_1, y_coord_1, edgecolor = 'b', facecolor = 'b', alpha = 0.7, s = 30)
axs.set_xlabel("x")
axs.set_title("Einzelner Punkt", fontdict = {'fontsize': 25, 'fontweight': 'medium'})
plt.xlim(0,1)
plt.ylim(0,1)

#%%
###Simmulation mehrerer Punkte
number_points_2 = int(input("Wie viele Punkte? "))
#In each coordinate, distribute the points uniformly
x_coord_2 = stats.uniform.rvs(loc = x_min, scale = x_max - x_min, size = number_points_2)
y_coord_2 = stats.uniform.rvs(loc = y_min, scale = y_max - y_min, size = number_points_2)

fig, axs = plt.subplots(figsize = (8, 8))

axs.scatter(x_coord_2, y_coord_2, edgecolor = 'b', facecolor = 'b', alpha = 0.7, s = 30)
axs.set_xlabel("x")
axs.set_title("Mehrere Punkte", fontdict = {'fontsize': 25, 'fontweight': 'medium'})
plt.xlim(0,1)
plt.ylim(0,1)

#%%
###Simmulation mehrerer Punkte
gamma = int(input("Welche Intensität? "))
number_points_3 = stats.poisson.rvs(gamma * area)
#In each coordinate, distribute the points uniformly
x_coord_3 = stats.uniform.rvs(loc = x_min, scale = x_max - x_min, size = number_points_3)
y_coord_3 = stats.uniform.rvs(loc = y_min, scale = y_max - y_min, size = number_points_3)

fig, axs = plt.subplots(figsize = (8, 8))

axs.scatter(x_coord_3, y_coord_3, edgecolor = 'b', facecolor = 'b', alpha = 0.7, s = 30)
axs.set_xlabel("x")
axs.set_title("Mehrere Punkte/ zufällige Anzahl", fontdict = {'fontsize': 25, 'fontweight': 'medium'})

plt.xlim(0,1)
plt.ylim(0,1)

