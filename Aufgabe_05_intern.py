# -*- coding: utf-8 -*-
"""
Created on Fri Nov 27 10:31:04 2020

@author: Herold
"""
import numpy as np
from sympy import *
import math
import matplotlib.pyplot as plt

def nteWurzel(x, n):
	return x**(1/float(n))

n=100
gamma=5000
number_points = stats.poisson.rvs(gamma,size=n)
X=[]
Y=[]
D=[]
for i in range(0, n):
    X.append([])
    Y.append([])
    D.append([])
    u_dist= stats.uniform.rvs(loc = 0, scale = 1, size = number_points[i])
    u_rad= stats.uniform.rvs(loc = 0, scale = 2*np.pi, size = number_points[i])
    for j in range(0,number_points[i]):
        new_x=nteWurzel(u_dist[j]/4,4)*np.cos(u_rad[j])+1/2
        new_y=nteWurzel(u_dist[j]/4,4)*np.sin(u_rad[j])+1/2
        if new_x<=1:
            if new_y<=1:
                if new_x>=0:
                    if new_y>=0:
                        X[i].append(new_x)
                        Y[i].append(new_y)
                        D[i].append(math.sqrt((new_y-0.5)**2+(new_x-0.5)**2))
   
plt.hist(D[0], bins=200)
#%%                        
fig, axs = plt.subplots(figsize = (8, 8))

axs.scatter(X[0], Y[0], edgecolor = 'b', facecolor = 'b', alpha = 0.7, s = 30)
axs.set_xlabel("x")
axs.set_title("Mehrere Punkte", fontdict = {'fontsize': 25, 'fontweight': 'medium'})
plt.xlim(0,1)
plt.ylim(0,1)      

#%% 
import pickle

with open("X_save.txt", "wb") as fp:   #Pickling
    pickle.dump(X, fp)

with open("Y_save.txt", "wb") as fp:   #Pickling
    pickle.dump(Y, fp)
#%%    
with open("X_save.txt", "rb") as fp:   # Unpickling
    X_imp = pickle.load(fp)
with open("Y_save.txt", "rb") as fp:   # Unpickling
    Y_imp = pickle.load(fp)

#%%
a=0
b=1
c=0
d=1
count=0
X_all=[]
Y_all=[]
for i in range(0,len(X_imp)):
    for j in range(0,len(X_imp[i])):
        X_all.append(X_imp[i][j])
        Y_all.append(Y_imp[i][j])
        if X_imp[i][j] > a and X_imp[i][j] < b and Y_imp[i][j] > c and Y_imp[i][j] < d:
            count=count+1

print(count/len(X_imp))

#%%
from mpl_toolkits.mplot3d import Axes3D  # noqa: F401 unused import

import matplotlib.pyplot as plt
import numpy as np


fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')
number_bins=10
hist, xedges, yedges = np.histogram2d(X_all, Y_all, bins=number_bins, range=[[0, 1], [0, 1]])

# Construct arrays for the anchor positions of the bins*bins bars.
xpos, ypos = np.meshgrid(xedges[:-1] , yedges[:-1] , indexing="ij")
xpos = xpos.ravel()
ypos = ypos.ravel()
zpos = 0

# Construct arrays with the dimensions for the bars.
dx = dy = 1/(2*number_bins)* np.ones_like(zpos)
dz = hist.ravel()

ax.bar3d(xpos, ypos, zpos, dx, dy, dz/len(X_imp), zsort='average')

plt.show()