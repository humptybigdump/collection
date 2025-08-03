# -*- coding: utf-8 -*-
"""
Created on Fri Nov 27 10:31:04 2020

@author: Herold
"""
import numpy as np
from sympy import *
import math
import matplotlib.pyplot as plt
import pickle

def nteWurzel(x, n):
	return x**(1/float(n))


#%% Optional: Daten speichern
with open("X_save.txt", "wb") as fp:   #Pickling
    pickle.dump(X, fp)

with open("Y_save.txt", "wb") as fp:   #Pickling
    pickle.dump(Y, fp)
#%%    Daten aus dem Ilias-Kurs einlesen
with open("X_save.txt", "rb") as fp:   # Unpickling
    X_imp = pickle.load(fp)
with open("Y_save.txt", "rb") as fp:   # Unpickling
    Y_imp = pickle.load(fp)

#%% 1. Teil Schätzer Intensitätsmaß
a=0
b=0.1
c=0
d=0.1
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

#%% 2. Teil Visualsierung der geschätzen Intensität
from mpl_toolkits.mplot3d import Axes3D  # noqa: F401 unused import


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