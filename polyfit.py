# -*- coding: utf-8 -*-
"""
Created on Tue Nov 19 18:34:47 2024

@author: Emmanuel
"""

import numpy as np
from matplotlib import pyplot as plt

# import the data
PP=np.loadtxt("D3D.txt", delimiter=",", skiprows=1)

# create a figure and the associated axes
fig, ax = plt.subplots()

# simply make the cross-plot and routine labelling
plt.plot(PP[:,0], PP[:,1], 'bo')
plt.grid()
ax.tick_params(labelsize=14)
ax.set_title("Regression on the D3D data", size=14)
ax.set_xlabel("X samples", size=14)
ax.set_ylabel("Y samples", size=14)
# differences along the x-axis and the y-axis have the same length
# with the command below. This respects the angles.
ax.axis('equal') 

# now fit a line (polynomial of 1st order) to get y = f(x)
Yfx = np.polyfit(PP[:,0], PP[:,1], deg=1)

# plot result on the graph: compute y-values at x locations
plt.plot(PP[:,0], np.polyval(Yfx, PP[:,0]), 'r', linewidth=3)

# now fit a line (polynomial of 1st order) to get x = g(y)
Xgy = np.polyfit(PP[:,1], PP[:,0], deg=1)

# plot result on the graph: compute y-values at x locations
plt.plot(np.polyval(Xgy, PP[:,1]), PP[:,1], 'g', linewidth=3)

# add the legend (remember the order we plot the things)
ax.legend(["Original data", "y=f(x)", "x=g(y)"], fontsize=14)
# save figure
plt.savefig("polyfit.png", dpi=200, format='png')

# now fit with the PCA
# first calculate covariance matrix
CC = np.cov(PP[:,0:2].T)
# then computes eigenvalues and eigenvectors
# and sort them in decreasing order
Eival, EIVEC = np.linalg.eig(CC)
idx = Eival.argsort()[::-1]   
Eival = Eival[idx]
EIVEC = EIVEC[:,idx]
# now prepare for the plot
# look for central point (mean of the cloud)
Ori = np.mean(PP[:,0:2], axis=0)
# define extension: 3 times the sqrt of the maximum eigenvalue
Ext = 3*np.sqrt(Eival[0])
# finally plot
plt.plot(Ori[0]+[-Ext*EIVEC[0,0], Ext*EIVEC[0,0]],
          Ori[1]+[-Ext*EIVEC[0,1], Ext*EIVEC[0,1]],
          'k', linewidth=3)

# add the legend (remember the order we plot the things)
ax.legend(["Original data", "y=f(x)", "x=g(y)", "PCA"], fontsize=14)
# save figure
plt.savefig("polyfit-w-pca.png", dpi=200, format='png')
