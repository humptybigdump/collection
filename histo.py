# -*- coding: utf-8 -*-
"""
Created on Fri Nov 15 09:47:43 2024

@author: Emmanuel
"""

# import pandas as pd
# PP=pd.read_csv("txyz.txt",sep=" ")

import numpy as np
from matplotlib import pyplot as plt
from scipy import stats

# import the data
PP=np.loadtxt("D3D.txt", delimiter=",", skiprows=1)

# prepare for the fit with a normal distribution
# calculate mean and std of distribution
mm = np.mean(PP[:,1])
ss = np.std(PP[:,1])

# make a first figure with the histogram
fig, ax = plt.subplots()
# ax.hist(PP[:,1], bins=100)

# Choose how to bin
meth=1
if meth==0:
    # Scott's rule for bin size
    lbin=3.49*ss/np.power(len(PP[:,1]), 1/3)
else:
    # Freedman Diaconis' rule for bin size
    lbin=2.*stats.iqr(PP[:,1])/np.power(len(PP[:,1]), 1/3)

# create vector containing the bin limits
Vbins = np.arange(start=PP[:,1].min(),
                  stop=PP[:,1].max(),
                  step=lbin)

# Vbins = np.histogram_bin_edges(PP[:,1], bins="scott")
# Vbins = np.histogram_bin_edges(PP[:,1], bins="fd")
ax.hist(PP[:,1], bins=Vbins , density=True, label="Original data")
ax.grid()
ax.tick_params(labelsize=14)
ax.set_xlabel("Sample value")
ax.set_ylabel("Probability density")

# create the x and f(x) to be later represented
Xx = np.linspace(start=PP[:,1].min(),stop=PP[:,1].max(),num=100)
ax.plot(Xx, stats.norm.pdf(Xx, loc=mm, scale=ss), linewidth=5, label="Best normal fit")
# ax.legend(["Original data", "Best normal fit"])
ax.legend()
# save figure
plt.savefig("histo.png", dpi=200, format='png')