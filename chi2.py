# -*- coding: utf-8 -*-
"""
Created on Mon Nov 25 18:50:36 2024

@author: Emmanuel
"""

# In this script, we want to check with the chi2-test if the data in the
# first column of the D3D are following a Normal distribution

import numpy as np
from matplotlib import pyplot as plt
from scipy import stats

# 1- import the data and keep first column only
PP = np.loadtxt("D3D.txt", delimiter=",", skiprows=1)
Xx = PP[:,0]

# 2- calculates the histogram and keep info on the bins
Hist, Bin_edges = np.histogram(Xx, bins="fd")

# check that all bins have more than 5 counts otherwise decrease the number
# of bins
while np.min(Hist)<5:
    Hist, Bin_edges = np.histogram(Xx, bins=len(Hist)-1)

print(f"Histogram optimal number of bins is: {len(Hist)}")

# 3- compute the theoretical histogram from the Normal distribution
# at the middle of each bin of the "observed" histogram
# calculate the array of centers
Midx = np.diff(Bin_edges[0:2])*0.5+Bin_edges[0:-1]
# calculate theoretical histogram. ddof=1 because unbiased distribution (i.e. 
# distribution on a statistical sample and not the population)
Hist_exp = stats.norm.pdf(Midx, np.mean(Xx), np.std(Xx,ddof=1))

# 4- rescale the pdf to transform it in "counts"
# and to fit with observed data counts
# because it is a pdf, the surface is used to rescale
Hist_exp = Hist_exp*np.sum(Hist)/np.sum(Hist_exp)

# 5- plot the histograms now, overlay them
fig, ax = plt.subplots()
# original data
plt.bar(Midx,Hist,color='b', width=0.9*np.diff(Bin_edges[0:2]))
# Expectation according to Normal distribution
plt.bar(Midx,Hist_exp,color='r', width=0.5*np.diff(Bin_edges[0:2]))
# basic labelling
ax.grid()
ax.tick_params(labelsize=14)
ax.set_xlabel("Value", size=14)
ax.set_ylabel("Count", size=14)
ax.set_title("Comparison with Normal distribution", size=14)
# add the legend (remember the order we plot the things)
ax.legend(["Original data", "Expectation"], fontsize=14)
plt.show()
# save figure
plt.savefig("chi2.png", dpi=200, format='png')

# 6- calculate the observed chisquare
chi2calc = np.sum((Hist-Hist_exp)**2/Hist_exp)

# 7- get the critical chisquare value at 95% significant level
# minus three because 2 parameters to define Normal distribution and we are
# dealing with a one variable sample
chi2crit = stats.chi2.ppf(1-0.05, len(Midx)-3)

# 8- see if we accept or reject that "our statistical data sample follows
# the proposed Normal distribution"
if (chi2crit > chi2calc):
   print("The observed statistical sample follows a Normal " +
         "distribution with 95% significance level")
   print("Chi-square observed = {:.2f} < {:.2f} (critical value)"\
         .format(chi2calc, chi2crit))
else:
   print("The observed statistical sample does not follows a Normal " +
         "distribution with 95% significance level")
   print("Chi-square observed = {:.2f} > {:.2f} (critical value)"\
         .format(chi2calc, chi2crit))
