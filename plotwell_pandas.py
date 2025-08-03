# -*- coding: utf-8 -*-
"""
Created on Wed Oct 30 13:28:52 2024

@author: Emmanuel

Version "pandas": Uses the standard python library 
                       the matplotlib library
                       the pandas library

           Only reproduce Task 2 
"""

# we will have to plot so we import the corresponding library
import matplotlib.pyplot as plt # this will be used for plotting
import pandas as pd

"""===========================================================================
Task 2: Create 3 subplots with depth as vertical axis increasing down, and
        as horizontal axes density, slowness P and slowness S respectively
        
        Same options as Task 1 but in addition put horizontal axes and labels 
        on the top
==========================================================================="""

# Read input file.
# Format is assumed ASCII (text file) with the 1st row taken as comment
# There are 6 columns separated by ","
# Columns are: depth_(ft),rho_(g/cc),slowp_(us/ft),slows_(us/ft),dummy,azi_(rad),inc_(rad)

# The pandas library will take care and understand how to read the csv file
# It will return a DataFrame object

WELL = pd.read_csv("well.txt")

# if quick and dirty check of the data in WELL object needed uncomment below
# WELL.plot()
# plt.show()

# Now make the plot using a loop
# Before, prepare list of xlabels
XLAB = ["Density (g/cc)", "Slowness S (us/ft)", "Slowness P (us/ft)"]
COL = ["r", "g", "b"]

# create a new figure
# Unlike Task 1, we differentiate the figure from the axes because we will
# create the axes as they come in the loop
fig=plt.figure(layout="constrained") 

# loop over the subplots
for ip in range(3):
    # create the first subplot and corresponding axes
    ax = plt.subplot(1,3,ip+1)
    # the color will depend on the subplot
    ax.plot(WELL[WELL.columns[ip+1]], WELL[WELL.columns[0]], color=COL[ip])
    # add a grid to the plot
    ax.grid()
    # flip up-down the y-axis, which correspond to depth
    ax.invert_yaxis()
    # set the size of the ticks to 12
    ax.tick_params(labelsize=12)
    # put the x-axis ticks and labels to the top
    ax.xaxis.tick_top()
    ax.xaxis.set_label_position("top")
    # the x axis label dpends on the variable to plot
    ax.set_xlabel(XLAB[ip], size=12, color=COL[ip])
    # put the y-axis label only on the first (left) subplot
    if ip==0:
        ax.set_ylabel("Depth (ft)", size=12, color="k")

# display figure
plt.show()    
# save figure
plt.savefig('plotwell_pandas.png', dpi=200, format='png') # save file
# close figure
plt.close()