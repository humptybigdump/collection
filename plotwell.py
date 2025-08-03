# -*- coding: utf-8 -*-
"""
Created on Wed Oct 30 13:28:52 2024

@author: Emmanuel

Version 0: Uses only the standard python library 
                 and the matplotlib library
"""

# we will have to plot so we import the corresponding library
import matplotlib.pyplot as plt # this will be used for plotting

"""===========================================================================
Task 1: Plot the rock density along depth

        Put labels to the axes
        Put a grid
        Depth is on the y-axis with increasing values going down (reverse axis)
        Save figure
==========================================================================="""

# Read input file.
# Format is assumed ASCII (text file) with the 1st row taken as comment
# There are 6 columns separated by ","
# Columns are: depth_(ft),rho_(g/cc),slowp_(us/ft),slows_(us/ft),dummy,azi_(rad),inc_(rad)

finp = open("well.txt","rt")
Depth = []
Rho = []
for line in finp:
    elt = line.split(",")
    if elt[0]=="depth_(ft)": # skip the first line
        continue
    else: # we are only interested in the 1st 2 columns
        Depth.append(float(elt[0]))
        Rho.append(float(elt[1]))
finp.close()        

# Now make the plot
# create figure and axes and define its size in cm
fig,ax=plt.subplots(1,1,
                    layout="constrained")
# plot black points and line
plt.plot(Rho, Depth, "ko-")
# flip up-down the y-axis, which correspond to depth 
ax.invert_yaxis()
# add a grid to the plot
ax.grid()
# define label of axes
ax.set_xlabel("Density (g/cc)")
ax.set_ylabel("Depth (ft)")
# define title of axes
ax.set_title("Formation density profile at the well")
# show the figure (otherwise, you do not see it although it exists!)
plt.show()
# save figure
plt.savefig('plotwell_t1.png', dpi=200, format='png')
# close figure
plt.close()

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

# Clear previous input data variables because we want to read another way
# the input file
# We want to read the data in a matrix like way, in practise a list of list way
# because then, we can creat a loop to make the subplots

del Depth, Rho

finp = open("well.txt","rt")
LL = []
for line in finp:
    elt = line.split(",")
    if elt[0]=="depth_(ft)": # skip the first line
        continue
    else:
        LL.append([float(elt[0]), float(elt[1]), float(elt[2]), float(elt[3])])
finp.close()        

# We need to transpose LL for plotting
# Three possibilities exist: 1, 2 or 3
poss=1
if poss==1:
    # First solution with built-in function zip()
    # transpose LL but output result (DATA) is a list of tuples
    DATA = list(zip(*LL))
elif poss==2:
    # Second solution by two nested loops
    DATA = list()
    for ii in range(len(LL[0])):
        row = list()
        for sublist in LL:
            row.append(sublist[ii])
        DATA.append(row)
elif poss==3:
    # Third solution as second one but in a so-called list comprehension way
    DATA = [[row[ii] for row in LL] for ii in range(len(LL[0]))]

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
    ax.plot(DATA[ip+1], DATA[0], COL[ip])
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
plt.savefig('plotwell_t2.png', dpi=200, format='png') # save file
# close figure
plt.close()

"""===========================================================================
Task 3: Make a polar plot of the azimuth and inclination of the fracture 
==========================================================================="""

# Read input file.
# Format is assumed ASCII (text file) with the 1st row taken as comment
# There are 6 columns separated by ","
# Columns are: depth_(ft),rho_(g/cc),slowp_(us/ft),slows_(us/ft),dummy,azi_(rad),inc_(rad)

finp = open("well.txt","rt")
Azi = []
Inc = []
for line in finp:
    elt = line.split(",")
    if elt[0]=="depth_(ft)": # skip the first line
        continue
    else: # we are only interested in the 1st 2 columns
        Azi.append(float(elt[-2]))
        Inc.append(float(elt[-1]))
finp.close()

# Prepare the polar plot
# Pi number does not exist in the standard library
PI = 3.141592654
# Create a dictionary with the expected keyword arguments (*kwargs)
polar_kw = dict(projection="polar",
                theta_offset = PI/2,
                theta_direction = -1)

# Now make the plot
fig,ax=plt.subplots(1,1,
                    layout="constrained",
                    subplot_kw=polar_kw)
# plot blue points
# WARNING: we need to convert the inclination values from radians to degrees
plt.polar(Azi, [Inc[ii]*180.0/PI for ii in range(len(Inc))], "bo")
# set the size of the ticks to 12
ax.tick_params(labelsize=12)
ax.set_title("Fracture orientation", size=12, color="r")
# show the figure (otherwise, you do not see it although it exists!)
plt.show()
# save figure
plt.savefig('plotwell_t3.png', dpi=200, format='png')
# close figure
plt.close()