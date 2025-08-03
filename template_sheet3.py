# -----------------------------------
# Template for NOP Excercise sheet 03
# -----------------------------------

import numpy as np
from Solvers import GradientDescent  # Import the solver(s)
from Problem import *  # The functions we want to minimize
from Visualize import *  # Visualization helper

# The 'from MyFile import *' statement imports all names in the file MyFile.py

# Try out your own functions from sheet 02 here
myf1 = f1()  

# TODO: Implement the Gradient Descent scheme in Solvers.py
solver = GradientDescent(myf1)

# Initialize the visualizer
plot_range_x = 6
plot_range_y = 6
viz = Visualize(myf1.value, np.linspace(-plot_range_x, plot_range_x, 500), np.linspace(-plot_range_y, plot_range_y, 500))

# Add the contour plot
viz.addContourPlot()

# A list of points to try out as initial values for Gradient Descent
x0_list = [np.array([0., 0.]), np.array([0., -4.]), np.array([0.2, -4.]),np.array([-3., -0.5]), np.array([-3., 0.5]), np.array([0., -1.])]


for x0 in x0_list:

    # Preset for alpha (might be adapted for more advances step size control)
    # alpha = 0.01

    # - Minimize the function using the initial value x0
    # - solver.solve() should return:
    #     - the solution x 
    #     - a list of the intermediate x values x_list_inter
    #       arising during the evaluation of the Gradient Descent scheme
    # - To make a function return multiple values a and b, wrap them in an outer list, e.g. return [a, b]

    alpha = 0.01
    x, res_list, x_list = solver.solve(x0, alpha)

    # Print the results
    print(
        f"Found a solution for x0 = {x0}: x = {x}, alpha = {alpha}")

    # Add the list of intermediate solution points to the plot
    if x_list is not None:
        viz.addXValues(x_list)
viz.showAll()
