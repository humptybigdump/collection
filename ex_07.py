import numpy as np 
import matplotlib.pyplot as plt 

#### EXERCISE 1 ####

class Parabola():
    """Class describing parabolas of the form 
    y=a*x**2+b*x+c.
    Members:
    a, b, c
    Methods:
    __init__: receives three float parameters a, b, c
    value: value of the function at location x
    diff: value of the derivation of the function at location x
    """
    def __init__(self, a, b, c):
        self.a = a
        self.b = b
        self.c = c
    def value(self, x):
        return self.a*x**2+self.b*x+self.c
    def diff(self, x):
        return 2*self.a*x+self.b


# Complete the following similar classes. Use np.cos, np.sin and np.exp,
# so that multiple location values given as a numpy array can be computed
# quickly.
class Cos():
    """Class describing cosine functions of the form 
    y=cos(a*x).
    Members:
    a
    Methods:
    __init__: receives one float parameter a
    value: value of the function at location x
    diff: value of the derivation of the function at location x
    """
    pass

class Exp():
    """Class describing exponential functions of the form 
    y=exp(a*x).
    Members:
    a
    Methods:
    __init__: receives one float parameter a
    value: value of the function at location x
    diff: value of the derivation of the function at location x
    """
    pass

# Define the functions from the exercise sheet by initializing 
# the classes with specific values.

# Implement the following functions:
# Note: at the boundaries, use one of the other functions by giving it a 
# reduced location vector describing only the boundary.
def forward_euler(x, f):
    """From a location array x and a function f, calculate the values
    of the numerical derivation according to forward euler.
    Returns f'(x). 
    """
    pass

def backward_euler(x, f):
    """From a location array x and a function f, calculate the values
    of the numerical derivation according to forward euler.
    Returns f'(x). 
    """
    pass

def midpoint_rule(x, f):
    """From a location array x and a function f, calculate the values
    of the numerical derivation according to forward euler.
    Returns f'(x). 
    """
    pass

if __name__ == "__main__":
    # Generate a location vector x = [0, ..., 1].
    # Plot the numerical and analytical derivation for that vector with 
    # plt.plot(x, f'(x), label=some_string_to_label_the_plot)
    # Then use plt.legend() to create a legend and plt.show() to show
    # your plots.
    # Experiment with the resolution of the vector and the functions
    # and derivation methods used.
    pass

#### EXERCISE 2 ####
# For equidistant discretizations, it's simpler to store the grid 
# constant h together with the values, so it doesn't have to be recalculated.
# Complete the class Grid1D which does that.
class Grid1D():
    """1D grid for numerical differentiation.
    Members:
    h
    N
    x, an array of the values defined by the parameters.
    Methods:
    __init__: receives the first value of the grid x0, the grid constant h
    and the number of points N.
    backward_matrix: backward euler matrix A, such that f'(x)=A@f(x)
    forward_matrix: forward euler matrix A, such that f'(x)=A@f(x)
    midpoint_matrix: midpoint matrix A, such that f'(x)=A@f(x)
    """
    pass

if __name__ == "__main__":
    # Generate a grid, use its x vector to calculate the values of the 
    # functions from exercise 1, and then its matrices to calculate 
    # the differentiation of that function. 
    # Assert that the matrix method is close to the functions from exercise 1.
    pass


#### EXERCISE 3 (Bonus) ####

def reduced_system(matrix, value):
    """Calculate the modified rhs. Delete row and column 0 from the matrix."""
    pass

if __name__ == "__main__":
    # Generate a grid of length 1 and solve the differential equation f'-f
    # with a left-hand boundary condition f(0) = 1
    pass