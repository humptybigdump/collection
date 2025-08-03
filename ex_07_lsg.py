import numpy as np 
import matplotlib.pyplot as plt 
from scipy.sparse import dia_matrix

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
    def __init__(self, a):
        self.a = a
    def value(self, x):
        return np.cos(self.a*x)
    def diff(self, x):
        return -self.a*np.sin(self.a*x)

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
    def __init__(self, a):
        self.a = a
    def value(self, x):
        return np.exp(self.a*x)
    def diff(self, x):
        return self.a*np.exp(self.a*x)

# Define the functions from the exercise sheet by initializing 
# the classes with specific values.
f_1 = Cos(100)
f_2 = Parabola(5, -3, 5)
f_3 = Exp(-5)

# Implement the following functions:
# Note: at the boundaries, use one of the other functions by giving it a 
# reduced location vector describing only the boundary.
def forward_euler(x, f):
    """From a location array x and a function f, calculate the values
    of the numerical derivation according to forward euler.
    Returns f'(x). 
    """
    result = np.zeros(x.shape)
    values = f.value(x)
    h = x[1:]-x[:-1]
    result[0:-1] = 1/h*(values[1:]-values[:-1])
    result[-1] = 1/h[-1]*(values[-1]-values[-2])
    return result

def backward_euler(x, f):
    """From a location array x and a function f, calculate the values
    of the numerical derivation according to forward euler.
    Returns f'(x). 
    """
    result = np.zeros(x.shape)
    values = f.value(x)
    h = x[1:]-x[:-1]
    result[1:] = 1/h*(values[1:]-values[:-1])
    result[0] = 1/h[0]*(values[1]-values[0])
    return result 

def midpoint_rule(x, f):
    """From a location array x and a function f, calculate the values
    of the numerical derivation according to forward euler.
    Returns f'(x). 
    """
    result = np.zeros(x.shape)
    values = f.value(x)
    h = x[2:]-x[:-2]
    result[1:-1] = 1/h*(values[2:]-values[:-2])
    result[0] = 1/(x[1]-x[0])*(values[1]-values[0])
    result[-1] = 1/(x[-1]-x[-2])*(values[-1]-values[-2])
    return result 

if __name__ == "__main__":
    # Generate a location vector x = [0, ..., 1].
    # Plot the numerical and analytical derivation for that vector with 
    # plt.plot(x, f'(x), label=some_string_to_label_the_plot)
    # Then use plt.legend() to create a legend and plt.show() to show
    # your plots.
    # Experiment with the resolution of the vector and the functions
    # and derivation methods used.
    for n in [10, 100, 1000]:
        x = np.linspace(0, 1, n)
        for f in [f_1, f_2, f_3]:
            plt.plot(x, f.diff(x), label="analytical")
            plt.plot(x, forward_euler(x, f), label="forward")
            plt.plot(x, backward_euler(x, f), label="backward")
            plt.plot(x, midpoint_rule(x, f), label="midpoint")
            plt.legend()
            plt.show()

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
    def __init__(self, x0, h, N):
        self.h = h
        self.N = N
        self.x = np.linspace(x0, x0+h*(N-1), N)
    def backward_matrix(self, sparse=True):
        if sparse:
            data = np.zeros((3,self.N))
            offsets = [-1, 0, 1]
            data[0,:] = -1
            data[1,:] = 1
            data[1,0] = -1
            data[2,1] = 1
            result = dia_matrix((data,offsets), shape=(self.N,self.N))
        else:
            # very unoptimized solution using a for loop
            result = np.zeros((self.N, self.N))
            for i in range(1, self.N):
                result[i, i-1] = -1
                result[i, i] = 1
            result[0,0] = -1
            result[0,1] = 1
        return result/self.h

    def forward_matrix(self, sparse=True):
        if sparse:
            data = np.zeros((3,self.N))
            offsets = [-1, 0, 1]
            data[1,:] = -1
            data[2,:] = 1
            data[1,-1] = 1
            data[0,-2] = -1
            result = dia_matrix((data,offsets), shape=(self.N,self.N))
        else:
            result = np.zeros((self.N, self.N))
            for i in range(0, self.N-1):
                result[i, i] = -1
                result[i, i+1] = 1
            result[-1,-1] = 1
            result[-1, -2] = -1
        return result/self.h

    def midpoint_matrix(self, sparse=True):
        if sparse:
            data = np.zeros((3,self.N))
            offsets = [-1, 0, 1]
            data[0,:] = -1/2
            data[2,:] = 1/2
            data[1,0] = -1
            data[2,1] = 1
            data[1,-1] = 1
            data[0,-2] = -1
            result = dia_matrix((data,offsets), shape=(self.N,self.N))
        else:
            result = np.zeros((self.N, self.N))
            for i in range(1, self.N-1):
                result[i, i-1] = -1/2
                result[i, i+1] = 1/2
            result[-1,-1] = 1
            result[-1,-2] = -1
            result[0,0] = -1
            result[0,1] = 1
        return result/self.h


if __name__ == "__main__":
    # Generate a grid, use its x vector to calculate the values of the 
    # functions from exercise 1, and then its matrices to calculate 
    # the differentiation of that function. 
    # Assert that the matrix method is close to the functions from exercise 1.
    pass

def test_grid1D():
    grid = Grid1D(-1, 0.1, 5)
    x = grid.x
    f = f_1.value(x)
    print(grid.backward_matrix()@f)
    print(grid.backward_matrix().todense())
    print(grid.forward_matrix().todense())
    print(backward_euler(x, f_1))
    print(forward_euler(x, f_1))
    assert np.allclose(grid.backward_matrix(sparse=False)@f, backward_euler(x, f_1))
    assert np.allclose(grid.forward_matrix(sparse=False)@f, forward_euler(x, f_1))
    print("MIDPOINT")
    print(midpoint_rule(x, f_1))
    print(grid.midpoint_matrix(sparse=False)@f)
    print(grid.midpoint_matrix()@f)
    assert np.allclose(grid.midpoint_matrix(sparse=False)@f, midpoint_rule(x, f_1))
    print(grid.backward_matrix().todense())
    print(grid.backward_matrix(sparse=False))
    assert np.allclose(grid.backward_matrix()@f, backward_euler(x, f_1))
    assert np.allclose(grid.forward_matrix()@f, forward_euler(x, f_1))
    assert np.allclose(grid.midpoint_matrix()@f, midpoint_rule(x, f_1))

if __name__=="__main__":
    test_grid = Grid1D(0,.1,6)
    print(test_grid.forward_matrix().todense())
    #test_grid1D()


#### EXERCISE 3 (Bonus) ####

def reduced_system(matrix, value):
    """Calculate the modified rhs. Delete row and column 0 from the matrix."""
    delta_rhs = -matrix[1:,0]*value
    reduced = matrix[1:, 1:].copy()
    return reduced, delta_rhs

if __name__ == "__main__":
    # Generate a grid of length 1 and solve the differential equation f'-f
    # with a left-hand boundary condition f(0) = 1
    length = 1
    N = 100
    h = length/(N-1)
    grid = Grid1D(0, h, N)
    matrix_dense = grid.midpoint_matrix().todense() - np.eye(N)
    value = 1
    reduced, rhs = reduced_system(matrix_dense, value)
    reduced_solution = np.linalg.solve(reduced, rhs)
    full_solution = np.zeros(N)
    full_solution[1:] = reduced_solution.flatten()
    full_solution[0] = value
    plt.plot(grid.x, full_solution)
    plt.show()