import numpy as np
import matplotlib.pyplot as plt
from polynomials import Polynomial
from ex_09 import LinElement1D, FEMGrid1D
from ex_04 import stiffness


### EXERCISE 1 ###

# Define a function to create and plot the base functions for a 1D FEM element
# of order n.

def plot_n_polynomials(n):
    """Fit and plot the base functions for a 1D FEM element of order n."""
    pass

### EXERCISE 2 ###

# Implement the class ReferenceElement1D, which calculates int(N*N) for a 1D
# FEM element of order n over [-1, 1].

class ReferenceElement1D():
    """Reference element of arbitrary order for 1D FEM. Always [-1, 1].

        members:
    dimension: 1
    n_nodes: number of nodes
    N: array of base functions (shape n_nodes)
    N_grad_squared_integral: relevant integral to calculate affine element K
        (shape (1, n_nodes, 1, 1, n_nodes, 1), for compatibility with 2D FEM)
    """
    def __init__(self, n_nodes):
        pass
        
    def get_N_grad_squared_integral(self):
        pass

### EXERCISE 3 ### 

# Implement the class AffineElement which uses the integral from
# ReferenceElement1D with a particular affine transformation to calculate K.

class AffineElement():
    """Affine FEM element based on a reference element of arbitrary dimension.

        members:
    nodes: node indices of element nodes
    reference element: underlying element for [-1, 1]
    A: transformation matrix
    c: transformation offset
    stiffness: full tensorial 3x3x3x3 stiffness
    reduced stiffness: dimensionally reduced dxdxdxd stiffness
    dimension: dimension of the reference element
    K: stiffness matrix
    """
    def __init__(self, nodes, reference_element, c, A, stiffness):
        pass

    def get_K(self):
        pass    
