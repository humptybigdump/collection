import numpy as np
import scipy.sparse
import timeit
import matplotlib.pyplot as plt
from ex_03 import sym

#### EXERCISE 1 ####
def is_posdef_sym(matrix):
    """Checks whether a matrix is positive-definite and symmetric."""
    pass

def cholesky(matrix):
    """Returns L according to the Cholesky decomposition of matrix."""
    pass

def solve_cholesky(matrix, rhs):
    """Solves the equation system matrix@x=rhs using the Cholesky
    decomposition."""
    pass


#### EXERCISE 2 ####
def np_matrix(d):
    """Creates A as a numpy array."""
    pass

def coo_matrix(d):
    """Creates A as a coo matrix."""
    pass

def csr_matrix(d):
    """Creates A as a coo matrix."""
    pass

def time_matrix_product(A, x):
    """Performs a matrix vector product and returns elapsed time in seconds."""
    start = timeit.default_timer()
    y = A@x
    stop = timeit.default_timer()
    return stop-start

if __name__ == "__main__":
    """Time the matrix product for various d. How large a matrix can your 
    computer handle?"""
    pass