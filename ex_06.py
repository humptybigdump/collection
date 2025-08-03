import numpy as np
import scipy.sparse
import scipy.sparse.linalg
import matplotlib.pyplot as plt


#### EXERCISE 1 ####
# Program a CG solver. Try it out. In particular, look at what happens if you
# change the keyword arguments abs_tol and max_N.
def solve_CG(A, b, x0=None, maxit=1000, tol=10**-8):
    """Solves the equation system A@x=y using the CG approach.
    Returns x and the list of all residuals
    """
    pass


def Test_LES(d):
    """Creates a test linear equation system of dimension d 
    by giving a matrix A and a right hand side b"""
    b = np.ones(d)
    A = np.zeros([d,d])
    for i in range(d):
        A[i,i] = 2
        if (i!=0):
            A[i,i-1] = -1
        if (i!=d-1):
            A[i,i+1] = -1
    return A,b


if __name__ == "__main__":
    d = 10
    A,b = Test_LES(d)