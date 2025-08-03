import numpy as np
from scipy import linalg

#### EXERCISE 1 ####
# Verify the following property of inversion: the solution eps of the linear
# equation system sigma = C[eps] is eps = C^-1[sigma].
# Use np.linalg.inv and np.linalg.solve, and an assert statement.
# note: use np.allclose instead of a "==" sign because there might be (small)
# numerical errors
# Inverting fourth-order tensors is best done in the normed voigt notation.
# Import your work from the previous exercise if necessary -- no copy-pasting!

stiffness = np.array([[[[282692.30769231,     0.,             0.],
                        [0.,        121153.84615385,     0.],
                        [0.,             0.,        121153.84615385]],
                       [[0.,         80769.23076923,     0.],
                        [80769.23076923,     0.,             0.],
                        [0.,             0.,             0.]],
                       [[0.,             0.,         80769.23076923],
                        [0.,             0.,             0.],
                        [80769.23076923,     0.,             0.]]],
                      [[[0.,         80769.23076923,     0.],
                        [80769.23076923,     0.,             0.],
                        [0.,             0.,             0.]],
                       [[121153.84615385,     0.,             0.],
                        [0.,        282692.30769231,     0.],
                        [0.,             0.,        121153.84615385]],
                       [[0.,             0.,             0.],
                        [0.,             0.,         80769.23076923],
                        [0.,         80769.23076923,     0.]]],
                      [[[0.,             0.,         80769.23076923],
                        [0.,             0.,             0.],
                        [80769.23076923,     0.,             0.]],
                       [[0.,             0.,             0.],
                        [0.,             0.,         80769.23076923],
                        [0.,         80769.23076923,     0.]],
                       [[121153.84615385,     0.,             0.],
                        [0.,        121153.84615385,     0.],
                        [0.,             0.,        282692.30769231]]]])
stress = np.array([[200,   0, -300],
                   [0,   0,    0],
                   [-300,   0,    0]])


def test_inversion():
    pass

# Similarly, compare np.linalg.solve with linalg.lu_factor (the LUP
# decomposition) and linalg.lu_solve


def test_LU():
    pass


#### EXERCISE 2 ####
def LUP(matrix):
    """Returns L, U, P according to the LUP decomposition of matrix."""
    pass

# Write a test for your function by reconstructing the matrix.


def test_LUP():
    pass


def solve_LUP(matrix, rhs):
    """Solves matrix@x = rhs using the LUP decomposition."""
    pass

# Write a test for your function by comparing with np.linalg.solve


def test_solve_LUP():
    pass
