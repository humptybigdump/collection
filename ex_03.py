import numpy as np


#### EXERCISE 1 ####
# write functions which calculate sym and skw of a matrix.
def sym(matrix):
    """Returns the symmetric part of matrix."""
    pass


def skw(matrix):
    """Returns the skew part of matrix."""
    pass


# this if condition assures that the code is not executed if the file is
# imported as a module.
if __name__ == "__main__":
    A = np.array([[1, 2, 3],
                  [4, 5, 6],
                  [7, 8, 9]])
    x = np.array([1, 2, 3])
    # CALCULATE det(A), rg(A), sym(A), skw(A), tr(A), ||A||_2,
    # eigenvalues and eigenvectors
    # as well as the matrix-vector product A x
    # use numpy functions where appropriate.
    # print each result to console.
    pass


#### EXERCISE 2 ####
# write a function which takes a 3,3 matrix (second-order tensor) and returns
# the normed voigt notation.


def normed_voigt_order_2(matrix):
    """Returns the normed voigt notation of a 3x3 matrix."""
    pass

# write two functions which take a 3,3 matrix (second-order tensor) and
# return the non-normed voigt notations (one each for stress, strain)


def stress_voigt_order_2(matrix):
    """Returns the stress voigt notation of a 3x3 matrix."""
    pass


def strain_voigt_order_2(matrix):
    """Returns the strain voigt notation of a 3x3 matrix."""
    pass

# write similar functions for order 4.


def normed_voigt_order_4(matrix):
    """Returns the normed voigt notation of a 3x3x3x3 matrix."""
    pass


def stress_voigt_order_4(matrix):
    """Returns the stress voigt notation of a 3x3x3x3 matrix."""
    pass


def strain_voigt_order_4(matrix):
    """Returns the strain voigt notation of a 3x3x3x3 matrix."""
    pass

# finally, write functions which work for input of order 2 _and_ 4, using your
# previous functions.


def normed_voigt(matrix):
    """Returns the normed voigt notation of a 3x3 or 3x3x3x3 matrix."""
    pass


def stress_voigt(matrix):
    """Returns the stress voigt notation of a 3x3 or 3x3x3x3 matrix."""
    pass


def strain_voigt(matrix):
    """Returns the strain voigt notation of a 3x3 or 3x3x3x3 matrix."""
    pass

# If you wish, you may check your work yourself at any point, by using the
# provided unit tests or self-written tests.

#### EXERCISE 3 (bonus) ####


def rotation(phi, theta, psi):
    """Calculates a 3x3 rotation matrix from three euler angles phi1, theta, 
    phi2 in radiants.
    """
    pass

# hint: np.einsum is very close to our index notation, e.g. A_ij b_j is equal
# to np.einsum("ij,j", A, b)


def rotation_nv(phi, theta, psi):
    """Calculates a 6x6 rotation matrix to be applied to tensors in normed 
    voigt notation.
    """
    pass
