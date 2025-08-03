import numpy as np


#### EXERCISE 1 ####
# write functions which calculate sym and skw of a matrix.
def sym(matrix):
    """Returns the symmetric part of matrix."""
    return 0.5*(matrix+matrix.T)


def skw(matrix):
    """Returns the skew part of matrix."""
    return 0.5*(matrix-matrix.T)


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
    print(np.linalg.det(A))
    print(np.linalg.matrix_rank(A))
    print(sym(A))
    print(skw(A))
    print(np.trace(A))
    print(np.linalg.norm(A))
    print(np.linalg.eig(A))
    print(A@x)


#### EXERCISE 2 ####
# write a function which takes a 3,3 matrix (second-order tensor) and returns
# the normed voigt notation.
normed_voigt_transformation = np.zeros((6, 3, 3))
stress_voigt_transformation = np.zeros((6, 3, 3))
strain_voigt_transformation = np.zeros((6, 3, 3))
for i in range(3):
    for j in range(3):
        if i == j:
            normed_voigt_transformation[i, j, j] = 1
            stress_voigt_transformation[i, j, j] = 1
            strain_voigt_transformation[i, j, j] = 1
        else:
            k = 3-(i+j)
            normed_voigt_transformation[k+3, i, j] = 1/np.sqrt(2)
            stress_voigt_transformation[k+3, i, j] = 1/2
            strain_voigt_transformation[k+3, i, j] = 1


def normed_voigt_order_2(matrix):
    """Returns the normed voigt notation of a 3x3 matrix."""
    return np.einsum("ijk, jk", normed_voigt_transformation, matrix)

# write two functions which take a 3,3 matrix (second-order tensor) and
# return the non-normed voigt notations (one each for stress, strain)


def stress_voigt_order_2(matrix):
    """Returns the stress voigt notation of a 3x3 matrix."""
    return np.einsum("ijk, jk", stress_voigt_transformation, matrix)


def strain_voigt_order_2(matrix):
    """Returns the strain voigt notation of a 3x3 matrix."""
    return np.einsum("ijk, jk", strain_voigt_transformation, matrix)

# write similar functions for order 4.


def normed_voigt_order_4(matrix):
    """Returns the normed voigt notation of a 3x3x3x3 matrix."""
    return np.einsum("ikl, jmn, klmn", normed_voigt_transformation,
                     normed_voigt_transformation, matrix)


def stress_voigt_order_4(matrix):
    """Returns the stress voigt notation of a 3x3x3x3 matrix."""
    return np.einsum("ikl, jmn, klmn", stress_voigt_transformation,
                     stress_voigt_transformation, matrix)


def strain_voigt_order_4(matrix):
    """Returns the strain voigt notation of a 3x3x3x3 matrix."""
    return np.einsum("ikl, jmn, klmn", strain_voigt_transformation,
                     strain_voigt_transformation, matrix)

# finally, write functions which work for input of order 2 _and_ 4, using your
# previous functions.


def normed_voigt(matrix):
    """Returns the normed voigt notation of a 3x3 or 3x3x3x3 matrix."""
    if matrix.ndim == 2:
        return normed_voigt_order_2(matrix)
    elif matrix.ndim == 4:
        return normed_voigt_order_4(matrix)
    else:
        raise ValueError("Invalid matrix order for voigt notation.")


def stress_voigt(matrix):
    """Returns the stress voigt notation of a 3x3 or 3x3x3x3 matrix."""
    if matrix.ndim == 2:
        return stress_voigt_order_2(matrix)
    elif matrix.ndim == 4:
        return stress_voigt_order_4(matrix)
    else:
        raise ValueError("Invalid matrix order for voigt notation.")


def strain_voigt(matrix):
    """Returns the strain voigt notation of a 3x3 or 3x3x3x3 matrix."""
    if matrix.ndim == 2:
        return strain_voigt_order_2(matrix)
    elif matrix.ndim == 4:
        return strain_voigt_order_4(matrix)
    else:
        raise ValueError("Invalid matrix order for voigt notation.")

# If you wish, you may check your work yourself at any point, by using the
# provided unit tests or self-written tests.

#### EXERCISE 3 (bonus) ####


def rotation(phi, theta, psi):
    """Calculates a 3x3 rotation matrix from three euler angles phi1, theta, 
    phi2 in radiants.
    """
    cos = np.cos
    sin = np.sin
    return np.array([[cos(phi)*cos(psi) - sin(phi)*cos(theta)*sin(psi),
                      -cos(phi)*sin(psi) - sin(phi)*cos(theta)*cos(psi),
                      sin(phi)*sin(theta)],
                     [sin(phi)*cos(psi) + cos(phi)*cos(theta)*sin(psi),
                      -sin(phi)*sin(psi) + cos(phi)*cos(theta)*cos(psi),
                      -cos(phi)*sin(theta)],
                     [sin(theta)*sin(psi), sin(theta)*cos(psi), cos(theta)]])

# hint: np.einsum is very close to our index notation, e.g. A_ij b_j is equal
# to np.einsum("ij,j", A, b)


def rotation_nv(phi, theta, psi):
    """Calculates a 6x6 rotation matrix to be applied to tensors in normed 
    voigt notation.
    """
    rot = rotation(phi, theta, psi)
    rot_box_rot_T = np.einsum("ik, lj-> ijkl", rot, rot)
    return normed_voigt(rot_box_rot_T)
