import numpy as np
from scipy import linalg
from ex_03 import normed_voigt

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
    c_nv = normed_voigt(stiffness)
    sigma_nv = normed_voigt(stress)
    eps_solved = np.linalg.solve(c_nv, sigma_nv)
    eps_inverted = np.linalg.inv(c_nv)@sigma_nv
    assert np.allclose(eps_solved, eps_inverted)

# Similarly, compare np.linalg.solve with linalg.lu_factor (the LUP
# decomposition) and linalg.lu_solve
def test_LU():
    c_nv = normed_voigt(stiffness)
    sigma_nv = normed_voigt(stress)
    eps_solved = np.linalg.solve(c_nv, sigma_nv)
    lu, piv = linalg.lu_factor(c_nv)
    eps_lu = linalg.lu_solve((lu, piv), sigma_nv)
    assert np.allclose(eps_solved, eps_lu)


#### EXERCISE 2 ####
def LUP(matrix):
    """Returns L, U, P according to the LUP decomposition of matrix."""
    n = matrix.shape[0]
    LU = np.zeros(matrix.shape)
    P = np.eye(n)
    AP = matrix@P.T
    for i in range(n):
        AP = matrix@P.T
        for j in range(i):
            # np.sum(LU[i, :j]*LU[:j, j]))
            LU[i, j] = 1/LU[j, j]*(AP[i, j] - LU[i, :j]@LU[:j, j])
        for j in range(i, n):
            LU[i, j] = AP[i, j] - LU[i, :i]@LU[:i, j]
        k = np.argmax(np.abs(LU[i, i:n]))+i
        if np.abs(LU[i, k]) > np.abs(LU[i, i]):
            tmp = LU[:, i].copy()
            LU[:, i] = LU[:, k]
            LU[:, k] = tmp
            tmp = P[i, :].copy()
            P[i, :] = P[k, :]
            P[k, :] = tmp
    L = np.eye(n)+np.tril(LU, k=-1)
    U = np.triu(LU)
    return L, U, P

# Write a test for your function by reconstructing the matrix.


def test_LUP():
    c_nv = normed_voigt(stiffness)
    sigma_nv = normed_voigt(stress)
    l, u, p = LUP(c_nv)
    assert np.allclose(l@u@p, c_nv)


def solve_LUP(matrix, rhs):
    """Solves matrix@x = rhs using the LUP decomposition."""
    L, U, P = LUP(matrix)
    n = matrix.shape[0]
    z = np.zeros(n)
    for i in range(n):
        z[i] = rhs[i] - L[i, :i]@z[:i]
    v = np.zeros(n)
    for i in reversed(range(n)):
        v[i] = 1/U[i, i]*(z[i] - U[i, i+1:n]@v[i+1:n])
    x = P.T@v
    return x

# Write a test for your function by comparing with np.linalg.solve


def test_solve_LUP():
    c_nv = normed_voigt(stiffness)
    sigma_nv = normed_voigt(stress)
    eps_solved = np.linalg.solve(c_nv, sigma_nv)
    eps_lup = solve_LUP(c_nv, sigma_nv)
    assert np.allclose(eps_solved, eps_lup)


if __name__ == "__main__":
    np.set_printoptions(linewidth=np.inf)
    test_LUP()
