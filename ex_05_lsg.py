import numpy as np
import scipy.sparse
import timeit
import matplotlib.pyplot as plt
from ex_03 import sym

#### EXERCISE 1 ####


def is_posdef_sym(matrix):
    """Checks whether a matrix is positive-definite and symmetric."""
    is_sym = np.allclose(sym(matrix), matrix)
    e_values, e_vectors = np.linalg.eig(matrix)
    is_posdef = np.all(e_values > 0)
    return is_sym and is_posdef

def cholesky(matrix):
    """Returns L according to the Cholesky decomposition of matrix."""
    L = np.zeros(matrix.shape)
    for i in range(matrix.shape[0]):
        for j in range(i):
            L[i, j] = 1/L[j,j]*(matrix[i, j]-L[i, :i]@L[j, :i])
        L[i, i] = np.sqrt(matrix[i, i] - L[i, :i]@L[i, :i])
    return L

def solve_cholesky(matrix, rhs):
    """Solves the equation system matrix@x=rhs using the Cholesky
    decomposition."""
    L = cholesky(matrix)
    b = np.zeros(rhs.shape)
    for i in range(len(rhs)):
        b[i] = (rhs[i]-L[i, :i]@b[:i])/L[i, i]
    x = np.zeros(rhs.shape)
    for i in reversed(range(len(rhs))):
        x[i] = (b[i]-L[i+1:, i]@x[i+1:])/L[i, i]
    return x


#### EXERCISE 2 ####
def np_matrix(d):
    """Creates A as a numpy array."""
    A = np.zeros((d, d))
    for i in range(1, d-1):
        A[i,i] = 2
        A[i,i+1] = -1
        A[i,i-1] = -1
    A[0,0] = -1
    A[0,1] = 1
    A[d-1, d-2] = -1
    A[d-1, d-1] = 1
    return A

def coo_matrix(d):
    """Creates A as a coo matrix."""
    n = 3*d-2
    i_vector = np.zeros(n, dtype=int)
    j_vector = np.zeros(n, dtype=int)
    v = np.zeros(n)
    alpha = 0

    def assign(i_alpha, j_alpha, v_alpha):
        i_vector[alpha] = i_alpha
        j_vector[alpha] = j_alpha
        v[alpha] = v_alpha
        return alpha + 1

    for i in range(1, d-1):
        alpha = assign(i, i, 2)
        alpha = assign(i, i-1, -1)
        alpha = assign(i, i+1, -1)
    alpha = assign(0, 0, -1)
    alpha = assign(0, 1, 1)
    alpha = assign(d-1, d-2, -1)
    alpha = assign(d-1, d-1, 1)

    A = scipy.sparse.coo_matrix((v, (i_vector, j_vector)))
    return A

def csr_matrix(d):
    """Creates A as a coo matrix."""
    return coo_matrix(d).tocsr()

def time_matrix_product(A, x):
    """Performs a matrix vector product and returns elapsed time in seconds."""
    start = timeit.default_timer()
    y = A@x
    stop = timeit.default_timer()
    return stop-start

if __name__ == "__main__":
    d = 2**np.arange(1,17)
    times = {"np": np.zeros(len(d)),
             "coo": np.zeros(len(d)),
             "csr": np.zeros(len(d))}
    for i, d_i in enumerate(d):
        matrices = {"np": np_matrix(d_i), "coo": coo_matrix(d_i),
                    "csr": csr_matrix(d_i)}
        x = np.random.rand(d_i)
        for key, A in matrices.items():
            times[key][i] = time_matrix_product(A, x)
    for key in times:
        plt.plot(d, times[key], label=key)
    plt.legend()
    ax = plt.gca()
    ax.set_yscale("log")
    ax.set_xscale("log")
    ax.set_xlabel("matrix dimension d")
    ax.set_ylabel("time in s")
    plt.show()