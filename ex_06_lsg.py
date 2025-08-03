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
    if x0 is None:
        x = np.zeros(len(b))
    else:
        x = x0
    g = A@x-b
    d = -g.copy()
    res = np.linalg.norm(g)
    r = [res]
    k = 1
    while res>tol and k<maxit:
        res_old = res
        z = A@d 
        alpha = np.linalg.norm(g)**2/np.dot(d, z)
        x += alpha*d
        g += alpha*z
        res = np.linalg.norm(g)
        r.append(res)
        beta = res**2/res_old**2
        d = -g + beta*d 
        k += 1
    if k == maxit:
        raise Exception("Maximum iterations exceeded.")
    return x, r 


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
    d = 100
    tol = 10**(-10)
    maxit = 1000
    A,b = Test_LES(d)
    x,res = solve_CG(A,b,tol=tol)
    k = np.arange(1,len(res)+1)
    fig,ax = plt.subplots()
    ax.semilogy([0,len(k)+1],[tol,tol],color='red')
    ax.semilogy(k,res,linestyle='-',marker='o',color='blue',markersize=4,linewidth=1)
    ax.set_xlabel('Iteration')
    ax.set_ylabel('Resdiual')
    ax.set_xlim((0,len(k)+1))
    ax.grid()