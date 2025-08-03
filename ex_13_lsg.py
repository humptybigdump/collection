from functions import*
from polynomials import*
from multiparameter_polynomials import*
import matplotlib.pyplot as plt

###############################################################################

#Ex. 1 a): Calculate Newton-Cotes integration points
def get_nc_points(n):
    """
    Gives n integration points for Newton-Cotes quadrature in closed form
    """
    return np.linspace(-1,1,num=n)

#Ex. 1 b): Calculate Gauss integration points
def get_gauss_points(n):
    """
    Gives n integration points for Gauss quadrature
    """
    p = Polynomial(np.array([1]))
    q = Polynomial(np.array([1,0,-1]))
    for i in range(n):
        p = p.__mul__(q)
    for i in range(n):
        p = p.diff()
    return np.sort(np.roots(p.coefficients))

#Ex. 1 c): Calculate weights
def get_weights(int_points):
    """
    Calculates weights for given 1D-array of integration points
    """
    w = np.zeros(len(int_points))
    for i in range(len(int_points)):
        lagrange = Polynomial(np.array([1]))
        for j in range(len(int_points)):
            if not j==i:
                coeff = np.array([1,-int_points[j]])/(int_points[i]-int_points[j])
                lagrange = lagrange.__mul__(Polynomial(coeff))
        w[i] = lagrange.integrate(low=-1,high=1)
    return w

#Ex. 1 d): Transform integration points
def transform_int_points(int_points,low,high):
    """
    Transforms integration points from reference element to an element definded
    by lower bound (low) and upper bound (high)
    """
    return (high-low)/2*int_points + (high+low)/2

#Ex. 1 e): Calculate weighted sum (numeric integration)
def get_integral_1D(func,int_points,w,low,high):
    """
    Calculates the weighted sum for a function object
    int_points: 1D-array of integration points
    w: 1D-array of weights
    low: lower bound
    high: upper bound
    """
    values = np.zeros(len(int_points))
    for i in range(len(values)):
        values[i] = func.value(int_points[i])
    return (high-low)/2*np.sum(w*values)

#No additions necessary
def integrate_1D(func,low,high,n,method='gauss'):
    """
    Calculates the numeric integral for a function object
    low: lower bound of integration
    high: upper bound of integration
    n: number of integration points
    method: 'newton-cotes' or 'gauss'
    """
    if method=='gauss':
        int_points = get_gauss_points(n)
    elif method=='newton-cotes':
        int_points = get_nc_points(n)
    w = get_weights(int_points)
    int_points = transform_int_points(int_points,low,high)
    return get_integral_1D(func,int_points,w,low,high)

#Ex. 1 f): Test 1D numeric integration
if __name__ == "__main__":
    print("1D")
    
    f = Polynomial([3,2,4,5,3,4,5,3,3,2]) # Function object: Polynomial
    e = Exp(0.3,2) # Function object: e = b*exp(a*x)
    c = Cos(3,1.7) # Function object: c = b*cos(a*x)
    a = -0.5 # lower bound
    b = 0.3 # upper bound
    
    # Analytical solutions
    f_int_analytical = f.integrate(a,b)
    e_int_analytical = e.integrate(a,b)
    c_int_analytical = c.integrate(a,b)
    
    nc = []
    gauss = []
    num_points = []
    func = c
    analytical = c_int_analytical
    for n in range(2,14):
        num_points.append(n)
        nc.append(abs((integrate_1D(func,a,b,n,method='newton-cotes')-analytical)/analytical))
        gauss.append(abs((integrate_1D(func,a,b,n,method='gauss')-analytical)/analytical))
    plt.semilogy(num_points,gauss,linestyle='solid',marker='o',color='blue',label='Gauss')
    plt.semilogy(num_points,nc,linestyle='solid',marker='o',color='red',label='Newton Cotes')
    plt.legend()
    plt.xlabel('Anzahl Integrationspunkte')
    plt.ylabel('Relativer Fehler')
    plt.grid()
    
###############################################################################

#Ex. 2 a): Transform 1D integration points and determine the 2 integration points
def transform_int_points_2D(int_points,low,high):
    """
    Calculates integration points on a rectangular element definded
    by lower bounds (low) and upper bounds (high) based on 1D integration points 
    """
    n = len(int_points)
    int_points_2D = np.zeros([n**2,2])
    int_points_x = transform_int_points(int_points,low[0],high[0])
    int_points_y = transform_int_points(int_points,low[1],high[1])
    k = 0
    for i in range(n):
        for j in range(n):
            int_points_2D[k] = np.array([int_points_x[i],int_points_y[j]])
            k += 1
    return int_points_2D

#Ex. 2 b): Calculate weights for 2D case
def get_weights_2D(int_points):
    """
    Calculates weights for 2D integration points based on 1D integration points
    """
    w = get_weights(int_points)
    w = np.outer(w,w)
    return w.reshape((len(int_points)**2))

#Ex. 2 c) Calculate weighted sum for 2D case
def get_integral_2D(func,int_points,w,low,high):
    """
    Calculates the weighted sum for a (scalar) multiparameter function object
    int_points: (n**2 x 2)-array of integration points
    w: 1D-array of weights with dim=n**2
    low: lower bounds of integration, 1D-array with dim=2
    high: upper bounds of integration, 1D-array with dim=2
    """
    values = np.zeros(len(int_points))
    for i in range(len(values)):
        values[i] = func.value(int_points[i])
    return (high[0]-low[0])*(high[1]-low[1])/4*np.sum(w*values)

#No additions necessary
def integrate_2D(func,low,high,n,method='gauss'):
    """
    Calculates the numeric integral for a (scalar) multiparameter function object
    low: lower bounds of integration, 1D-array with dim=2
    high: upper bounds of integration, 1D-array with dim=2
    n: number of integration points per dimension
    method: 'newton-cotes' or 'gauss'
    """
    if method=='gauss':
        int_points = get_gauss_points(n)
    elif method=='newton-cotes':
        int_points = get_nc_points(n)
    w = get_weights_2D(int_points)
    int_points = transform_int_points_2D(int_points,low,high)
    return get_integral_2D(func,int_points,w,low,high)

#Ex. 2 d): Test 2D numeric integration
if __name__ == "__main__":
    print("")
    print("2D")
    
    coeff = np.array([[1,2,0.5,2],[3,0,1,2.5],[3,4,1,2],[1,4,5,2]])
    P = MultiparameterPolynomial(coeff) # Function object: multiparameter Polynomial
    print(P.integrate(low=[-2,0],high=[1,5]))
    print(integrate_2D(P,[-2,0],[1,5],1,method='gauss'))
