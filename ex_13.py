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
    pass

#Ex. 1 b): Calculate Gauss integration points
def get_gauss_points(n):
    """
    Gives n integration points for Gauss quadrature
    """
    pass

#Ex. 1 c): Calculate weights
def get_weights(int_points):
    """
    Calculates weights for given 1D-array of integration points
    """
    pass

#Ex. 1 d): Transform integration points
def transform_int_points(int_points,low,high):
    """
    Transforms integration points from reference element to an element definded
    by lower bound (low) and upper bound (high)
    """
    pass

#Ex. 1 e): Calculate weighted sum (numeric integration)
def get_integral_1D(func,int_points,w,low,high):
    """
    Calculates the weighted sum for a function object
    int_points: 1D-array of integration points
    w: 1D-array of weights
    low: lower bound
    high: upper bound
    """
    pass

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
    
    print(integrate_1D(f,a,b,3,method='gauss'))
    
###############################################################################

#Ex. 2 a): Transform 1D integration points and determine the 2 integration points
def transform_int_points_2D(int_points,low,high):
    """
    Calculates integration points on a rectangular element definded
    by lower bounds (low) and upper bounds (high) based on 1D integration points 
    """
    pass

#Ex. 2 b): Calculate weights for 2D case
def get_weights_2D(int_points):
    """
    Calculates weights for 2D integration points based on 1D integration points
    """
    pass

#Ex. 2 c) Calculate weighted sum for 2D case
def get_integral_2D(func,int_points,w,low,high):
    """
    Calculates the weighted sum for a (scalar) multiparameter function object
    int_points: (n**2 x 2)-array of integration points
    w: 1D-array of weights with dim=n**2
    low: lower bounds of integration, 1D-array with dim=2
    high: upper bounds of integration, 1D-array with dim=2
    """
    pass

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
    print(integrate_2D(P,[-2,0],[1,5],2,method='gauss'))