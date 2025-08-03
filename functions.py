import numpy as np

class Polynomial():
    """1D polynomials.

    Members:
    n: highest exponent in the polynomial expression
    coefficients: np array of polynomial coefficients, starting with x^n

    Methods:
    value: evaluate polynomial(x)
    diff: return the derivative. Optionally, return f'(x)
    integrate: return the indefinite integral F. Optionally, definite integral.
    from_fit: given a sequence of xs and a sequence of ys, perfectly fit a
    polynomial of the highest order possible.
    """
    def __init__(self, coefficients):
        self.n = len(coefficients)-1
        self.coefficients = coefficients
        
    def value(self, x):
        return np.sum(self.coefficients*x**np.arange(self.n, -1, -1))
    
    def diff(self, x=None):
        derivative = Polynomial(self.coefficients[:-1]*np.arange(self.n, 0, -1))
        if x is None:
            return derivative
        return derivative.value(x)
    
    def integrate(self, low=None, high=None):
        new_coeffs = np.zeros(self.n+2)
        new_coeffs[:-1] = self.coefficients/np.arange(self.n+1, 0, -1)
        antiderivative = Polynomial(new_coeffs)
        if low is None or high is None:
            return antiderivative
        return antiderivative.value(high) - antiderivative.value(low)
        
    def __mul__(self, other):
        assert isinstance(other, Polynomial)
        coeff_prod = np.tensordot(self.coefficients, other.coefficients, axes=0)
        antidiags = [np.diag(coeff_prod[:, ::-1], offset) for offset
                 in range(len(other.coefficients)-1, -len(self.coefficients), -1)]
        new_coeffs = np.array([np.sum(d) for d in antidiags])
        return Polynomial(new_coeffs)
    
    def __add__(self, other):
        assert isinstance(other, Polynomial)
        new_n = max(self.n, other.n)
        new_coeffs = np.zeros(new_n+1)
        new_coeffs[new_n-self.n:] += self.coefficients
        new_coeffs[new_n-other.n:] += other.coefficients
        return Polynomial(new_coeffs)
    
    @classmethod
    def from_fit(cls, xs, ys):
        n = len(xs)-1
        assert len(ys) == n+1
        matrix = np.zeros((n+1, n+1))
        for i, x in enumerate(xs):
            matrix[i, :] = x**np.arange(n, -1, -1)
        coeffs = np.linalg.solve(matrix, ys)
        return cls(coeffs)
    
    def __str__(self):
        x_str="x"
        return " ".join(f"{c}*{x_str}^{n}" for c, n in zip(self.coefficients,
                                                     np.arange(self.n, -1, -1)))


class Cos():
    """Class describing cosine functions of the form 
    y=b*cos(a*x).
    Members:
    a
    Methods:
    __init__: receives one float parameter a
    value: value of the function at location x
    diff: value of the derivation of the function at location x
    """
    def __init__(self,a,b):
        self.a = a
        self.b = b
    def value(self, x):
        return self.b*np.cos(self.a*x)
    def diff(self, x):
        derivative = Sin(self.a,-self.a*self.b)
        if x is None:
            return derivative
        return derivative.value(x)
    def integrate(self,low=None,high=None):
        antiderivative = Sin(self.a,self.b/self.a)
        if low is None or high is None:
            return antiderivative
        return antiderivative.value(high) - antiderivative.value(low)


class Sin():
    """Class describing sinus functions of the form 
    y=b*sin(a*x).
    Members:
    a
    Methods:
    __init__: receives one float parameter a
    value: value of the function at location x
    diff: value of the derivation of the function at location x
    """
    def __init__(self,a,b):
        self.a = a
        self.b = b
    def value(self, x):
        return self.b*np.sin(self.a*x)
    def diff(self, x):
        derivative = Cos(self.a,self.a*self.b)
        if x is None:
            return derivative
        return derivative.value(x)
    def integrate(self,low=None,high=None):
        antiderivative = Cos(self.a,-self.b/self.a)
        if low is None or high is None:
            return antiderivative
        return antiderivative.value(high) - antiderivative.value(low)


class Exp():
    """Class describing exponential functions of the form 
    y=b*exp(a*x).
    Members:
    a
    Methods:
    __init__: receives one float parameter a
    value: value of the function at location x
    diff: value of the derivation of the function at location x
    """
    def __init__(self,a,b):
        self.a = a
        self.b = b
    def value(self, x):
        return self.b*np.exp(self.a*x)
    def diff(self, x):
        derivative = Exp(self.a,self.a*self.b)
        if x is None:
            return derivative
        return derivative.value(x)
    def integrate(self,low=None,high=None):
        antiderivative = Exp(self.a,self.b/self.a)
        if low is None or high is None:
            return antiderivative
        return antiderivative.value(high) - antiderivative.value(low)