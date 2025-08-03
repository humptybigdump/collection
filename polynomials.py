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

def test_multiplication():
    p1 = Polynomial([1, 1, 1])
    p2 = Polynomial([2, 1])
    test_coeffs = (p1*p2).coefficients
    check_coeffs = np.array([2, 3, 3, 1])
    assert np.allclose(test_coeffs, check_coeffs)

def test_addition():
    p1 = Polynomial([1, 1, 1])
    p2 = Polynomial([2, 1])
    test_coeffs = (p1+p2).coefficients
    check_coeffs = np.array([1, 3, 2])
    assert np.allclose(test_coeffs, check_coeffs)

def test_fit():
    xs = [0, 1, 2, 3]
    ys = [3, 15, 7, 1]
    p = Polynomial.from_fit(xs, ys)
    assert len(p.coefficients) == len(xs)
    for i in range(len(xs)):
        assert np.isclose(p.value(xs[i]), ys[i])

def test_diff():
    p1 = Polynomial([4, 2, 3])
    test_coeffs = p1.diff().coefficients
    check_coeffs = np.array([8, 2])
    assert np.allclose(test_coeffs, check_coeffs)
    assert np.isclose(p1.diff().value(0), p1.diff(x=0))

def test_integrate():
    p1 = Polynomial([4, 2, 3])
    test_coeffs = p1.integrate().coefficients
    check_coeffs = np.array([4/3, 1, 3, 0])
    assert np.allclose(test_coeffs, check_coeffs)
    p2 = p1.integrate()
    assert np.isclose(p1.integrate(low=0, high=1), p2.value(1)-p2.value(0))