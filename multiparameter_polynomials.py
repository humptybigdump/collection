import numpy as np
from polynomials import Polynomial

class MultiparameterPolynomial():
    def __init__(self, coefficients):
        self.coefficients = np.array(coefficients)
        self.d = self.coefficients.ndim
        self.shape = np.array(self.coefficients.shape) 
        self.n = self.shape - np.ones(self.d, dtype=int)
    
    def _x_grid(self, x):
        x = np.array(x)
        powers_x = [x[i, np.newaxis]**np.arange(self.n[i], -1, -1)
                    for i in range(self.d)]
        x_grids = np.meshgrid(*powers_x, indexing="ij")
        full_grid = np.product(x_grids, axis=0)
        return full_grid
        
    def value(self, x):
        full_grid = self._x_grid(x)
        return np.sum(self.coefficients*full_grid)
    
    def grad(self, x=None, axis=None):
        if axis is None:
            leftgrad = np.array([self.grad(x=x, axis=i) for i in range(self.d)])
            rightgrad = np.moveaxis(leftgrad, 0, -1)
            return rightgrad
        if self.n[axis] == 0:
            new_coefficients = self.coefficients
        else:
            new_coefficients = self.coefficients.take(range(0, self.n[axis]), axis=axis)
            einsumstr = "ijkl"[:self.d] + ", " + "ijkl"[axis] + "->"+"ijkl"[:self.d]
            new_coefficients = np.einsum(einsumstr, new_coefficients, np.arange(self.n[axis],0,-1))
        derivative = MultiparameterPolynomial(new_coefficients)
        if x is None:
            return derivative
        return derivative.value(x)
    
    def integrate(self, low=None, high=None, axis=None):
        if axis is None:
            reduced = self
            for i in reversed(range(self.d)):
                if low is None:
                    reduced = reduced.integrate(axis=i)
                else:
                    reduced = reduced.integrate(low=low[i], high=high[i], axis=i)
                    
            return reduced
        new_coeffs = np.zeros(self.shape + np.ones(self.d, dtype=int))
        padding = [(0, 0)]*self.d
        padding[axis] = (0,1)
        einsumstr = "ijkl"[:self.d] + ", " + "ijkl"[axis] + "->"+"ijkl"[:self.d]
        new_coeffs = np.einsum(einsumstr, self.coefficients, 1/np.arange(self.n[axis]+1,0,-1))
        new_coeffs = np.pad(new_coeffs, padding)
        axis_antiderivative = MultiparameterPolynomial(new_coeffs)
        if low is None or high is None:
            return axis_antiderivative
        reduced_coeffs = []
        for x in (low, high):
            powers_x = x**np.arange(self.n[axis]+1, -1, -1)
            einsumstr = "ijkl"[:self.d] + ","+"ijkl"[axis]
            reduced_coeffs.append(np.einsum(einsumstr, new_coeffs, powers_x))
        new_coeffs = reduced_coeffs[1] - reduced_coeffs[0]
        if self.d > 1:
            result = MultiparameterPolynomial(new_coeffs)
            return result
        return new_coeffs
        
    def __mul__(self, other):
        assert isinstance(other, MultiparameterPolynomial)
        assert self.d == other.d
        assert self.d <= 4
        str1 = "ikmo"[:self.d]
        str2 = "jlnp"[:self.d]
        einsumstr = str1+", "+str2
        coeff_prod = np.einsum(einsumstr, self.coefficients,
                               other.coefficients)
        for dim in reversed(range(self.d)):
            axis0 = 2*dim
            axis1 = 2*dim + 1
            extents = (coeff_prod.shape[axis0], coeff_prod.shape[axis1])
            rev_coeff_prod = np.flip(coeff_prod, axis=axis1)
            antidiags = [np.diagonal(rev_coeff_prod, offset, axis1=axis0, axis2=axis1) for offset
                         in range(extents[1]-1, -extents[0], -1)]
            coeff_prod = np.array([np.sum(d, axis=-1) for d in antidiags])
            coeff_prod = np.moveaxis(coeff_prod, 0, axis0)
        return MultiparameterPolynomial(coeff_prod)
    
    def __add__(self, other):
        assert isinstance(other, MultiparameterPolynomial)
        assert self.d == other.d
        new_shape = np.max(self.shape, other.shape)
        new_coeffs = np.zeros(new_shape)
        drange = range(self.d)
        selfslice = tuple([np.s_[new_shape[i]-self.shape[i]:] for i in drange])
        new_coeffs[selfslice] += self.coefficients
        otherslice = tuple([np.s_[new_shape[i]-other.shape[i]:] for i in drange])
        new_coeffs[otherslice] += other.coefficients
        return MultiparameterPolynomial(new_coeffs)
    
    def __repr__(self):
        return self.__str__()
    
    def __str__(self):
        x_strs = [f"x{i}" for i in range(self.d)]
        str_ranges = [[f"*{x_strs[i]}^{p}" for p in range(self.n[i], 0, -1)]+[""]
                      for i in range(self.d)]
        str_ranges = np.array(str_ranges, dtype=np.object)
        str_grids = np.meshgrid(*str_ranges, indexing="ij")
        str_coeffs = self.coefficients.astype(str).astype(np.object)
        for i in range(self.d):
            str_coeffs += str_grids[i]
        return " + ".join(str_coeffs.flatten())
    
    @classmethod
    def from_fit(cls, xs, ys, coeff_shape):
        x_shape = xs[0].shape
        for x in xs[1:]:
            assert x.shape == x_shape
        assert len(xs) == len(ys)
        assert np.product(coeff_shape) == len(xs)
        multi_1 = MultiparameterPolynomial(np.ones(coeff_shape))
        matrix = np.zeros((len(xs), len(xs)))
        for i, x in enumerate(xs):
            matrix[i, :] = multi_1._x_grid(x).flatten()
        coeffs = np.linalg.solve(matrix, ys)
        return cls(coeffs.reshape(coeff_shape))
    
    @classmethod
    def from_polynomials(cls, *polynomials):
        coeffs = polynomials[0].coefficients
        for p in polynomials[1:]:
            coeffs = np.tensordot(coeffs, p, axes=0)
        return cls(coeffs)
    
    @classmethod
    def zero(cls, dim):
        coeff_shape = (1,)*dim
        return cls(np.zeros(coeff_shape))


def test_against_polynomials():
    coeffs = [2, 3, 4]

    ref_p = Polynomial(coeffs)
    test_mp = MultiparameterPolynomial.from_polynomials(ref_p)
    assert np.allclose(test_mp.coefficients, coeffs)
    assert test_mp.d == 1
    assert test_mp.shape == (3,)
    
    assert np.allclose(test_mp.grad(axis=0).coefficients, ref_p.diff().coefficients)
    assert np.allclose(test_mp.grad()[0].coefficients, ref_p.diff().coefficients)
    
    assert np.allclose(test_mp.integrate().coefficients, ref_p.integrate().coefficients)
    
    coeffs2 = [1, 5, 4]
    ref_p2 = Polynomial(coeffs2)
    test_mp2 = MultiparameterPolynomial.from_polynomials(ref_p2)
    print(test_mp*test_mp2)
    print(ref_p*ref_p2)
    assert np.allclose((test_mp*test_mp2).coefficients, (ref_p*ref_p2).coefficients)


def test_x_grid():
    mp = MultiparameterPolynomial(np.ones((3, 2)))
    assert np.all(mp._x_grid([2, 3]) == np.array([[12, 4], [6, 2], [3, 1]]))
    

def test_grad():
    mp = MultiparameterPolynomial(np.array([[4,3], [2, 1]]))
    grad = mp.grad()
    assert grad.shape == (2,)
    grad0 = mp.grad(axis=0)
    assert np.allclose(grad0.coefficients, [[4, 3]])
    assert np.allclose(grad0.shape, (1,2))
    grad1 = mp.grad(axis=1)
    assert np.allclose(grad1.coefficients, [[4], [2]])
    assert np.allclose(grad1.shape, (2,1))

def test_integrate():
    mp = MultiparameterPolynomial(np.array([[4,3], [2, 1]]))
    integral = mp.integrate()
    assert np.allclose(integral.coefficients, np.array([[1, 1.5, 0], [1, 1, 0], [0, 0, 0]]))
    indef_integral_1 = mp.integrate(axis=0)
    assert np.allclose(indef_integral_1.coefficients, np.array([[2, 1.5], [2, 1], [0, 0]]))
    integral_1 = mp.integrate(-1, 1, axis=0)
    assert np.allclose(integral_1.coefficients, np.array([4, 2]))
    integral_2 = mp.integrate(-1, 1, axis=1)
    assert np.allclose(integral_2.coefficients, np.array([6, 2]))
    integral_12 = integral_2.integrate(-1, 1, axis=0)
    assert np.isclose(integral_12, 4)
    assert np.isclose(integral_12, mp.integrate([-1, -1], [1, 1]))
    
if __name__ == "__main__":
    test_integrate()