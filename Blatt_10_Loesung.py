import numpy as np
import matplotlib.pyplot as plt
from polynomials import Polynomial
from ex_09 import LinElement1D, FEMGrid1D
from ex_04 import stiffness


### EXERCISE 1 ###

# Define a function to create and plot the base functions for a 1D FEM element
# of order n.

def plot_n_polynomials(n):
    """Fit and plot the base functions for a 1D FEM element of order n-1."""
    points_x = np.linspace(-1, 1, num=n)
    base_functions = [Polynomial.from_fit(points_x, ys)
                      for ys in np.eye(n)]
    x_range = np.linspace(-1, 1, 50)
    for i, p in enumerate(base_functions):
        values = [p.value(x) for x in x_range]
        plt.plot(x_range, values, label=f"p{i}")
    plt.show()

if __name__ == "__main__":
    plot_n_polynomials(4)

### EXERCISE 2 ###

# Implement the class ReferenceElement1D, which calculates int(N*N) for a 1D
# FEM element of order n over [-1, 1].

class ReferenceElement1D():
    """Reference element of arbitrary order for 1D FEM. Always [-1, 1].

        members:
    dimension: 1
    n_points: number of points
    N: array of base functions (shape n_points)
    N_grad_squared_integral: relevant integral to calculate affine element K
        (shape (1, n_points, 1, 1, n_points, 1), for compatibility with 2D FEM)
    """
    def __init__(self, n_points):
        self.dimension = 1
        self.n_points = n_points
        points_x = np.linspace(-1, 1, num=self.n_points)
        self.N = [Polynomial.from_fit(points_x, ys)
                  for ys in np.eye(self.n_points)]
        self.N_grad_squared_integral = self.get_N_grad_squared_integral()
        
    def get_N_grad_squared_integral(self):
        N_grad = np.array([p.diff() for p in self.N])
        N_grad = N_grad.reshape((1, self.n_points, 1))
        N_grad_squared = np.tensordot(N_grad, N_grad, axes=0)
        flat_integral = np.zeros(N_grad_squared.size)
        for i, p in enumerate(N_grad_squared.flatten()):
            flat_integral[i] = p.integrate(low=-1, high=1)
        return flat_integral.reshape(N_grad_squared.shape)

### EXERCISE 3 ### 

# Implement the class AffineElement which uses the integral from
# ReferenceElement1D with a particular affine transformation to calculate K.

class AffineElement():
    """Affine FEM element based on a reference element of arbitrary dimension.

        members:
    nodes: node indices of element nodes
    reference element: underlying element for [-1, 1]
    A: transformation matrix
    c: transformation offset
    stiffness: full tensorial 3x3x3x3 stiffness
    reduced stiffness: dimensionally reduced dxdxdxd stiffness
    dimension: dimension of the reference element
    K: stiffness matrix
    """
    def __init__(self, nodes, reference_element, c, A, stiffness,
                 area=1, thickness=1, reduced_in="strain"):
        self.A = A
        self.A_inv = np.linalg.inv(A)
        self.c = c
        self.reference_element = reference_element
        self.stiffness = stiffness
        self.dimension = self.reference_element.dimension
        if self.dimension == 3:
            self.reduced_stiffness = self.stiffness
        elif reduced_in == "strain":
            self.reduced_stiffness = self.stiffness[0:self.dimension,
                                                    0:self.dimension,
                                                    0:self.dimension,
                                                    0:self.dimension]
        elif reduced_in == "stress":
            """This is a bit difficult to implement here because we have not
            programmed a function to get from the normed voigt notation to a 
            full tensorial form in ex. 03.
            Instead, I'll describe the algorithm: 
                1. convert the stiffness to normed voigt notation
                2. invert to get the normed voigt compliance
                3. reduce either in the normed voigt form or by converting
                   to the full tensorial form, then reducing, then converting
                   back
                4. invert the normed voigt reduced compliance to get the red.
                   stiffness
                5. convert back to full tensorial form.
            For 1D, it's simpler, because there's no difference between
            reduced normed Voigt and reduced full tensorial form."""
            raise NotImplementedError()
        else:
            raise ValueError(f"Invalid reduced_in value: {reduced_in}")
        self.nodes = nodes
        if self.dimension == 1:
            self.rest_integral = area
        elif self.dimension == 2:
            self.rest_integral = thickness
        else:
            self.rest_integral = 1
        self.K = self.get_K()

    def get_K(self):
        integral = self.reference_element.N_grad_squared_integral
        result = np.einsum("macibd, cn, dj, ijmn",
                           integral, self.A_inv, self.A_inv, 
                           self.reduced_stiffness)
        result *= self.rest_integral*np.linalg.det(self.A)
        return result
    

if __name__ == "__main__":
    ref_K = LinElement1D(210000, [0,1], [2,3]).K
    
    refLin1D = ReferenceElement1D(2)
    lin_elem = AffineElement([0, 1], refLin1D, 2.5, np.array([[0.5]]), stiffness)
    
    refQuad1D = ReferenceElement1D(3)
    quad_elem = AffineElement([0, 1, 2], refQuad1D, 2.5, np.array([[0.5]]), stiffness)
    
    ex9_elem = LinElement1D(210000, [0,1], [2,3])
    ex9_grid = FEMGrid1D([2, 3], [ex9_elem], [(0,0)], [(1,1)])
    print(ex9_grid.solve_for_pos())
    
    lin_grid = FEMGrid1D([2, 3], [lin_elem], [(0, 0)], [(1, 1)])
    print(lin_grid.solve_for_pos())

    quad_grid = FEMGrid1D([2, 2.5, 3], [quad_elem], [(0, 0)], [(2, 1)])
    print(quad_grid.solve_for_pos())
    