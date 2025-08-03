#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Jan  3 09:43:19 2022

@author: max
"""
import numpy as np
from ex_10 import AffineElement
from ex_04 import stiffness
from ex_08 import BoundaryValueProblem
from multiparameter_polynomials import MultiparameterPolynomial
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

### EXERCISE 1 ###
# Calculate any of the base functions of the 2D linear reference element.
# Use the MultiParameterPolynomial class and in particular its fit function.
# Code to plot in 3D is given.

def plot_linear_polynomial():
    xs = np.array([[ 1, 1],
                   [ 1,-1],
                   [-1,-1],
                   [-1, 1]])
    ys = np.array([1, 0, 0, 0])
    mp1 = MultiparameterPolynomial.from_fit(xs, ys, (2,2))
    plot_multiparameter_polynomial(mp1)

def plot_multiparameter_polynomial(mp):    
    plot_res = 20
    fig = plt.figure()
    ax = fig.add_subplot(111, projection="3d")
    x = np.linspace(-1, 1, plot_res)
    y = np.linspace(-1, 1, plot_res)
    grid_x, grid_y = np.meshgrid(x, y, indexing="ij")
    grid_z = np.zeros((plot_res, plot_res))
    for i in range(plot_res):
        for j in range(plot_res):
            grid_z[i,j] = mp.value([grid_x[i, j], grid_y[i, j]])
    ax.plot_surface(grid_x, grid_y, grid_z)
    plt.show()

if __name__ == "__main__":
    plot_linear_polynomial()

# Just as a bonus, here's a quadratic base function:

def plot_quad_polynomial():
    xs = np.meshgrid([-1, 0, 1], [-1, 0, 1], indexing="ij")
    xs = np.moveaxis(xs, 0, -1).reshape((9, 2))
    ys = np.zeros(9)
    ys[1] = 1
    mp1 = MultiparameterPolynomial.from_fit(xs, ys, (3,3))
    plot_multiparameter_polynomial(mp1)

if __name__ == "__main__":
    plot_quad_polynomial()
    

### EXERCISE 2 ###

# Implement the 2D reference element

class ReferenceElement2D():
    """Reference element of arbitrary order for 1D FEM. Always [-1, 1].

        members:
    dimension: 2
    n_nodes_1D: number of nodes in one direction (i.e. 3 for a quad element)
    n_nodes: overall number of nodes: n_nodes_1D**2
    dof: number of degrees of freedom: 2*n_nodes
    nodes: positions of nodes ([[-1,-1], [-1, 1], [ 1,-1], [ 1, 1]] for linEl)
    N: array of base functions (shape (2, dof))
    N_grad_squared_integral: relevant integral to calculate affine element K
        shape (2, dof, 2, 2, dof, 2)
    """
    def __init__(self, n_nodes_1D):
        self.dimension = 2
        nodes_x = np.linspace(-1, 1, num=n_nodes_1D)
        nodes_y = np.linspace(-1, 1, num=n_nodes_1D)
        self.n_nodes_1D = n_nodes_1D
        self.n_nodes = n_nodes_1D**2
        self.dof = 2*self.n_nodes
        nodes = np.meshgrid(nodes_x, nodes_y, indexing="ij")
        nodes = np.moveaxis(nodes, 0, -1).reshape((-1, 2))
        self.nodes = nodes
        
        self.n_polynomials = [MultiparameterPolynomial.from_fit(nodes, ys, (n_nodes_1D, n_nodes_1D))
                              for ys in np.eye(self.n_nodes)]
        self.N_grad_squared_integral = self.get_N_grad_squared_integral()
        
    def get_N(self):
        zero_polynomial = MultiparameterPolynomial.zero(2)
        return np.array([self.n_polynomials+[zero_polynomial]*self.n_nodes,
                        [zero_polynomial]*self.n_nodes+self.n_polynomials])
    
    def get_N_grad(self):
        N = self.get_N()
        N_grad = np.zeros(N.shape + (2,), dtype=object)
        N_grad = np.array([p.grad() for p in N.flatten()])
        N_grad = N_grad.reshape(2, self.dof, 2)
        return N_grad
        
    def get_N_grad_squared_integral(self):
        N_grad = self.get_N_grad()
        N_grad_squared = np.tensordot(N_grad, N_grad, axes=0)
        flat_integral = np.zeros(N_grad_squared.size)
        for i, p in enumerate(N_grad_squared.flatten()):
            flat_integral[i] = p.integrate(low=[-1, -1], high=[1, 1])
        return flat_integral.reshape(N_grad_squared.shape)


def single_element_2D_tension():
    # With a single element, the system matrix is the element stiffness matrix
    # -- no assembly required.
    refLin2D = ReferenceElement2D(2)
    lin_elem = AffineElement([0, 1, 2, 3], refLin2D, np.array([0, 0]), np.array([[0.5, 0], [0, 0.5]]),
                             stiffness, 
                             thickness=1)
    stiffness_matrix = lin_elem.K
    
    # Nodes [0,1] fixed in x direction, also nodes [0, 2] fixed in y direction 
    fixed_u_system_coords = np.array([0,1,4,6])
    fixed_u = np.zeros(4)
    
    # Nodes [2,3] with force applied in x direction
    total_force = 2
    F_system_coords = np.array([2, 3])
    F = total_force/2*np.array([1, 1])
    
    x0 = np.zeros(refLin2D.dof)
    dirichlet = list(zip(fixed_u_system_coords, fixed_u))
    neumann = list(zip(F_system_coords, F))
    
    single_element_tension = BoundaryValueProblem(x0, dirichlet, neumann)    
    
    def assemble_full_matrix():
        return stiffness_matrix
    
    # this is called monkey patching... don't try it at home
    single_element_tension.assemble_full_matrix = assemble_full_matrix
    
    u = single_element_tension.solve()
    u_0 = u[0:4]
    u_1 = u[4:8]
    u_vectors = np.array([u_0, u_1]).T
    F = stiffness_matrix@u
    F_vectors = np.array([F[0:4], F[4:8]]).T
    #check that the fixed points are fixed
    assert np.allclose(u_vectors[0], np.zeros(2))
    assert np.isclose(u_vectors[1][0], 0)
    assert np.isclose(u_vectors[2][1], 0)
    
    #check that both non-fixed points acted similarly
    assert np.isclose(u_vectors[2][0], u_vectors[3][0])
    assert np.allclose(F_vectors[:, 1], 0)
    
    # eps = delta_l / l
    # sigma = F/A[0]
    eps = u_vectors[2][0]/0.5
    sigma = total_force/0.5
    
    C = sigma/eps
    print(f"Given E: 210000")
    print(f"Theoretical plane strain stiffness: around 230769.2")
    print(f"Measured plane strain stiffness: {C}")

single_element_2D_tension()