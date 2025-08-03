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
from ex_11 import ReferenceElement2D
from multiparameter_polynomials import MultiparameterPolynomial
import matplotlib.pyplot as plt
from matplotlib.path import Path
import matplotlib.patches as patches

### EXERCISE 1 ###
# The 2D FEM Problem

def affine_element_from_fit(reference_element, nodes, destination_points,
                            stiffness):
    assert reference_element.dimension == 2
    assert reference_element.n_nodes_1D == 2
    
    linear_equation_matrix = np.zeros((6,6))
    for nodes_chosen in [[0,1,2], [1,2,0], [2,0,1],
                         [2,1,0], [0,2,1], [1,0,2]]:
        for i in range(3):
            x_hat = reference_element.nodes[i]
            linear_equation_matrix[2*i, 0] = x_hat[0]
            linear_equation_matrix[2*i, 1] = x_hat[1]
            linear_equation_matrix[2*i, 4] = 1
            linear_equation_matrix[2*i+1, 2] = x_hat[0]
            linear_equation_matrix[2*i+1, 3] = x_hat[1]
            linear_equation_matrix[2*i+1, 5] = 1
        rhs = destination_points[nodes_chosen].flatten()
        sol = np.linalg.solve(linear_equation_matrix, rhs)
        A = np.zeros((2,2))
        A[0, :] = sol[0:2]
        A[1, :] = sol[2:4]
        c = sol[4:]
        x4 = A@reference_element.nodes[3] + c
        if np.linalg.det(A) > 0 and np.allclose(x4, destination_points[3]):
            break
    else:
        raise ValueError("No valid node combination was found.")
    assert np.allclose(x4, destination_points[3])
    return AffineElement(nodes, reference_element, c, A, stiffness)
    

class FEMGrid2D(BoundaryValueProblem):
    """2D FEM grid, to be used with linear elements.
    Members:
    node_positions: (n_nodes, 2) numpy array of initial x,y-values
    elements: list of elements (e.g. LinElement2D)
    Methods:
    __init__: Receives node_positions, reference element, displacements_x,
        displacements_y, forces_x and forces_y (the last four as lists of
                                                tuples of node, value)
        Calls super().__init__
    assemble_full_matrix: Assembles the full stiffness matrix from element
    stiffness matrices.
    solve_for_pos: Uses the boundary value problem solution for displacements
        to calculate displaced positions as an (n_nodes, 2) array.
    render: Generates and shows a plot of the deformed mesh.
    """
    def __init__(self, node_positions, elements,
                 displacements_x, displacements_y, forces_x, forces_y):
        super().__init__(x0, displacements, forces)

    def render(self, pos_result):
        codes = [
            Path.MOVETO,
            Path.LINETO,
            Path.LINETO,
            Path.LINETO,
            Path.CLOSEPOLY,
        ]
        ax = plt.gca()
        for element in self.elements:
            el_verts = [pos_result[n] for n in element.nodes]
            verts = [el_verts[i] for i in [0, 1, 2, 3, 0]]

            path = Path(verts, codes)
            patch = patches.PathPatch(path, facecolor="C0", lw=2)
            ax.add_patch(patch)
        plt.plot(pos_result[:,0], pos_result[:,1], "o", color="black")
        for index, node_pos in enumerate(pos_result):
            plt.text(node_pos[0], node_pos[1], str(index), ha="right", va="top")
        ax.axis('equal')
        plt.show()

    def assemble_full_matrix(self):
        pass

    def solve_for_pos(self):
        pass


if __name__ == "__main__":
    ref_el = ReferenceElement2D(2)
    
    def tension():
        x_range = np.linspace(0, 1, 2)
        x_grid, y_grid = np.meshgrid(x_range, x_range, indexing="xy")
        node_positions = np.array(list(zip(x_grid.flatten(), y_grid.flatten())))
        print(node_positions)
        nodes_per_elem = np.array([[0,1,3,2]])
        A = np.eye(2)
        c = np.zeros(2)
        elements = [affine_element_from_fit(ref_el, nodes, node_positions, stiffness)
                    for nodes in nodes_per_elem]
        #node_positions = ref_el.nodes
        print(node_positions)

        displacements_x = [[0,0], [2,0], [1, 0.5], [3,0.5]]
        displacements_y = [[2,0]]
        forces_x = []
        forces_y = []
        
        fem = FEMGrid2D(node_positions, elements, displacements_x,
                        displacements_y, forces_x, forces_y)
        print("CHECKING BOUNDARY CONDITIONS")
        print(fem.dirichlet)
        print(fem.neumann)
        print("CHECKING STIFFNESS MATRIX")
        print(elements[0].K)
        print(fem.assemble_full_matrix())
        pos = fem.solve_for_pos()
        print(pos)
        fem.render(pos)

    def hole_plate():
        x_range = np.linspace(0, 6, 4)
        x_grid, y_grid = np.meshgrid(x_range, x_range, indexing="xy")
        node_positions = np.array(list(zip(x_grid.flatten(), y_grid.flatten())))
        print(node_positions)
        nodes_per_elem = np.array([[5, 1, 0, 4],
                                   [6, 2, 1, 5],
                                   [7, 3, 2, 6],
                                   [8, 9, 5, 4],
                                   [11, 7, 6, 10],
                                   [13, 9, 8, 12],
                                   [14, 10, 9, 13],
                                   [15, 11, 10, 14]])
        elements = []
        for nodes in nodes_per_elem:
            destination_points = np.array([node_positions[n] for n in nodes])
            element = affine_element_from_fit(ref_el, nodes,
                                              destination_points, stiffness)
            elements.append(element)
        displacements_x = [[0, 0], [15, 0.5]]
        displacements_y = [[0, 0]]
        forces_x = []
        forces_y = []
        fem = FEMGrid2D(node_positions, elements,
                        displacements_x, displacements_y, forces_x, forces_y)

        fem.render(fem.solve_for_pos())

    tension()

    hole_plate()
