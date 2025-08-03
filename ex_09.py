import numpy as np
from ex_08 import BoundaryValueProblem

class LinElement1D():
    """1D linear finite element with homogeneous stiffness.
    
    Members:
    stiffness: scalar tensile stiffness E*A
    nodes: node index of both nodes
    l: length
    K: stiffness matrix

    Methods:
    __init__: Receives stiffness, nodes and node_positions 
    get_N: calculate N analytically for testing purposes (takes parameter x)
    get_B: calculate B analytically for testing purposes
    get_K: calculate K analytically 
    """
    
    def __init__(self, stiffness, nodes, node_positions):
        pass

    def get_N(self, x):
        pass

    def get_B(self, x):
        pass

    def get_K(self):
        pass

class FEMGrid1D(BoundaryValueProblem):
    """1D FEM grid boundary value problem.

    Members:
    node_positions: positions of all nodes in the system
    elements: list elements defined as a list of node ids
    displacements: displacements as (node_id, value) pairs
    forces: forces as (node_id, value) pairs

    Methods:
    assemble_full_matrix: assembles system matrix out of element matrices
    solve_for_pos: solves system in u, then adds x0 to return solution for x
    """
    def __init__(self, node_positions, elements,
                 displacements, forces):
        pass

    def assemble_full_matrix(self):
        pass

    def solve_for_pos(self):
        pass
