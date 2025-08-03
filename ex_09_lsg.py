import numpy as np
from Blatt_08_Loesung import BoundaryValueProblem

class LinElement1D():
    """1D linear finite element with homogeneous stiffness.

    Members:
    stiffness: scalar tensile stiffness E*A
    nodes: node index of both nodes
    l: length
    K: stiffness matrix

    Methods:
    __init__: Receives stiffness, nodes and node_positions (of all nodes
                       in the system)
    get_N: calculate N analytically for testing purposes (takes parameter x)
    get_B: calculate B analytically for testing purposes (takes parameter x)
    get_K: calculate K analytically 
    """
    def __init__(self, stiffness, nodes, node_positions):
        self.stiffness = stiffness
        self.nodes = nodes
        self.l = abs(node_positions[1] - node_positions[0])
        self.K = self.get_K()

    def get_N(self, x):
        result = np.zeros((1,2))
        result[0,0] = -x/self.l+1
        result[0,1] =  x/self.l
        return result

    def get_B(self):
        result = np.zeros((1,2))
        result[0,0] = -1/self.l
        result[0,1] = 1/self.l
        return result

    def get_K(self):
        B = self.get_B()
        volume = self.l
        result = volume*self.stiffness*np.outer(B,B)
        return result

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
        self.node_positions = node_positions
        self.elements = elements
        x0 = np.zeros(len(self.node_positions))
        super().__init__(x0, displacements, forces)

    def assemble_full_matrix(self):
        stiffness_matrix = np.zeros((self.n_dof, self.n_dof))
        for element in self.elements:
            global_indices = element.nodes
            local_indices = range(len(element.nodes))
            for i in local_indices:
                for j in local_indices:
                    stiffness_matrix[global_indices[i], global_indices[j]] += element.K[i,j]
        return stiffness_matrix

    def solve_for_pos(self):
        return self.node_positions+self.solve()


def homogeneous_bar(node_positions):
    print("Investigating discretization:", node_positions)
    element_nodes = []
    element_node_positions = []
    for i in range(len(node_positions)-1):
        element_nodes.append([i, i+1])
        element_node_positions.append([node_positions[i],node_positions[i+1]])
    element_nodes = np.array(element_nodes)
    element_node_positions = np.array(element_node_positions)

    stiffnesses = np.array([1]*len(element_nodes))
    elements = [LinElement1D(stiffness, nodes, positions) 
                for stiffness, nodes, positions in zip(stiffnesses, element_nodes, element_node_positions)]
    displacements = [(0,0)]
    forces = [(len(node_positions)-1, 1)]
    fem_problem = FEMGrid1D(node_positions, elements, displacements, forces)
    u_solution = fem_problem.solve()
    print("Solution for u:", u_solution)
    x_solution = fem_problem.solve_for_pos()
    print("Solution for x:", x_solution)
    print("Stiffness:", forces[0][1]/u_solution[-1])

if __name__ == "__main__":
    
    node_positions = np.array([0, 2])
    homogeneous_bar(node_positions)
    node_positions = np.array([0, 0.5, 1, 2])
    homogeneous_bar(node_positions)
