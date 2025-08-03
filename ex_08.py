import numpy as np 
from scipy.sparse import linalg
from mpl_toolkits import mplot3d
import matplotlib.pyplot as plt 

#### EXERCISE 1 ####
# Implement the general BoundaryValueProblem.
# The reduced system is the full system matrix with all rows and columns
# corresponding to nodes which are fixed in place removed, and a reduced rhs. 

class BoundaryValueProblem(object):
    """Describes a general boundary value problem matrix@x = rhs.
    Members:
    x0: initial guess for the solution
    n_dof: number of degrees of freedom
    dirichlet: list of tuples of index, value for dirichlet bc
    neumann: list of tuples of index, value for neumann bc
    dirichlet_i, neumann_i: lists of boundary indices
    dirichlet_v, neumann_v: lists of boundary values

    Methods:
    __init__: Receives initial value x0 (as np array), 
                       dirichlet and neumann bc (as lists of tuples).
    assemble_full_matrix: Raises NotImplementedError.
    reduced_system: Removes dirichlet elements from the system matrix and builds
    a right hand side vector. Returns reduced system matrix, reduced rhs, 
    reduced x0.
    full_x: Receives x_reduced, returns the full x by adding given dirichlet 
    values.
    solve: Solves the boundary value problem using scipy.sparse.linalg.cg"""
    def __init__(self, x0, dirichlet, neumann):
        pass

    def assemble_full_matrix(self):
        raise NotImplementedError()

    def reduced_system(self):
        pass

    def full_x(self, x_reduced):
        pass

    def solve(self):
        pass


#### EXERCISE 2 ####
# Implement SpringProblem1D.
# For a really efficient implementation, it would be better to pass
# the information in springs as 3 np arrays. This version allows for simpler 
# code using the for loop syntax: for index_1, index_2, C in springs

class SpringProblem1D(BoundaryValueProblem):
    """Describes a 1D spring displacement problem Cu = F.
    Members:
    x_relaxed, springs
    Methods:
    __init__: Receives a relaxed node position array x_relaxed, a list of 
    tuples describing springs connecting 2 nodes, and two lists of tuples
    describing displacements and forces respectively. 
    [(index_1, index_2, C_i) ... ].
    assemble_full_matrix: Returns assembled stiffness matrix.
    solve_for_pos: Solves system and returns x, not u.
    """
    def __init__(self, x_relaxed, springs, displacements, forces):
        pass

    def assemble_full_matrix(self):
        pass

    def solve_for_pos(self):
        pass


class SpringVisualizer():
    def __init__(self, spring_width=.8, spring_distance=.2, coil_length=.3):
        self.spring_width = spring_width
        self.spring_distance = spring_distance
        self.coil_length = coil_length

    def _spring_plot_points(self, spring_length):
        """Returns x, y plot coordinates of an undeformed spring, adjusting
        the number of coils to the spring length.
        """
        n_coils = int(spring_length/self.coil_length)-1
        remainder = spring_length-self.coil_length*n_coils
        x = np.zeros(2*n_coils+4)
        x[1] = remainder/2
        x[-2] = spring_length-remainder/2
        x[-1] = spring_length
        y = np.zeros(2*n_coils+4)
        for coil_index in range(n_coils):
            up_index = 2+2*coil_index
            down_index = up_index+1
            x[up_index] = self.coil_length*(coil_index+0.25)+x[1]
            x[down_index] = self.coil_length*(coil_index+0.75)+x[1]
            y[2+2*coil_index] = self.spring_width/2
            y[3+2*coil_index] = -self.spring_width/2
        return x, y

    def plot(self, x_result, spring_problem):
        """Plots the solution of the given spring problem."""
        n_springs = len(spring_problem.springs)
        y_coords = np.arange(len(spring_problem.springs))
        spring_increment = self.spring_width+self.spring_distance

        for spring_index in range(n_springs):
            index_1, index_2, C = spring_problem.springs[spring_index]
            x_1, x_2 = x_result[index_1], x_result[index_2]
            length = abs(x_2-x_1)
            length_0 = abs(spring_problem.x_relaxed[index_1]
                           -spring_problem.x_relaxed[index_2])
            x, y = self._spring_plot_points(length_0)
            x = length/length_0*x+min(x_1, x_2)
            y = y+spring_index*spring_increment
            plt.plot(x,y, "C0")

        lower_bound = -self.spring_distance
        upper_bound = spring_increment*n_springs
        for x in x_result:
            plt.plot([x, x], [lower_bound, upper_bound], "C0")


if __name__ == "__main__":
    # Solve the problem in the exercise sheet.
    # Use the visualizer to plot your result. You still need to plt.show.
    visualizer = SpringVisualizer()

    x_relaxed = np.array([0,2,4,6,8])
    springs = [(0, 2, 1000), 
               (0, 1, 2000), 
               (1, 2, 3000), 
               (1, 3, 2000), 
               (0, 3, 2500), 
               (3, 4, 6000), 
               (2, 4, 2000)]
    displacements = [(0,0)]
    forces = [(4,1000)]
    sheet_problem = SpringProblem1D(x_relaxed, springs, displacements, forces)
    x_solution = sheet_problem.solve_for_pos()
    visualizer.plot(x_solution, sheet_problem)
    plt.show()


#### BONUS ####
# Implement SpringProblem2D.
# We assume all springs are either horizontal or vertical.
# We assume that a vertical movement of a node does not cause a horizontal
# lengthening of springs. (Small deformations.)
# Then the different directions of the problem are not coupled, and we only
# need to deal with managing the inputs.
# The positions of the nodes in this problem are a (n,2)-array.
# This means that the system matrix is a fourth order tensor. Similar to the
# Voigt notation, it is possible to write a (2n)-array instead, which gives us
# a (2n,2n) system matrix which can be solved with the usual methods. For this
# reason, we distinguish an external index (i, j) with i in [0,n) and
# j in [0,1] from an internal index k with k in [0, 2n).

class SpringProblem2D(BoundaryValueProblem):
    """Describes a 2D spring displacement problem Cu = F.
    Members:
    x_relaxed, springs
    Methods:
    __init__: Receives a relaxed node position array pos_relaxed, a list of 
    tuples describing springs connecting 2 nodes, and two lists of tuples
    describing displacements and forces respectively. Spring descriptions are
    like the 1D case. The displacements and forces are [(i, j, V)], where i is
    the node index, j is either 0 or 1 for x or y direction, and V is the value.
    assemble_full_matrix: Returns assembled stiffness matrix.
    solve_for_pos: Solves system and returns x, not u.
    """
    def __init__(self, pos_relaxed, springs, displacements, forces):
        super().__init__(np.zeros(x_relaxed.shape), displacements, forces)
        self.pos_flat = np.flatten(pos_relaxed)
        self.springs = springs

    @staticmethod
    def internal_index(external_index):
        return 2*external_index[0] + external_index[1]

    @staticmethod
    def external_index(internal_index):
        return (internal_index//2, internal_index%2)

    def assemble_full_matrix(self):
        stiffness_matrix = np.zeros((self.n_dof, self.n_dof))
        for i1, i2, C in self.springs:
            stiffness_matrix[i1, i1] += C
            stiffness_matrix[i2, i2] += C
            stiffness_matrix[i1, i2] += -C
            stiffness_matrix[i2, i1] += -C
        return stiffness_matrix

    def solve_for_pos(self):
        return self.x_relaxed+self.solve()