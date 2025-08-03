import numpy as np
import matplotlib.pyplot as plt
from scipy.optimize import NonlinearConstraint, LinearConstraint
from scipy.optimize import minimize


class BendingElement:
    def __init__(self, L=1, E=5000, h=1, b=0.1):
        # Moment of Inertia
        self.I = h**3*b/12

        # Element stiffness matrix on [w0, omega0, w1, omega1] with displacment w and angle omega
        self.ke = E*self.I/(L**3)*np.array([[12, 6*L, -12, 6*L],
                                            [6*L, 4*L**2, -6*L, 2*L**2],
                                            [-12, -6*L, 12, -6*L],
                                            [6*L, 2*L**2, -6*L, 4*L**2]])


class FEMSolver:
    def __init__(self, F):
        self.F = F

    def solve(self, h_list):
        """Solve 1D bending beam using Finite Elements. Input is a list of segment heights and output ist the compliance of the beam."""
        self.h_list = h_list
        self.N = len(h_list)
        ellist = []

        # Collect elements with varying beam height
        for h in h_list:
            ellist.append(BendingElement(h=h, L=1/self.N))

        # Each node has a displacement and an angle
        self.system_size = 2*(self.N+1)

        # Assemble the global system
        self.global_system = np.zeros([self.system_size, self.system_size])
        for i, el in enumerate(ellist):
            # Assigning matrices are used to distribute element stiffnesses into global system
            L_e_matrix = np.zeros([self.system_size, 4])
            L_e_matrix[2*i:2*i+4, :] = np.eye(4)
            self.global_system += L_e_matrix@el.ke@L_e_matrix.T

        # Enforce boundary condition on the left side (clamped beam)
        self.reduced_system = self.global_system[2:, 2:]

        # The forces vector only contains the force F at the free beam end. Try a distributed load, too!
        # The force acts on the displacement part of the last node
        forces = np.zeros([self.system_size-2])
        forces[-2] = self.F

        # Solve the linear FE system
        # You might also use one of your own solvers here!
        self.u = np.linalg.solve(self.reduced_system, forces)

        # Output, every other entry is the displacement
        print(f'u: {self.u[::2]}')

        # Plot the displacement
        # plt.plot(range(int(self.system_size/2)),[0]+list(self.u[::2]))

        # Compliance
        self.compliance = self.u[-2]/self.F

        return self.compliance


def main():
    """main method. Perform your optimization here."""

    # Initialize segment height list, the length of h0 determines the number of elements
    N = 4
    h0 = 0.5*np.ones(N)
        
    # Define constraints
    # ...

    # Minimize
    # ...

    # Postprocessing and plotting of the results
    # ...

if __name__=='__main__':
    main()