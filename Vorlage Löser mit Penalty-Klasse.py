import numpy as np

class EqualityConstraint:
    def __init__(self, constraint_function, ):
        # Do stuff
        pass

class InequalityConstraint:
    def __init__(self):
        # Do stuff
        pass

class Solver:
    """Abstract solver class."""

    def __init__(self, function, tol, maxit) -> None:
        self.function = function
        self.tol = tol
        self.maxit = maxit


class GradientDescent(Solver):
    """Minimize a function using the Gradient Descent scheme"""

    def __init__(self, function, tol=1e-6, maxit=1000, constraint_list=None) -> None:
        super().__init__(function, tol, maxit)
        self.constraint_list = constraint_list
        


    def solve(self, x0, alpha_preset):
        if self.constraint_list is not None:
            ###
            # Update function value and gradient 
            ###
            pass
        ################################
        ### Implement GD here
        ################################

       
        # For plotting, append the intermediate x's to this array.
        x_list = [x0]
        res_list = []

        # Implement the Gradient Descent scheme so that x is a local minimizer of the function
        x = np.array([5, 10])

        return [x, res_list, x_list]
