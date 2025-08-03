import numpy as np
from scipy.optimize import NonlinearConstraint
from scipy.optimize import minimize
from Problem import f1  # This is the Himmelblau function
from Visualize import Visualize


def squared_distance_to(center):
    """
    This method does not return a value, but a 'callable'. The callable then returns the squared distance to the fixed center.
    'lambda x: f(x)' is a shorthand to quickly create callable functions f(x).
    You can hand over the results to another function, which can then evaluate the lambda on its own.
    """
    return lambda x: (x-center)@(x-center)


# We can use squared_distance_to_center() as follows:
my_distance = squared_distance_to(np.array([1, 1]))

# We now have a function that always returns the squared distance from point [1,1]
print(my_distance(np.array([0, 0])))

###
# Now for the actual problem
###

center = np.array([2, 2])
radius = 2

# Create a callable distance function to hand over to NonlinearConstraint
callable_distance_to_center = squared_distance_to(center)

# Create two constraints. Note the squared bounds (as we are using the squared distance).
# Inequality constraint with lower and upper bound.
constrIneq = NonlinearConstraint(callable_distance_to_center, 0, radius**2)

# Equality constraint. We achieve this by passing the same upper and lower bound.
constrEq = NonlinearConstraint(callable_distance_to_center, radius**2, radius**2)

# Hint: One-sided constraints are also possible using -np.inf or np.inf

# Minimize
sol_ineq = minimize(f1().value, np.ones(2), constraints=[constrIneq])
sol_eq = minimize(f1().value, np.ones(2), constraints=[constrEq])

# Hint: minimize returns the minimizer (e.g., sol_eq.x) as well as the function value at that point (sol_eq.fun)

###
# Visualization
###

# Create the cartesian coordinates of a circle with center and radius
phi = np.linspace(0, 2*np.pi)
x = center[0] + radius*np.cos(phi)
y = center[1] + radius*np.sin(phi)

# Visualize the function values
viz = Visualize(f1().value, np.linspace(-6, 6, 200), np.linspace(-6, 6, 200))
viz.addContourPlot()

# Visualize the circle
viz.addPlot(x, y)

# Visualize the inequality solution
viz.addPlot(sol_ineq.x[0], sol_ineq.x[1], marker='*')

# Visualize the equality solution
viz.addPlot(sol_eq.x[0], sol_eq.x[1], marker='*')

# Show
viz.showAll()
