from Solvers import *
from Problem import *
import matplotlib.pyplot as plt


n = 8
x0 = np.ones(n)
myFunction = f2()

# Dictionary with different types of step sizes and (if applicable) a fixed stepsize
stepsizes = {
    # 'FullRelaxationStepsize': None,
    'ConstantStepsize': 0.02,
    'MalitskyMishchenko': None,
    'HeavyBall': None
}

fig, ax = plt.subplots()
fig2, ax2 = plt.subplots()
plot_range_x = 6
plot_range_y = 6

# Iterate over the different types of stepsize
for stepsize_name in stepsizes:

    # Initialize stepsize object
    stepsize = AbstractStepsize.select_from_name(myFunction, stepsize_name, stepsizes[stepsize_name])
    
    # Initialize Solver
    solver = GradientDescent(myFunction, stepsize)

    # Solve
    x, res_list, alpha_list, x_list = solver.solve(x0, list_x = True)

    # Output
    print(
        f"Found a solution using {stepsize_name} for x0 = {x0} in {len(res_list)} iterations:\n\t x = {x}\n")
    x_res = np.arange(len(res_list))
    ax2.plot(x_res, alpha_list, label=f'Alpha {stepsize_name}')
    ax.semilogy(x_res, res_list, label=f'Residual {stepsize_name}')

# Plot
ax.legend()
ax2.legend()
plt.show()
