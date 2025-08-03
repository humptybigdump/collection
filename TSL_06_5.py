import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation
from sympy import symbols, Function, Eq, dsolve, cos, sin, Derivative, simplify, lambdify

# Define symbols
t = symbols('t')
# Parameters
ratio_damping = 2
ratio_omega = 1/2
D_1 = 1/4  # 1/4
D_2 = ratio_damping * D_1
omega_1 = 1
omega_2 = ratio_omega * omega_1
varepsilon = 1
Omega = 3/4 * omega_1
eta_1 = Omega / omega_1
eta_2 = Omega / omega_2

# Assume we have initial conditions
C1, C2 = symbols('C1 C2')

# Define functions
y_W = Function('y_W')(t)
z_W = Function('z_W')(t)

# Define differential equations
dgly = Eq(Derivative(y_W, t, 2) + 2*D_1*Derivative(y_W, t) + y_W, varepsilon * eta_1**2 * cos(eta_1 * t))
dglz = Eq(Derivative(z_W, t, 2) + 2*D_2*Derivative(z_W, t) + z_W, varepsilon * eta_2**2 * sin(eta_2 * t))

# Solve the differential equations for particular solutions
Lsgy = simplify(dsolve(dgly, y_W).rhs.subs({t: t * omega_1, C1: 0, C2: 0}))
Lsgz = simplify(dsolve(dglz, z_W).rhs.subs({t: t * omega_2, C1: 0, C2: 0}))

# Stationary coordinates
y_S = Lsgy + varepsilon * cos(Omega * t)
z_S = Lsgz + varepsilon * sin(Omega * t)

# Substitute parameters for plotting
params = {omega_1: omega_1, omega_2: omega_2, varepsilon: varepsilon, Omega: Omega}

# Create time array for plotting
t_values = np.linspace(0, 8 * np.pi, 500)

# Evaluate the functions for plotting
Lsgy_func = lambdify(t, Lsgy.subs(params), 'numpy')
Lsgz_func = lambdify(t, Lsgz.subs(params), 'numpy')
y_S_func = lambdify(t, y_S.subs(params), 'numpy')
z_S_func = lambdify(t, z_S.subs(params), 'numpy')

# Print evaluated functions for verification
print("Lsgy_func: {}".format(Lsgy_func(t_values)))
print("Lsgz_func: {}".format(Lsgz_func(t_values)))

# # Plotting
# plt.figure(figsize=(12, 8))
#
# # Plot stationary solutions
# plt.subplot(2, 1, 1)
# plt.plot(t_values, Lsgy_func(t_values), label='Lsgy', color='blue')
# plt.plot(t_values, Lsgz_func(t_values), label='Lsgz', color='green')
# plt.title('Stationary Solutions')
# plt.xlabel('t')
# plt.ylabel('y_W, z_W')
# plt.legend()
#
# # Plot shifted coordinates
# plt.subplot(2, 1, 2)
# plt.plot(t_values, y_S_func(t_values), label='y_S', color='red')
# plt.plot(t_values, z_S_func(t_values), label='z_S', color='orange')
# plt.title('Shifted Coordinates')
# plt.xlabel('t')
# plt.ylabel('y_S, z_S')
# plt.legend()
#
# plt.tight_layout()
# plt.show()


# Evaluate the solutions to numerical values for animation
y_W_vals = Lsgy_func(t_values)
z_W_vals = Lsgz_func(t_values)
y_S_vals = y_S_func(t_values)
z_S_vals = z_S_func(t_values)

# Prepare the plot for the animation
fig, ax = plt.subplots()
ax.plot(y_W_vals, z_W_vals, label='W', color='red')
ax.plot(y_S_vals, z_S_vals, label='S', color='blue')

point_W, = ax.plot(y_W_vals[0], z_W_vals[0], 'ro', markersize=5, label='Point W')
point_S, = ax.plot(y_S_vals[0], z_S_vals[0], 'bo', markersize=5, label='Point S')
line, = ax.plot([y_W_vals[0], y_S_vals[0]], [z_W_vals[0], z_S_vals[0]], color='black')

ax.legend()
ax.set_xlim([min(min(y_W_vals), min(y_S_vals))-0.1, max(max(y_W_vals), max(y_S_vals))+0.1])
ax.set_ylim([min(min(z_W_vals), min(z_S_vals))-0.1, max(max(z_W_vals), max(z_S_vals))+0.1])


def animate(i):
    point_W.set_data(y_W_vals[i-1:i], z_W_vals[i-1:i])
    point_S.set_data(y_S_vals[i-1:i], z_S_vals[i-1:i])
    line.set_data([y_W_vals[i-1:i], y_S_vals[i-1:i]], [z_W_vals[i-1:i], z_S_vals[i-1:i]])
    return point_W, point_S, line


ani = animation.FuncAnimation(fig=fig, func=animate, frames=len(t_values), interval=30)
plt.grid()
plt.show()
