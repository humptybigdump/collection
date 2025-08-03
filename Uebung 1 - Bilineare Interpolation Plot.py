# Simple Plot of Bilinear Interpolation of four data points

import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D



# Make data. Points to interpolate:
# (0,0,0), (0,1,0), (1,0,0), (1,1,1)
# Bilinear Interpolation yields y(u,z) = u*z


u_data = np.asarray([0, 1])
z_data = np.asarray([0, 1])
u_data, z_data = np.meshgrid(u_data, z_data)
y_data = u_data*z_data

u = np.linspace(0,1,11)
z = np.linspace(0,1,11)
u, z = np.meshgrid(u, z)
y = u*z

u_data = np.asarray([0, 1])
z_data = np.asarray([0, 1])
u_data, z_data = np.meshgrid(u_data, z_data)
y_data = u_data*z_data


fig = plt.figure()
ax = fig.gca(projection='3d')
ax.plot_surface(u, z, y, cmap="autumn_r", lw=0.5, rstride=1, cstride=1, alpha=0.6)

ax.plot_wireframe(u, z, y, rstride=1, cstride=1, color='#4185f2')

theta = np.linspace(-4 * np.pi, 4 * np.pi, 100)
u = np.linspace(0, 1, 100)
z = u
y = u*z

ax.plot(u, z, y, color='#19458c', linewidth=2.5)
ax.scatter(z_data, u_data, y_data, marker='o', color='r', label='Daten', s=35)

ax.set_xlabel('u')
ax.set_ylabel('z')
ax.set_zlabel('y')

plt.title('Bilineare Interpolation von Vier Stützstellen\n')
# ax.legend()

plt.show()

