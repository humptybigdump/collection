import numpy as np
import matplotlib.pyplot as plt
from scipy.spatial import Delaunay
from matplotlib.tri import Triangulation

### Mesh construction
# radial coordinates; you can increase the resolution here
# Try to keep them such that the points coincide with the boundary between
# the high and low permittivity media. This way the mesh conforms to the geometry
# and there is no extra error from geometry-discretization mismatch.
r = np.linspace(0, 1, 31)
dxt = r[1]-r[0]
xc = [0.0]
yc = [0.0]
for i in range(1,len(r)):
    # Add points circle by circle, such that resolution stays the same.
    Np = int((2*np.pi*r[i]) // dxt)
    phi = np.linspace(0, 2*np.pi, Np+1)
    xt = r[i]*np.cos(phi[0:-1])
    yt = r[i]*np.sin(phi[0:-1])
    xc += list(xt)
    yc += list(yt)
x = np.array(xc)
y = np.array(yc)
# Sort elements; this may or may not help the solver
i = np.argsort(x)
x = x[i]
y = y[i]
i = np.argsort(y)
x = x[i]
y = y[i]

### Delaunay triangulation
c = np.stack([x.flat, y.flat], axis=1)  # N by 2
tri = Delaunay(c)
N_elements = tri.simplices.shape[0]
centers = tri.points[tri.simplices].mean(axis=1) # N by 2
N_points = tri.points.shape[0]

### Plot the mesh
plt.figure()
plt.triplot(x,y,tri.simplices)
plt.axis('equal')

### Notes about using the mesh / triangulation "tri"
# Get indices of the points that form the triangle with index n
#   ip = tri.simplices[n, :]
# Get coordinates of the point with index m
#   c = tri.points[m,:]
# Get transform matrix A for the triangle n,
#   A = tri.transform[n, 0:2, 0:2]

### Plotting field distributions
# matplotlib wants its own triangulation object
mpltri = Triangulation(tri.points[:,0], tri.points[:,1], triangles=tri.simplices)
plt.figure()
# Here, 'field' is a vector of field unknowns in the same order as the
# corresponding points in the triangulation.
plt.tripcolor(mpltri, abs(field))
plt.show()
