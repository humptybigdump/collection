# Computation of persistent homology with Ripser
# Lecture course "Applications of Topological Data Analysis"
# Lecture 28
# Andreas Ott
# KIT SS 2021

# Import Python numpy and Ripser package

import numpy as np
import random as rd
from ripser import ripser
from matplotlib import pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

# Function for the computation of the Euclidean distance

def distance(x, y):

    sum = 0
    for i in range(len(x)):
        sum = sum + (x[i]-y[i])**2
    dist = np.sqrt(sum)

    return dist


# Define point clouds

# Random point cloud
R = 5
S_1 = []
for n in range(150):
    x = R * rd.random()
    y = R * rd.random()
    z = R * rd.random()
    S_1.append((x, y, z))

# Random points on 2-sphere
R = 5
S_2 = []
for n in range(100):
    phi = rd.random() * 2 * np.pi
    theta = rd.random() * np.pi
    x = R * np.sin(theta) * np.cos(phi)
    y = R * np.sin(theta) * np.sin(phi)
    z = R * np.cos(theta)
    S_2.append((x, y, z))

# Choose point cloud
S = S_2

print('Set of points S:')
for x in S:
    print(x)
print('\n')

# Compute number of points

n = len(S)
print('Number of points: ', n)
print('\n')

# Compute distance matrix

D = np.zeros((n,n))
for i in range(n):
    for j in range(n):
        D[i,j] = distance(S[i], S[j])

print('Distance matrix:')
print(D)
print('\n')

# Write distance matrix to file

np.savetxt("distance_matrix_3d_cloud.csv", D, delimiter=",")

# Compute persistent homology with Ripser

data = ripser(D, distance_matrix=True, maxdim=2)

# Print persistence intervals

for deg in range(len(data['dgms'])):
    print('Persistence intervals in degree ' + str(deg) + ':')
    print(data['dgms'][deg])
    print('\n')

# Plot point cloud and write plot to file

fig = plt.figure()
ax = Axes3D(fig)
ax.scatter([point[0] for point in S], [point[1] for point in S], [point[2] for point in S])
plt.savefig('figure_3d_cloud.pdf')
plt.show()
plt.close()
