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

# Function for the computation of the Euclidean distance

def distance(x, y):

    return np.sqrt( (x[0]-y[0])**2 + (x[1]-y[1])**2 )


# Define point clouds

# Simple point configurations
S_1a = [
    (0,0),
    (0,1),
    (1,0),
    (1,1)
]

S_1b = [
    (0,0),
    (0,1),
    (1,0),
    (1,1),
    (0,2),
    (1,2)
]

S_1c = [
    (0,0),
    (0,1),
    (1,0),
    (1,1),
    (0.5,1.5)
]

# Random point cloud
R = 5
S_2 = []
for n in range(50):
    x = R * rd.random()
    y = R * rd.random()
    S_2.append((x, y))

# Random point clusters
R = 5
S_3 = []
for vector in [(0,0), (20,0), (0,20), (20,20)]:
    S_pre = []
    for n in range(15):
        x = R * rd.random()
        y = R * rd.random()
        S_pre.append((x + vector[0], y + vector[1]))
    S_3.extend(S_pre)

# Random points on 1-sphere
R = 5
S_4 = []
for n in range(200):
    phi = rd.random() * 2 * np.pi
    x = R * np.sin(phi)
    y = R * np.cos(phi)
    S_4.append((x, y))

# Choose point cloud
S = S_4

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

np.savetxt("distance_matrix_2d_cloud.csv", D, delimiter=",")

# Compute persistent homology with Ripser

data = ripser(D, distance_matrix=True, maxdim=2)

# Print persistence intervals

for deg in range(len(data['dgms'])):
    print('Persistence intervals in degree ' + str(deg) + ':')
    print(data['dgms'][deg])
    print('\n')

# Plot point cloud and write plot to file

plt.scatter([point[0] for point in S], [point[1] for point in S])
plt.savefig('figure_2d_cloud.pdf')
plt.show()
plt.close()
