# Computation of persistent homology and cycle localization with Ripser
# Lecture course "Applications of Topological Data Analysis"
# Lecture 29
# Andreas Ott
# KIT SS 2021

# Import Python numpy package

import numpy as np
from matplotlib import pyplot as plt
import subprocess


# Function for the computation of the Euclidean distance

def distance(x, y):

    sum = 0
    for i in range(len(x)):
        sum = sum + (x[i]-y[i])**2

    return np.sqrt( sum )


# Define point cloud

S_1 = [
    (0, 0),
    (1, 0),
    (1, 1),
    (0, 1)
]

S_2 = [
    (0, 0),
    (1, 0),
    (1.5, 0.5),
    (1, 1),
    (0,1)
]

S_3 = [
    (0, 0),
    (1, 0),
    (1, 1),
    (1, 2),
    (0, 2),
    (0, 1)
]

S_4 = [
    (-1, -2),
    (1, -2),
    (0, -2),
    (0, -1),
    (1, 0),
    (2, 0),
    (3, 0),
    (4, 0),
    (5, 0),
    (0, 4),
    (0, 3),
    (0, 2),
    (0, 1),
    (-1, 0),
    (-2, 0),
    (-3, 0)
]

S = S_1

print('Set of points S:')
for x in S:
    print(x)
print('\n')

# Compute number of points

n = len(S)

# Compute distance matrix

D = np.zeros((n,n))
for i in range(n):
    for j in range(n):
        D[i,j] = distance(S[i], S[j])

print('Distance matrix:')
print(D)
print('\n')

# Write distance matrix to file

np.savetxt("distance_matrix.csv", D, delimiter=",")

# Compute persistent homology and cycle representatives with Ripser

# Write result to screen
subprocess.run(['./ripser-representatives', 'distance_matrix.csv'])

# Write result to file
#with open('ripser_localization.txt', 'w') as file:
#    subprocess.run(['./ripser-representatives', 'distance_matrix.csv'], stdout=file)

# Plot point cloud and write plot to file

plt.scatter([point[0] for point in S], [point[1] for point in S])
plt.savefig('figure_localization.pdf')
plt.show()
plt.close()
