# Computation of persistent homology with Ripser
# Point cloud data set from Exercise 41
# Lecture course "Topological Data Analysis"
# KIT  WS 20/21

# Import Python numpy and Ripser package

import numpy as np
from ripser import ripser

# Function for the computation of the Euclidean distance

def distance(x, y):

    return np.sqrt( (x[0]-y[0])**2 + (x[1]-y[1])**2 )


# Define set of points

#S_test = [
#    (0,0),
#    (0,1),
#    (1,0),
#    (1,1)
#]

S = [
    (0,0),
    (4,1),
    (6,2),
    (7,4),
    (6,6),
    (4,8),
    (2,9),
    (0,9),
    (-2,9),
    (-4,8),
    (-5,6),
    (-5,4),
    (0,11),
    (0,13),
    (2,14),
    (2,16),
    (0,17),
    (-2,16),
    (-2,14)
]

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

print('Distance matrix D:')
print(D)
print('\n')

# Write distance matrix to file

np.savetxt("distance_matrix_ex_41.csv", D, delimiter=",")

# Compute persistent homology with Ripser

data = ripser(D, distance_matrix=True)

# Print persistence intervals

for deg in range(len(data['dgms'])):
    print('Persistence intervals in degree ' + str(deg) + ':')
    print(data['dgms'][deg])
