# Computation of persistent homology with Ripser
# Point cloud data set from Exercise 42
# Lecture course "Topological Data Analysis"
# KIT  WS 20/21

# Import Python numpy and Ripser package

import numpy as np
from ripser import ripser

# Function for the computation of the Hamming distance

def distance(x, y):

    dist = 0
    for i in range(len(x)):
        if x[i] != y[i]:
            dist += 1

    return dist

#print(distance('MOUSE', 'HOUSE'))

# Define set of points

S = [
    'MOUSEXXXXX',
    'HOUSEXXXXX',
    'STUHLXXXXX',
    'GOOSEYYYYY',
    'SITTEYYYYY',
    'VIRUSZZZZZ',
    'HILFEWWWWW'
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

np.savetxt("distance_matrix_ex_42.csv", D, delimiter=",")

# Compute persistent homology with Ripser

data = ripser(D, distance_matrix=True)

# Print persistence intervals

for deg in range(len(data['dgms'])):
    print('Persistence intervals in degree ' + str(deg) + ':')
    print(data['dgms'][deg])
