# Computation of persistent homology with Ripser
# Lecture course "Topological Data Analysis"
# KIT  WS 20/21

# Import Python numpy and Ripser package

import numpy as np
from ripser import ripser


# Define distance matrix

D = np.array([
    [  0,   1, 1.4,  1   ],
    [  1,   0,   1,  1.4 ],
    [  1.4, 1,   0,  1   ],
    [  1, 1.4,   1,  0   ],
])

# Compute persistent homology with Ripser

data = ripser(D, distance_matrix=True)

# Print persistence intervals

# print(data)

np.savetxt("distance_matrix_square.csv", D, delimiter=",")

for deg in range(len(data['dgms'])):
    print('Persistence intervals in degree ' + str(deg) + ':')
    print(data['dgms'][deg])
