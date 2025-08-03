# Computation of the persistent homology of a phylogenetic network
# Lectures 31 & 32
# Lecture course "Topological Data Analysis"
# KIT SS 21

# Import Python numpy and subprocess package

import numpy as np
import subprocess

# Function for the computation of the Hamming distance

def hamming_dist(x, y):

    dist = 0
    for i in range(len(x)):
        if x[i] != y[i]:
            dist += 1

    return dist


# Define nucleotide sequence data sets

S_1 = [
    'AAAA',
    'CAAA',
    'AAAG',
    'CAAT',
    'CGAA',
    'TAAG',
    'GAAG',
    'CATT',
    'CCAT',
    'CGAC',
    'CGGA',
    'CGTA',
    'TTAG',
    'TACG',
    'GAGG',
    'GGAG'
]

S_2 = [
    'AA',
    'CA',
    'AC',
    'CC'
]

S_3 = [
    'AAAA',
    'CAAA',
    'AAAG',
    'CAAT',
    'CGAA',
    'TAAG',
    'CAAG',
    'CATT',
    'CCAT',
    'CGAC',
    'CGGA',
    'TTAG',
    'TACG',
    'CGAG',
    'CAGG'
]

S_4 = [
    'AAAAACCCCC',
    'CAAAACCCCC',
    'AAAAACCCCT',
    'CAAAACCGCC',
    'CAATACCCCC',
    'AAAAAGCCCT',
    'CAGTACCCCC',
    'AAAAAGACCT',
    'CAGTACTCCC',
    'CAGTTCCCCC',
    'CGGTACTCCC',
    'AAAAAGAACT',
    'CGGTCCTCCC',
    'AAAAAGAAGT',
    'AATAAGAACT',
    'CGGTCCTCAC',
    'CGGTCGAAGT'
]

S = S_2

print('Set of nucleotide sequences S:')
for i, x in enumerate(S):
    print('{i}: {x}'.format(i=i, x=x))
print('\n')


# Compute number of points

n = len(S)

# Compute distance matrix

D = np.zeros((n,n))
for i in range(n):
    for j in range(n):
        D[i,j] = hamming_dist(S[i], S[j])

print('Distance matrix D:')
print(D)
print('\n')

# Write distance matrix to file

np.savetxt("distance_matrix_phylogeny.csv", D, delimiter=",")

# Write result to screen
subprocess.run(['./ripser-representatives', 'distance_matrix_phylogeny.csv'])

# Write result to file
#with open('ripser_localization.txt', 'w') as file:
#    subprocess.run(['./ripser-representatives', 'distance_matrix.csv'], stdout=file)
