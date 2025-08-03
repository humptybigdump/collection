import numpy as np
import math

#### EXERCISE 1 ####

# (a)
print('Exercise (1a):')
a = 0.1 + 0.1 + 0.1
b = 0.3

# (b)
print('\nExercise (1b):')
c = 0.1
d = 0.10000000000000001

# (c)
print('\nExercise (1c):')
e = 1 + 10**(-20)
f = 10**(-20)


###############################################################################


#### EXERCISE 2 ####

A = [1,2,3,4,5,6]

# (a) Summation by partial sums using a for loop
def get_sum(summands):
    pass
print('\nExercise (2a)')
print(sum(A)==get_sum(A))

# (b) Summation by partial sums with a recursive function
def get_recursive_sum(summands):
    pass
print('\nExercise (2b)')
print(sum(A)==get_recursive_sum(A))

# (c) Pairwise summation
def get_pairwise_sum(summands):
    pass
print('\nExercise (2c)')
print(sum(A)==get_recursive_sum(A))

# (d) Compare get_sum, get_pairwise_sum, sum and np.sum
print('\nExcercise (2d): ')
B = 10**(-6)*np.ones(10**6,dtype='float64')
print('Own partial sum: ',get_sum(B))
print('Own pairwise sum: ',get_pairwise_sum(B))
print('Pythons partial sum: ',sum(B))
print('Numpys pairwise sum: ',np.sum(B))