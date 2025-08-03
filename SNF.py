# Computation of the Smith normal form
# Lecture course "Topological Data Analysis"
# Exercise 35
# KIT  WS 20/21

# Import Python numpy package

import numpy as np

# Define functions

def pivot(A):

    # Get number of rows (m) and columns (n) of A
    (m, n) = np.shape(A)

    if A[0,0] == 0:

        # Find pivot element
        pivot = []

        # Find pivot element in first row
        j=1
        while pivot == [] and j < n:
            if A[0,j] != 0:
                pivot = [0,j]
            j+=1

        # Find pivot element in first column
        i=1
        while pivot == [] and i < m:
            if A[i,0] != 0:
                pivot = [i,0]
            i+=1

        # If pivot element in first row, apply row operation of type (R I)
        if pivot[0] != 0:
            S = np.eye(m)
            S[0, pivot[0]] = 1
            S[pivot[0], 0] = 1
            S[0,0] = 0
            S[pivot[0], pivot[0]] = 0
            A = np.dot(S, A)

        # If pivot element in first column, apply column operation of type (C I)
        if pivot[1] != 0:
            T = np.eye(n)
            T[0, pivot[1]] = 1
            T[pivot[1], 0] = 1
            T[0,0] = 0
            T[pivot[1], pivot[1]] = 0
            A = np.dot(A, T)

    return A


def normalize(A):

    # Get number of rows (m) and columns (n) of A
    (m, n) = np.shape(A)

    # Apply row operation of type (R II)
    S = np.eye(m)
    S[0, 0] = 1/A[0, 0]
    A = np.dot(S, A)

    return A


def reduce(A):

    # Get number of rows (m) and columns (n) of A
    (m, n) = np.shape(A)

    for i in range(1, m):
        # Apply row operation of type (R III)
        S = np.eye(m)
        S[i, 0] = -A[i, 0]
        A = np.dot(S, A)

    for j in range(1, n):
        # Apply column operation of type (C III)
        T = np.eye(n)
        T[0, j] = -A[0, j]
        A = np.dot(A, T)

    return A


# Run main program

# Example matrix

A = np.array([
    [  2,   6,   4],
    [ -3,  -7,   2],
    [  1,   5,   10]
])

# The boundary matrices from Exercise 37

A_1 = np.array([[-1, 0, 1, 0, 0, 0, 0, 0, 0, -1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, -1, 0, 0], [1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 1, 0, 0, 0, -1, 0, 0, 0, 0, 1, 0, 0, 0], [0, 1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 1, 0, 0, 0, -1, 0, 0, 0, 0, 1], [0, 0, 0, -1, 0, 1, 0, 0, 0, 1, -1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, -1, 0], [0, 0, 0, 1, -1, 0, 0, 0, 0, 0, 0, 0, 1, -1, 0, 0, 0, 0, 0, -1, 0, 1, 0, 0, 0, 0, 0], [0, 0, 0, 0, 1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, -1, 0, 0, 0, 0, 0, -1, 0, 1, 0, 0], [0, 0, 0, 0, 0, 0, -1, 0, 1, 0, 1, -1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, -1], [0, 0, 0, 0, 0, 0, 1, -1, 0, 0, 0, 0, 0, 1, -1, 0, 0, 0, 0, 0, -1, 0, 1, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0, 0, 1, -1, 0, 0, 0, 0, 0, 0, 0, 1, -1, 0, 0, 0, 0, 0, -1, 0, 1, 0]])

A_2 = np.array([[1, 0, 0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, -1, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, -1], [0, -1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0, 0, -1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 1, 0, 0, 0], [0, 0, 0, -1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 1, 0, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 1, 0], [-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0], [0, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0], [0, 0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1], [0, 1, 0, 0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], [0, 0, 0, 1, 0, 0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 1, 0, 0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, -1, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, -1, 0, 0, 0], [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, -1, 0], [1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], [0, 0, 1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0, 1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0, 0, 0, 1, -1, 0, 0, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, -1, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, -1, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, -1, 0, 0], [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, -1]])

A = A_1

print("\nInput matrix A: \n")
print(A)

# Get number of rows (m) and columns (n) of A
(m, n) = np.shape(A)

SNF = A

while A.size > 0 and not np.all(A==0):
#    print('------------------------')
#    print(m, n)
#    print('\n')
#    print(A)
    A = pivot(A)
    A = normalize(A)
    A = reduce(A)
    SNF[-m:, -n:] = A
    A = A[1:, 1:]
    (m, n) = np.shape(A)

print("\nSmith normal form of A: \n")
print(SNF)
print("\n")
