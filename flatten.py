matrix = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
print(matrix) # Output: [[1, 2, 3], [4, 5, 6], [7, 8, 9]]

# Using a for loop to flatten the matrix
flattened = []
for row in matrix:
    for x in row:
        flattened.append(x)
print(flattened)

# Using list comprehension
flattened_list_comprehension = [x for row in matrix for x in row]
print(flattened_list_comprehension)