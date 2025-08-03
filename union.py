# Define two sets
set1 = {1, 2, 3, 4, 5}
set2 = {4, 5, 6, 7, 8}

# Find the union of all sets
union_set = set.union(set2)
# or union_set = set1 | set2
print(f"Union of all sets: {union_set}")

# Find the intersection of all sets
intersection_set = set1.intersection(set2)
# or intersection_set = set1 & set2
print(f"Intersection of all sets: {intersection_set}")

# Find the elements that are unique to set1
difference_set = set1.difference(set2)
# or difference_set = set1 - set2
print(f"Elements unique to set1: {difference_set}")

# Find the elements that are in set1 but not in set 2, or in set 2 and not in set 1
symmetric_difference_set = set1.symmetric_difference(set2)
# symmetric_difference_set = set1 ^ set2
print(f"Elements in set1 and set2: {symmetric_difference_set}")
