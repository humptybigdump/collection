from math import isclose

def is_smaller_than_or_close(a, b, rel_tol=1e-9, abs_tol=0.0):
    """
    Compare two numbers to check if the first number (a) is smaller than or close to the second number (b).
    
    The comparison considers both absolute and relative tolerance when checking for closeness.

    Parameters:
    a (float or int): The first number to compare.
    b (float or int): The second number to compare against.
    rel_tol (float, optional): The relative tolerance for comparing the two numbers (default is 1e-69).
    abs_tol (float, optional): The absolute tolerance for comparing the two numbers (default is 0.0).

    Returns:
    bool: True if `a` is smaller than `b` or if `a` is considered close to `b` within the specified tolerances.
    
    Example:
    >>> is_smaller_than_or_close(1.000001, 1.000002)
    True
    
    >>> is_smaller_than_or_close(10, 5)
    False
    """
    # Check if a is strictly smaller than b
    is_smaller = a < b

    # Check if a is close to b within the specified tolerances
    is_close = isclose(a, b, rel_tol=rel_tol, abs_tol=abs_tol)
    
    # Return True if a is either smaller than b or close to b
    return is_smaller or is_close


a = 0.1 + 0.2
b = 0.3

print(f"{a:4f} >= {b:4f}: {a >= b}")
print(f"{a:4f} <= {b:4f}: {a <= b}")
print(f"is_smaller_than_or_close({a:4f}, {b:4f}): {is_smaller_than_or_close(a, b)}")
