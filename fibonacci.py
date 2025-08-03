def fibonacci(n):
    """ Return fibonacci sequence for n """
    a, b = 0, 1
    for _ in range(n):
        print(a, end=' ')
        a, b = b, a + b
  
# Call fibonacci
n = 10
print(f"fibonacci sequence for {n}:")          
fibonacci(10)
  