def newton_1D(f, x0, tol = 1e-6, maxiter = 100):
    """
        Newton-Raphson method for finding roots of a function.
        
        Parameters:
        f : function
            The function for which we are trying to find a root.
        x0 : float
            Initial guess for the root.
        tol : float
            Tolerance for convergence.
        max_iter : int
            Maximum number of iterations.
        
        Returns:
        float
            The estimated root.
    """
    dx = 1e-6
    
    x = x0

    for i in range(maxiter):
        
        fx  = f(x)

        # Numerical derivative
        dfx = ( f(x+dx) - f(x) ) / dx
        
        if abs(fx) < tol:
            print(f"Function converged in {i} iterations")
            print(f"Function value is: {fx}")
            return x

        x = x - fx/dfx
    raise ValueError("Maximum number of iterations reached. No Solution found.")







