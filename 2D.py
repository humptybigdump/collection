###########################################################################
###                        2D Newton Raphson Code                       ###
### O. Stein, J. Rivas, N. Seubert (ITV)                        2010-12 ###
### Revised by M. Sontheimer (ITV)                              2018-01 ###
### Python version by D. Maerker (EBI-TFS)                      2024-08 ###
########################################################################### 

###########################################################################
###                                TODO                                 ###
### 1. Fix missing parts in the Ev2Fc function
### 2. Fix missing parts in the main 
### Missing parts are highlighted by <YOUR CODE HERE>
###########################################################################

###########################################################################
###                               IMPORTS                               ###
import numpy as np
import matplotlib.pyplot as plt
###########################################################################


###########################################################################
###                             FUNCTIONS                               ###

def CaDfDx(x, eps=1e-5):
    # preparations
    dfdx = np.zeros((2, 2))  # set shape of the Jacobian
    e = np.zeros(2)  # init epsilon array
    
    # calculate tolerance array
    eps = 1E-5  # set basic tolerance
    for j in range(2):  # loop over j
        if abs(x[j]) > 1.0:
            e[j] = eps * abs(x[j])  # epsj = |x_j|*eps for |x_j| >  1.0
        else:
            e[j] = eps  # epsj = eps for |x_j| <= 1.0
    
    # calculate Jacobian
    # 1st row: df1/dxj
    dfdx[0, 0] = (Ev2Fc(x[0] + e[0], x[1], 1) - Ev2Fc(x[0], x[1], 1)) / e[0]  # calc df1/dx1
    dfdx[0, 1] = (Ev2Fc(x[0], x[1] + e[1], 1) - Ev2Fc(x[0], x[1], 1)) / e[1]  # calc df1/dx2
    
    # 2nd row: df2/dxj
    dfdx[1, 0] = (Ev2Fc(x[0] + e[0], x[1], 2) - Ev2Fc(x[0], x[1], 2)) / e[0]  # calc df2/dx1
    dfdx[1, 1] = (Ev2Fc(x[0], x[1] + e[1], 2) - Ev2Fc(x[0], x[1], 2)) / e[1]  # calc df2/dx2
    
    return dfdx

def Ev2Fc(x1, x2, i):
    # Evaluate the 2D equation system

    ###########################################################################
    # TODO 1: Evaluation of functions f1 and f2                               #
    ###########################################################################

    if i == 1:
        f = # <YOUR CODE HERE>  # Define the function for f1 = f(x1, x2)
    elif i == 2:
        f = # <YOUR CODE HERE>  # Define the function for f2 = f(x1, x2)
    else:
        raise ValueError("No such function!")
    
    return f

def createPlots(xi, di, numIter):
    fig, ax = plt.subplots()
    xi = xi[:, :numIter]
    di = di[:, :numIter]
    iter = np.arange(1, numIter+1, 1)
    ax.plot(iter, xi[0, :], color='red', linestyle='-', label="$x_1$")
    ax.plot(iter, xi[1, :], color='blue', linestyle='-', label="$x_2$")
    ax.plot(iter, di[0, :], color='red', linestyle='--', label="$d_1$")
    ax.plot(iter, di[1, :], color='blue', linestyle='--', label="$d_2$")
    ax.set_xticks(iter)
    ax.set_xticklabels(iter)
    ax.set_xlabel('NR iteration')
    ax.set_ylabel('Function values $x_i$ and Deviations $\delta_i$')
    ax.legend(loc="upper right")
    fig.tight_layout()
    plt.show()

###########################################################################

###########################################################################
###                                MAIN                                 ###

# Note: "if __name__ == '__main__':" is not necessary, but it is good practice.
# This if-condition prevents the main part of the code to run in case its functions
# are imported to another script.
# Python automatically gives the script the runtime name __main__ if it is 
# the script that is called with "python <name_of_script>.py"
# You are encouraged to use it in your codes.

if __name__ == '__main__':
    
    # Set parameters
    x0 = np.array([-1.0, 1.0])  # set initial values
    tol = 1E-10  # set tolerance to stop iterating
    itmx = 100  # set max. no. of iterations
    LoDmp = True  # apply damping? (True/False)

    # Initializations
    x = x0  # set initial solution vector
    dl = np.ones(2) * 2 * tol  # set initial delta vector
    f = np.zeros(2)  # set initial RHS (shape only)
    xi = np.zeros((2, itmx + 1))  # set solution vector
    di = np.zeros((2, itmx + 1))  # set delta vector
    it = 0  # init iteration counter

    while (it < itmx) and (np.max(np.abs(dl)) > tol):  # loop until convergence

        ###########################################################################
        # TODO 2: Make the necessary changes to run the code.                     #
        ###########################################################################

        # Count iteration
        it = # <YOUR CODE HERE> 

        # Evaluate functions (fill these in with the actual equations)
        f[0] = # <YOUR CODE HERE>  # evaluate f1
        f[1] = # <YOUR CODE HERE>  # evaluate f2

        # Calculate Jacobian dfdx & deviation delta
        # Use a search engine of your choice to find numpy functions for 
        # matrix inversion and matrix vector multiplication. You'll need them!
        dfdx = # <YOUR CODE HERE>  # calc. Jacobian (nD)
        dl = # <YOUR CODE HERE>  # solve for delta

        # Move on to next NR iteration
        x = # <YOUR CODE HERE>  # update x

        # Store x, delta & norm
        xi[:, it] = x  # store x at current iteration it
        di[:, it] = dl  # store dl at current iteration it

        # Iteration output
        for neq in range(2):
            print(f"x{neq+1}: {x[neq]:8.6f}")
        print(f"it: {it:4d}, max(|delta|): {np.max(np.abs(dl)):4.2e}\n")

    # Output on NR success
    if it < itmx:
        print('\nNR iteration finished, convergence.\n')
    else:
        print('\nNR iteration finished, itmx reached!\n')
    
    createPlots(xi, di, it)