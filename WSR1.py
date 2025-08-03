###########################################################################
###                        Well Stirred Reactor I                       ###
### M. Sontheimer (ITV)                                         2020-04 ###
### Python version by D. Maerker (EBI-TFS)                      2024-08 ###
########################################################################### 


###########################################################################
###                               IMPORTS                               ###
import numpy as np
import matplotlib.pyplot as plt
###########################################################################


###########################################################################
###                             FUNCTIONS                               ###

def parSet(iFu:int,iOx:int,iPr:int,VReac,Ru,MW,cp,hf0_Fu,A2F_ST)->np.ndarray:
    '''
    Creates and returns vector par from individual parameters
    '''
    return np.array([iFu,iOx,iPr,VReac,Ru,MW,cp,hf0_Fu,A2F_ST])

def parGet(par:np.ndarray)->list:
    '''
    Returns individual parameters from vector par
    '''
    l = par.tolist()
    l[0] = int(l[0])
    l[1] = int(l[1])
    l[2] = int(l[2])
    return l

def setInletState(Phi:float, par:np.ndarray)->list:
    '''
    Sets the inlet state
    '''
    # Get parameter for par vector
    iFu,iOx,iPr,_,_,_,_,_,A2F_ST = parGet(par)

    # Set inlet temperature
    T_in = 298.0

    # Set inlet pressure
    p_in = 101325.0

    # Inlet composition
    A2F = A2F_ST/Phi
    Y_in = np.zeros(3)
    Y_in[iFu] = 1.0/(1.0+A2F)
    Y_in[iOx] = 1.0-Y_in[iFu]
    Y_in[iPr] = 0.0

    # Return Values
    return [Y_in, T_in, p_in]

def setInitialGuess(Phi:float, par:np.ndarray)->list:
    '''
    Sets the initial guess required for the Newton-Raphson method
    '''
    
    # Get parameters
    [iFu,iOx,iPr,_,_,_,_,_,_] = parGet(par)

    # Set mass fractions
    Y0 = np.zeros(3)
    if Phi<0.9:
        # Small  Phi => more Ox
        Y0[iFu] = 0.00
        Y0[iOx] = 0.20
        Y0[iPr] = 1-Y0[iFu]-Y0[iOx]
    else:
        # Larger Phi => less Ox
        Y0[iFu] = 0.05
        Y0[iOx] = 0.15
        Y0[iPr] = 1-Y0[iFu]-Y0[iOx]
    
    # Set temperature
    T0 = 2000.0
    
    # Return Values
    return [Y0, T0]

def setSolverOptions()->dict:
    '''
    Set the solver options and returns them as a dictionary
    '''
    options = {}
    options['tol'] = 1e-7
    options['iterMax'] = 100
    options['damping'] = True
    # Create upper and lower bounds for 
    options['q_lb'] = np.zeros(4)
    options['q_lb'][-1] = 298.0
    options['q_ub'] = np.ones(4)
    options['q_ub'][-1] = 5000.0

    return options

def calcRHS(q:np.ndarray,Y_in:np.ndarray,T_in:float,P:float,mDot:float,par:np.ndarray)->np.ndarray:
    '''
    Calculation of the right-hand side of the equation system
    '''
    # Get data from vector q
    Y = q[0:-1]
    Y = np.minimum(np.maximum(Y,0.0),1.0)
    T = q[-1]

    # Get parameters
    [iFu,iOx,iPr,VReac,Ru,MW,cp,hf0_Fu,A2F_ST] = parGet(par)

    ###########################################################################
    # TODO 1: Implement the WSR equations.                                    #
    ###########################################################################

    # Calculate the fuel reaction rate
    omg_Fu = # <YOUR CODE HERE>
    # Calculate RHS
    rhs_Y = np.zeros_like(Y)
    rhs_Y[iFu] = # <YOUR CODE HERE>
    rhs_Y[iOx] = # <YOUR CODE HERE>
    rhs_Y[iPr] = # <YOUR CODE HERE>
    rhs_T = # <YOUR CODE HERE>

    # Return RHS
    return np.array([*rhs_Y, rhs_T])

def NewtonRaphson(calcRHS_,q0:np.ndarray,options:dict):
    '''
    Newton-Raphson method for solving a system of nonlinear equations. 
    The implementation follows the instructions given in Turns (2000).
    '''
    # Get numer of variables to sovle
    nVars = len(q0)

    # Set solver options (if available) or use defaults
    # dictionary.get(key, <default value if key does not exist>)
    tol = options.get('tol', 1e-7)
    iterMax = options.get('iterMax', 100)
    damping = options.get('damping', True)
    q_lb = options.get('q_lb', -np.inf*np.ones_like(q0))
    q_ub = options.get('q_ub', np.inf*np.ones_like(q0))

    # Iteration
    q = np.copy(q0)
    f = calcRHS_(q) # Check the main function as for why calcRHS_(q) is used instead of calcRHS(...)
    norm = np.sum(np.abs(f))
    i = 0
    maxRes = np.inf

    # Iteration loop
    while i < iterMax and maxRes > tol:
        i += 1
        # Calculate Jacobian
        J = np.zeros((nVars, nVars))
        for j in range(nVars):
            eps = max(1e-5, 1e-5*abs(q[j]))
            q_ = np.copy(q)
            q_[j] += eps
            f_ = calcRHS_(q_)
            J[:, j] = (f_-f)/eps

        # Solve for new solution vector (without dampening)
        try:
            delta = np.matmul(-1*np.linalg.inv(J), f)
        except:
            print('#'*10 + "WARNING" + "#"*10)
            print('Singular Matrix found. Returning last solution for q!')
            print('#'*10 + "WARNING" + "#"*10)
            return q

        # Advance q
        q_ = q+delta
        q_ = np.minimum(np.maximum(q_, q_lb), q_ub)
        f_ = calcRHS_(q_)
        norm_ = np.sum(np.abs(f_))

        # Apply dampening
        if damping and norm_ > norm:
            delta = 0.2 * delta
            q = q+delta
            q = np.minimum(np.maximum(q, q_lb), q_ub)
        else: 
            q = q_
        
        # Update RHS and norm
        f = calcRHS_(q)
        norm = np.sum(np.abs(f)) 

        # Check residuals
        res = np.abs(delta)
        for k in range(nVars):
            if abs(q[k]) > tol:
                res[k] = abs(delta[k]/q[k])
        maxRes = np.max(res)

        print(f'Iteration {i:4d} : maximum residual = {maxRes:.4e}')
    
    # Check if iteration was successful
    if maxRes <= tol: 
        print(f'Solution successfully converged after {i} iterations!')
        return q
    else:
        Warning(f'Solution did NOT converge in NR method after {i} iterations with tolerance {tol:3.2e}!')
    return q

def plotWSR_bo(mDot_bo_all, Phi_all):
    '''
    Plots the equivalance ratio over mDot_bo
    '''

    # Reference data (Turns, Fig. 6.8)
    mDot_bo_ref = [0.0781, 0.0950, 0.1122, 0.1287, 0.1452, 0.1633, 0.1794, 0.1888, 0.1928, 0.1935]
    Phi_ref     = [0.5484, 0.5722, 0.5987, 0.6278, 0.6595, 0.7045, 0.7600, 0.8261, 0.9054, 0.9874]

    fig, ax = plt.subplots()
    
    ax.plot(mDot_bo_ref, Phi_ref, color='gray', linestyle='-', marker="x", label="Reference from Turns (2000)")
    ax.plot(mDot_bo_all, Phi_all, color='red', linestyle='-', marker="o", label="Calculated")

    ax.set_xlim(0, 0.25)
    ax.set_ylim(0, 1.2)

    ax.set_xticks(np.arange(0, 0.25, 0.05))
    ax.set_yticks(np.arange(0, 1.2, 0.2))

    ax.set_xlabel('blow-off mass flow rate in kg/s')
    ax.set_ylabel('Equivalence ratio $\Phi$')
    ax.legend()
    fig.tight_layout()
    ax.grid()
    plt.show()

def plotWSR_mDot(mDot,Y,T,par):
    [iFu,iOx,iPr,_,_,_,_,_,_] = parGet(par)

    fig, ax = plt.subplots()
    
    ax.plot(mDot, T/1000, color='black', linestyle='-', marker="x", label="T/1000")
    ax.plot(mDot, Y[:, iFu]*10, color='red', linestyle='--', marker="o", label="$Y_{Fu}*10$")
    ax.plot(mDot, Y[:, iOx]*0.233*10, color='blue', linestyle='--', marker="o", label="$Y_{Ox}*10$")
    ax.plot(mDot, Y[:, iPr], color='green', linestyle='--', marker="o", label="$Y_{Pr}$")
    

    ax.set_xlim(0, 0.25)
    ax.set_ylim(0, 2.5)

    ax.set_xticks(np.arange(0, 0.25, 0.05))
    ax.set_yticks(np.arange(0, 2.5, 0.5))

    ax.set_xlabel('mass flow rate in kg/s')
    ax.set_ylabel('T in K, $Y_i$')
    ax.legend()
    fig.tight_layout()
    ax.grid()
    plt.show()

def plotWSR_states(Y_in,T_in,Y0,T0,Y,T,mDot,par, iter):
    [iFu,iOx,iPr,_,_,_,_,_,_] = parGet(par)
    x = [0.5, 1.5, 2.5]

    fig, ax = plt.subplots()

    ax.plot(x, np.array([T_in, T0, T[iter]])/1000, color='black', linestyle='', marker="x", label="T/1000")
    ax.plot(x, np.array([Y_in[iFu], Y0[iFu], Y[iter, iFu]])*10, color='red', linestyle='', marker="o", label="$Y_{Fu}*10$")
    ax.plot(x, np.array([Y_in[iOx], Y0[iOx], Y[iter, iOx]])*0.233*10, color='blue', linestyle='', marker="o", label="$Y_{Ox}*10$")
    ax.plot(x, np.array([Y_in[iPr], Y0[iPr], Y[iter, iPr]]), color='green', linestyle='', marker="o", label="$Y_{Pr}$")


    ax.set_xlim(0, 3)
    ax.set_ylim(0, 3)

    # ax.set_xticks(np.arange(0, 0.25, 0.05))
    ax.set_yticks(np.arange(0, 2.5, 0.5))

    ax.text(0.5, 2.7, 'Inlet state', horizontalalignment='center')
    ax.text(1.5, 2.7, 'Initial state', horizontalalignment='center')
    ax.text(2.5, 2.7, 'Final state', horizontalalignment='center')

    ax.set_xlabel('')
    ax.set_ylabel('T in K, $Y_i$')
    ax.legend()
    fig.tight_layout()
    ax.grid()
    plt.show()        

###########################################################################

###########################################################################
###                                MAIN                                 ###
if __name__ == '__main__':
    # Input parameters:
    # Species indices
    iFu = 0 
    iOx = 1 
    iPr = 2

    # Reactor diameter [m] and volume [m^3]
    DReac = 80.0E-03
    VReac = (np.pi/6.0)*DReac**3

    # Thermodynamic data
    Ru     = 8314.4626  # Universal gas constant [J/kmol-K]
    MW     = 29.0       # Molecular weight [kg/kmol]
    cp     = 1200.0     # Heat capacity [J/kg-K]
    hf0_Fu = 4.0e7      # Enthalpy of formation of the fuel [J/kg]
    A2F_ST = 16.0       # Stoichiometric air-fuel ratio [-]
    
    # Equivalence ratio [-]
    ###########################################################################
    # TODO 3.1: Vary the equivalence ratio                                    #
    ###########################################################################
    Phi = 1.0

    # Mass flow rates [kg/s]
    ###########################################################################
    # TODO 2 (and 3.2): Vary the mass flow rate(s)                            #
    ###########################################################################
    mDot = [0.01]

    # Initialisation
    # Save all parameters in one vector
    par = parSet(iFu,iOx,iPr,VReac,Ru,MW,cp,hf0_Fu,A2F_ST)

    # Set inlet state
    [Y_in,T_in,P] = setInletState(Phi,par)

    # Set the initial guess for the Newton-Raphson method
    [Y0,T0] = setInitialGuess(Phi,par)

    # Set solver options
    options = setSolverOptions()

    # Initialise fields to store the results
    Y = np.zeros((len(mDot),3))
    T = np.zeros(len(mDot))

    # Initialise blow-off parameters
    bo = False
    mDot_bo = -1.0

    for i in range(len(mDot)):
        print(f'Solving WSR equations for mDot = {mDot[i]:12.4e} kg/s')

        # Calling the Newton Raphson method
        q = NewtonRaphson(lambda q: calcRHS(q,Y_in,T_in,P,mDot[i],par),np.array([*Y0, T0]),options)
        # Information regarding the lambda function:
        # A lambda function is a nameless function, in this case it is used to 
        # provide access to a more complex function (calcRHS) while only requiring
        # one parameter (q). The remaining parameters are taken form the main function
        # and must not be transferred to the NewtonRaphson (NR) function in order to be used
        # in the call of the calcRHS function inside the NR function.

        # Save results
        Y[i, :] = q[0:-1]
        T[i] = q[-1]

        # Check for blow off
        if not bo and abs(T[i]-T_in)<1e-3:
            bo = True
            mDot_bo = mDot[i]
        
        # Text output
        print(f'T = {T[i]:.2f} K')
        print(f'Done {i+1} of {len(mDot)}')
        # You might want to disable this plot function when calculating multiple mDot 
        plotWSR_states(Y_in,T_in,Y0,T0,Y,T,mDot[i],par, i)
        print('================================================\n')

    if bo:
        print(f'Blow-off occured at mDot = {mDot_bo:.4e} kg/s')
    else:
        print('Blow-off did not occur')

    plotWSR_mDot(mDot,Y,T,par)
