###########################################################################
###                        Well Stirred Reactor I                       ###
### M. Sontheimer (ITV)                                         2020-04 ###
### Python version by D. Maerker (EBI-TFS)                      2024-08 ###
########################################################################### 


###########################################################################
###                               IMPORTS                               ###
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.colors as mcolors
import cantera as ct
import i2rf.wsr2 as pp
###########################################################################

###########################################################################
###                              Cantera                                ###
class Mixture():
    def __init__(self) -> None:
        self.gas = ct.Solution('h2o2.yaml')
        self.h_in = -1

    def setInletState(self, Phi)->list:
        # Inlet Temperature in K
        T_in = 298.0

        # Inlet pressure in Pa
        P_in = 101325.0

        # Inlet composition
        mrN2    = 3.76          # molar ratio of N2 in air [-] 
        nuAirSt = 0.5           # stoichiometric air coefficient [-]
        ###########################################################################
        # TODO 1: Set the inlet composition (based on Phi).                       #
        # Lean or stoichiometric conditions (Phi<=1) are assumed!                 #
        #                                                                         #
        # H2 + nuAir*(O2 + mrN2*N2) -> H2O + (nuAir-0.5)*O2 + nuAir*mrN2*N2       #
        ###########################################################################
        nuAir = nuAirSt/Phi     # calculate air coefficient from Phi
        
        # Stoichiometric coefficients for each species
        nuH2 = # <YOUR CODE HERE>
        nuO2 = # <YOUR CODE HERE>
        nuN2 = # <YOUR CODE HERE>

        # Set gas composition
        self.gas.X = {'H2':nuH2, 
                      'O2':nuO2, 
                      'N2':nuN2}

        # Set gas temperature 
        self.gas.TP = T_in, P_in

        # Calculate enthalpy of the inlet state and store as in the mixture class
        self.h_in = self.gas.h

        return [self.gas.Y, T_in, P_in]

    def getReactionRate(self, Y, T, P_in):
        reactingGas = ct.Solution('h2o2.yaml')
        reactingGas.TPY = T, P_in, Y
        return reactingGas.net_production_rates # in kmol/(m3 s)

    def getEnthalpy(self, Y, T, P_in):
        reactingGas = ct.Solution('h2o2.yaml')
        reactingGas.TPY = [T, P_in, Y]
        return reactingGas.h
    
    def getMolecularWeights(self):
        reactingGas = ct.Solution('h2o2.yaml')
        return reactingGas.molecular_weights

    def getGasDensity(self, Y, T, P):
        reactingGas = ct.Solution('h2o2.yaml')
        reactingGas.TPY = [T, P, Y]
        return reactingGas.density
    
###########################################################################

###########################################################################
###                             FUNCTIONS                               ###

def setInitialGuess()->list:
    '''
    Sets the initial guess required for the Newton-Raphson method
    '''
    # Create dummy mixture with inlet composition
    dummyMix = Mixture()
    dummyMix.setInletState(1)
    # Calculate Equilibrium state with constant enthalpy and pressure
    dummyMix.gas.equilibrate('HP') # Updates the state of gas in dummyMix to equilibrium

    # Return equilibrium composition and temperature
    return [dummyMix.gas.Y, dummyMix.gas.T]
    
    # Return Values
    # return [Y0, T0]

def setSolverOptions(mode = 1)->dict:
    '''
    Set the solver options and returns them as a dictionary
    '''
    options = {}
    dummyGas = Mixture()
    nSpecies = len(dummyGas.gas.Y)
    if mode == 1:
        options['tol'] = 1e-7
        options['iterMax'] = 100
        options['damping'] = False
    elif mode == 2:
        options['tol'] = 1e-7
        options['iterMax'] = 100
        options['damping'] = True
    elif mode == 3:
        options['tol'] = 1e-7
        options['iterMax'] = 100
        options['damping'] = True
        # Create upper and lower bounds for 
        options['q_lb'] = np.zeros(nSpecies+1)
        options['q_lb'][-1] = 298.0
        options['q_ub'] = np.ones(nSpecies+1)
        options['q_ub'][-1] = 5000.0
    elif mode == 4:
        options['tol'] = 1e-12
        options['iterMax'] = 1000
        options['damping'] = True
        # Create upper and lower bounds for 
        options['q_lb'] = np.zeros(nSpecies+1)
        options['q_lb'][-1] = 298.0
        options['q_ub'] = np.ones(nSpecies+1)
        options['q_ub'][-1] = 5000.0
    else:
        raise ValueError(f'This mode ({mode}) is not available.')

    return options

def calcRHS(q:np.ndarray,Y_in:np.ndarray,h_in:float,P_in:float,mDot:float,V, mix:Mixture)->np.ndarray:
    '''
    Calculation of the right-hand side of the equation system
    '''
    # Get data from vector q
    Y = q[0:-1]
    T = q[-1]

    ###########################################################################
    # TODO 2: Implement the WSR equations.                                    #
    #                                                                         #
    # Step 1: Set the variables RR, h and MW                                  #
    #         Their values can be obtained using the mixture object mix       #
    #         Take a look into the Mixture class!                             #
    # Hint: The self argument in a class function is passed automatically by  #
    # the class object you're calling the function with.                      #
    #                                                                         #
    # Step 2: Use the values in the calculation of the RHS                    #
    ########################################################################### 

    # Calculate Reaction Rate
    RR = # <YOUR CODE HERE>

    # Calculate enthalpy
    h = # <YOUR CODE HERE>
    
    # Get molecular weights
    MW = # <YOUR CODE HERE>

    # Calculate RHS
    rhs_Y = np.zeros_like(Y) # Initialisation
    rhs_Y = # <YOUR CODE HERE>
    rhs_T = # <YOUR CODE HERE>
    
    # Return RHS
    return np.array([*rhs_Y, rhs_T])

def NewtonRaphson(calcRHS_,q0:np.ndarray,options:dict)->np.ndarray:
    '''
    Newton-Raphson method for solving a system of non-linear equations. 
    The implementation follows the instructions given in Turns (2000).
    '''
    # Get number of variables to solve
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
            J += 1e-20*np.identity(nVars)
            delta = np.matmul(-1*np.linalg.inv(J), f)

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

        # Terminate calculation if temperature or concentrations are below 0
        if any(q < 0):  
            raise ValueError("Non-physical results -> no convergence, change solver mode!")

        print(f'Iteration {i:4d} : maximum residual = {maxRes:.4e}')
    
    # Check if iteration was successful
    if maxRes <= tol: 
        print(f'Solution successfully converged after {i} iterations!')
        return q
    else:
        Warning(f'Solution did NOT converge in NR method after {i} iterations with tolerance {tol:3.2e}!')
    return q

def calcResidenceTime(Y, T, P, mDot, V)->float:

    # create
    dummyMix = Mixture()

    ###########################################################################
    # TODO 3: Calculate the residence time tR.                                #
    #         Make use of the function parameters and dummyMix object         #
    ###########################################################################

    # Get gas density
    rho = dummyMix.# <YOUR CODE HERE>

    # Calculate residence time in s
    tR = # <YOUR CODE HERE>

    return tR

def plotTemperature(tR, T)->None:

    data = pp.i2rf()
    _, refTemp = data.getTemperature()
    _, refTR = data.getResidenceTime()

    fig, ax = plt.subplots()
    
    ax.plot(tR, T, color='red', linestyle='', marker="x", label="Calculated")
    ax.plot(refTR, refTemp, color='black', linestyle='-', marker="", label="Reference")

    ax.set_xlim(1e-5, 1)
    ax.set_ylim(1000, 2500)

    ax.set_xticks([1e-5, 1e-4, 1e-3, 1e-2, 1e-1, 1])
    ax.set_yticks(np.arange(1000, 2501, 500))

    ax.set_xscale('log')

    ax.set_xlabel('Residence time $t_R$ in s')
    ax.set_ylabel('Temperature $T$ in K')
    # ax.legend()
    fig.tight_layout()
    ax.grid()
    # fig.savefig("04_WellStirredReactor2/Plots/tR_T.png")
    plt.show()

def plotMassFractions(mDot, Y, species:list=['H2O', 'H2', 'O2', 'OH'])->None:

    data = pp.i2rf()
    mDot_ref, refSpecies = data.getSpecies(['H2O', 'H2', 'O2', 'OH'])

    dummyMix = Mixture()
    speciesNames = dummyMix.gas.species_names
    canteraIds = [speciesNames.index(x) for x in species]

    fig, ax = plt.subplots()
    
    for i, s in enumerate(species):
        color_ = list(mcolors.TABLEAU_COLORS.values())[i]
        ax.plot(mDot, Y[:, canteraIds[i]], color=color_, linestyle='', marker="x", label=f"{s}")
        ax.plot(mDot_ref, refSpecies[:, i], color=color_, linestyle='-', marker="")

    ax.set_xlim(1e-5, 1)
    # ax.set_ylim(0, 1)

    ax.set_xticks([1e-5, 1e-4, 1e-3, 1e-2, 1e-1, 1])
    # ax.set_yticks(np.arange(1000, 2501, 500))

    ax.set_xscale('log')

    ax.set_xlabel('Mass flow rate in kg/s')
    ax.set_ylabel('Mass fraction')
    ax.legend()
    fig.tight_layout()
    ax.grid()
    # fig.savefig("04_WellStirredReactor2/Plots/mDot_Y.png")
    plt.show()


###########################################################################



###########################################################################
###                                MAIN                                 ###
if __name__ == "__main__":
    # Reactor volume [m^3]
    V = 67.4e-06

    # Mass flow rates in kg/s
    mDot = [5e-4]
    # mDot = np.array([1e-5, 1e-4, 1e-3, 1e-2, 1e-1, 0.95]) 
    # Equivalence ratio [-]
    Phi = 1.0

    # Create gas mixture object from Cantera Class
    mix = Mixture()

    # Set Inlet State
    Y_in, T_in, P_in = mix.setInletState(Phi)

    # Solver options
    ###########################################################################
    # TODO 4: Change the number to choose a different set of options          #
    ###########################################################################
    options = setSolverOptions(mode = 1)

    # Allocation
    Y = np.zeros((len(mDot), len(Y_in)))
    T = np.zeros_like(mDot)
    tR = np.zeros_like(mDot)

    # Set initial state
    Y0 = np.zeros_like(Y_in)
    Y0, T0 = setInitialGuess()

    # Main calculation loop
    for i in range(len(mDot)):
        print(f'Solving WSR equations for mDot = {mDot[i]:12.4e} kg/s')

        # Calling the Newton Raphson method
        q = NewtonRaphson(lambda q: calcRHS(q, Y_in, mix.h_in, P_in, mDot[i], V, mix),np.array([*Y0, T0]),options)
        # Information regarding the lambda function:
        # A lambda function is a nameless function, in this case it is used to 
        # provide access to a more complex function (calcRHS) while only requiring
        # one parameter (q). The remaining parameters are taken form the main function
        # and must not be transferred to the NewtonRaphson (NR) function in order to be used
        # in the call of the calcRHS function inside the NR function.

        # Save results
        Y[i, :] = q[0:-1]
        T[i] = q[-1]

        # Calculate residence time
        tR[i] = calcResidenceTime(Y[i, :], T[i], P_in, mDot[i], V)
        
        # Text output
        print(f'T = {T[i]:.2f} K')
        print(f"tR = {tR[i]:.4e}")
        print(f'Done {i+1} of {len(mDot)}')
        print(f"Y of all species: {Y[i, :]}")
        # plotWSR_states(Y_in,T_in,Y0,T0,Y,T,mDot[i],par, i)
        print('================================================\n')
        
    plotTemperature(tR, T)
    plotMassFractions(mDot, Y)


