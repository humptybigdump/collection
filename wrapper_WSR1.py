###########################################################################
###                        Well Stirred Reactor I                       ###
### M. Sontheimer (ITV)                                         2020-04 ###
### Python version by D. Maerker (EBI-TFS)                      2024-08 ###
########################################################################### 


###########################################################################
###                               IMPORTS                               ###
import numpy as np
import matplotlib.pyplot as plt
# Importing every function from WSR1
# Make sure WSR1 is in the same directory as this wrapper (or adjust the import accordingly)
from WSR1 import * 
###########################################################################

###########################################################################
###                                MAIN                                 ###
if __name__ == '__main__':
    # Set Phi range
    Phi_all = # <YOUR CODE HERE>

    # Set mDot range
    mDot = # <YOUR CODE HERE>

    mDot_bo_all = np.zeros_like(Phi_all)

    for p in range(len(Phi_all)):
        Phi = Phi_all[p]

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
            print('================================================\n')

        if bo:
            print(f'Blow-off occured at mDot = {mDot_bo:.4e} kg/s')
        else:
            print('Blow-off did not occur')
   
        mDot_bo_all[p] = mDot_bo
    
    plotWSR_bo(mDot_bo_all, Phi_all)
###########################################################################