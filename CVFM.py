###########################################################################
###                Constant Volume Fixed Mass Reactor                   ###
### O. Stein, J. Rivas, N. Seubert (ITV)                        2010-1 ###
### M. Sontheimer (ITV)                                         2019-01 ###
### Python version by D. Maerker (EBI-TFS)                      2024-12 ###
########################################################################### 


###########################################################################
###                               IMPORTS                               ###
import numpy as np
import matplotlib.pyplot as plt

###########################################################################


###########################################################################
###                             FUNCTIONS                               ###

def calcInitialChi(par_thermo, par_global)->np.ndarray:

    iFu, iOx, iPr, _, _, _, _, nSpc = par_global
    _, _, _, _, A2F_ST, Phi, _, _ = par_thermo

    ch0 = np.zeros(nSpc)
    ch0[iFu] = 1.0/(A2F_ST/Phi+1.0)
    ch0[iOx] = 1.0 - ch0[iFu]
    ch0[iPr] = 0.0

    return ch0

def isentropicCompression(par_isentropic):

    T_BDC, P_BDC, gamma, r_compr = par_isentropic

    TP_TDC = np.zeros(2)

    TP_TDC[0] = T_BDC*r_compr**(gamma-1)    # Temperature
    TP_TDC[1] = P_BDC*r_compr**(gamma)      # Pressure

    return TP_TDC

def calcChi2MolConc(Chi,Tm,Ps,Ru):
    return (Chi * Ps) / (Ru * Tm);

def calcDFuDt(Tm,mcFu,mcOx):
    
    m   = 0.1
    n   = 1.65
    Pre = 6.19E9
    Ta  = 15098.0

    ###########################################################################
    # TODO 2a: Implement the calculation dFu/dt                               #
    # Use the given parameter above.                                          #
    ###########################################################################

    #<YOUR CODE HERE>

    return dFuDt

def calcDTmDt(Tm,dFuDt,h0fF,cp,Ru,Ps):

    ###########################################################################
    # TODO 2b: Implement the calculation dTm/dt                               #
    ###########################################################################

    #<YOUR CODE HERE>

    return dTmDt

def calcPsOfTm(Tm,dTmDt,Tm0,Ps0):

    PsNew = Ps0*(Tm/Tm0)
    dPsDt = dTmDt*Ps0/Tm0

    return (PsNew, dPsDt)

def calcMolConc2Chi(MolCon,Tm,Ps,Ru):

    Chi = (MolCon*Ru*Tm)/Ps
    return Chi

def output(ch, st, nt, tp):

    print(f" Chemical state @ nt: {nt}, tp: {tp[nt]:8.6f}:")
    print(f" Temperature: {st[nt, iTm]:8.2f} K, Pressure: {st[nt, iPs]:e} Pa, dP/dt: {st[nt, idP]:e} Pa/s")
    print(f" Molar concs [kmol/m^3]: Fu: {st[nt, iFu]:6.4f}, Ox: {st[nt, iOx]:6.4f}, Pr: {st[nt, iPr]:6.4f}")
    print(f" Mole fractions [-]:     Fu: {ch[nt, iFu]:6.4f}, Ox: {ch[nt, iOx]:6.4f}, Pr: {ch[nt, iPr]:6.4f}\n")

def plotTempEvolCh(ch, tp, pltRange, par_global):
    l, u = pltRange
    iFu, iOx, iPr, _, _, _, _, _ = par_global

    fig, ax = plt.subplots()

    ax.plot(tp[l:u]*1000, ch[l:u, iFu], label="Fuel")
    ax.plot(tp[l:u]*1000, ch[l:u, iOx], label="Oxidiser")
    ax.plot(tp[l:u]*1000, ch[l:u, iPr], label="Product")

    ax.set_ylabel("mole fraction")
    ax.set_xlabel("time in ms")

    fig.tight_layout()

    ax.legend()
    ax.grid()
    plt.show()

def plotTempEvolSt(st, tp, pltRange, par_global):
    l, u = pltRange
    iFu, iOx, iPr, iTm, _, idP, _, _ = par_global
    scfac = [1E+00, 1E-01, 1E-01, 1E-06, 0, 5E-15]

    fig, ax = plt.subplots()

    ax.plot(tp[l:u]*1000, st[l:u, iFu]*scfac[iFu], label=f"Fuel * {scfac[iFu]}")
    ax.plot(tp[l:u]*1000, st[l:u, iOx]*scfac[iOx], label=f"Oxidiser * {scfac[iOx]}")
    ax.plot(tp[l:u]*1000, st[l:u, iPr]*scfac[iPr], label=f"Product * {scfac[iPr]}")
    ax.plot(tp[l:u]*1000, st[l:u, iTm]*scfac[iTm], label=f"Temperature * {scfac[iTm]}")
    ax.plot(tp[l:u]*1000, st[l:u, idP]*scfac[idP], label=f"dP/dt * {scfac[idP]}")

    ax.set_ylabel("mol. conc. in kmol/m**3, temp. in K, dP/dt in Pa/s")
    ax.set_xlabel("time in ms")

    fig.tight_layout()

    ax.legend()
    ax.grid()
    plt.show()

def plotTempEvolStlog(st, tp, pltRange, par_global):
    l, u = pltRange
    iFu, iOx, iPr, iTm, _, idP, _, _ = par_global
    scfac = [1E+00, 1E-00, 1E-00, 1E-06, 0, 1E-13]

    fig, ax = plt.subplots()

    ax.plot(tp[l:u]*1000, st[l:u, iFu]*scfac[iFu], label=f"Fuel * {scfac[iFu]}")
    ax.plot(tp[l:u]*1000, st[l:u, iOx]*scfac[iOx], label=f"Oxidiser * {scfac[iOx]}")
    ax.plot(tp[l:u]*1000, st[l:u, iPr]*scfac[iPr], label=f"Product * {scfac[iPr]}")
    ax.plot(tp[l:u]*1000, st[l:u, iTm]*scfac[iTm], label=f"Temperature * {scfac[iTm]}")
    ax.plot(tp[l:u]*1000, st[l:u, idP]*scfac[idP], label=f"dP/dt * {scfac[idP]}")

    ax.set_ylabel("mol. conc. in kmol/m**3, temp. in K, dP/dt in Pa/s")
    ax.set_xlabel("time in ms")

    ax.set_yscale("log")

    fig.tight_layout()

    ax.legend()
    ax.grid()
    plt.show()

###########################################################################

###########################################################################
###                                MAIN                                 ###
if __name__ == '__main__':
    # Input parameters:
    # Global indices
    iFu = 0             # Fuel
    iOx = 1             # Oxidiser
    iPr = 2             # Product
    iTm = 3             # Temperature
    iPs = 4             # static pressure
    idP = 5             # pressure change
    nVar = 6            # number of variables
    nSpc = 3            # number of species

    # Save all parameters in list
    par_global = [iFu, iOx, iPr, iTm, iPs, idP, nVar, nSpc]

    # Isentropic compression
    T_BDC = 300.0       # Temperature at BDC [K]
    P_BDC = 101325.0    # Pressure at BDC [Pa]
    gamma = 1.4         # isentropic coefficient [-]
    r_compr = 10.0      # compression ratio [-]

    # Save all parameters in list
    par_isentropic = [T_BDC, P_BDC, gamma, r_compr]

    # Thermodynamic data
    Ru = 8314.4626      # Universal gas constant [J/kmol-K]
    MW = 29.0           # Molecular weight [kg/kmol] 
    cp = 1200.0*MW      # Heat capacity [J/kmol-K]
    hf0_Fu = 4.0e7*MW   # Enthalpy of formation of the fuel [J/kmol]
    A2F_ST = 16.0       # Stoichiometric air-fuel ratio [-]
    Phi = 1             # Equivalence ration [-]
    fOx = 16.0          # factor to evaluate oxidiser reaction rate 
    fPr = -17.0         # factor to evaluate product reaction rate

    # Save all parameters in list
    par_thermo = [Ru, MW, cp, hf0_Fu, A2F_ST, Phi, fOx, fPr]

    # Time control
    dt = 1e-7               # Time step width [s]
    ntmax = 40000           # maximum number of time steps
    ntdsp = 1000            # only print every ntdsp-th time step
    pltRange = (0, ntmax)   # Plotting range (min, max)

    # Initial conditions
    ###########################################################################
    # TODO 1: Implement the calculation of the initial state                  #
    # Use the functions calcInitialChi, isentropicCompression and             #
    # calcChi2MolConc. Store the results in the numpy arrays ch0 and st0      #
    ###########################################################################
    ch0 = #<YOUR CODE HERE>
    st0 = np.zeros(nVar)
    st0[iTm:iPs+1] = #<YOUR CODE HERE>
    st0[iFu:iPr+1] = #<YOUR CODE HERE>

    # Initialise Arrays
    st = np.zeros((ntmax+1,nVar))
    ch = np.zeros((ntmax+1,nSpc))
    tp = np.zeros(ntmax+1)

    st[0, :] = st0
    ch[0, :] = ch0

    for nt in range(1, ntmax+1):
        tp[nt] = nt*dt

        # Calculate time derivatives
        dFuDt = calcDFuDt(st[nt-1,iTm],st[nt-1,iFu],st[nt-1,iOx])
        dTmDt = calcDTmDt(st[nt-1,iTm],dFuDt,hf0_Fu,cp,Ru,st[nt-1,iPs])

        # Numerical time integration
        ###########################################################################
        # TODO 3: Solve the ODEs below using the explicit Euler Scheme.           #
        # Update the pressure and pressure gradient as well.                      #
        ###########################################################################
        st[nt, iFu] = st[nt-1, iFu] + #<YOUR CODE HERE>
        st[nt, iOx] = st[nt-1, iOx] + #<YOUR CODE HERE>
        st[nt, iPr] = st[nt-1, iPr] + #<YOUR CODE HERE>
        st[nt, iTm] = st[nt-1, iTm] + #<YOUR CODE HERE>

        # Update pressure
        st[nt,iPs],st[nt,idP] = #<YOUR CODE HERE>

        ch[nt] = calcMolConc2Chi(st[nt,iFu:iPr+1], st[nt,iTm], st[nt,iPs], Ru)

        if nt%ntdsp == 0: output(ch, st, nt, tp)
    
    print("<", 40*"=", ">")
    output(ch, st, nt, tp)

    dPmax = np.max(st[:,idP])
    ntdPmax = np.argmax(st[:,idP])

    print(f"Maximum pressure gradient occurs at t = {tp[ntdPmax]*1000:2.5f} ms")
    
    plotTempEvolCh(ch, tp, pltRange, par_global)
    plotTempEvolSt(st, tp, pltRange, par_global)
    plotTempEvolStlog(st, tp, pltRange, par_global)





