###########################################################################
###                            1D Transport                             ###
### D. Maerker (EBI-TFS)                                        2024-12 ###
########################################################################### 


###########################################################################
###                               IMPORTS                               ###
import numpy as np
import matplotlib.pyplot as plt
import cantera as ct
import os

###########################################################################


###########################################################################
###                              Classes                                ###

# Mixture class to handle Cantera
class Mixture():
    def __init__(self) -> None:
        self.solution = ct.Solution("GRI30.yaml")
    
    # Set state of solution object with temperature, pressure and molar fractions
    def setStateX(self, T, P, X)->None:
        self.solution.TPX = T, P, X
    
    # Set state of solution object with temperature, pressure and mass fractions
    def setStateY(self, T, P, Y)->None:
        self.solution.TPY = T, P, Y

    # Return density of gas with current parameters (T, P, Y) of solution object    
    def rho(self)->float:
        return self.solution.density
    
    # Return diffusion coefficient of gas in m**2/s with current parameters (T, P, Y) of solution object
    def D(self)->float:
        return self.solution.mix_diff_coeffs

    # Return mass fractions of solutions object    
    def Y(self)->np.ndarray:
        return self.solution.Y
    
    # Return molar fractions of solutions object 
    def X(self)->np.ndarray:
        return self.solution.X

    # Return current pressure of the solution object   
    def P(self)->float:
        return self.solution.P
    
    # Return current temperature of the solution object 
    def T(self)->float:
        return self.solution.T
    
    # Helper function to determine speciesId (e.g. used in Y) by speciesName
    def findSpeciesId(self, name):
        speciesNames = self.solution.species_names
        return speciesNames.index(name)

# Cell class to store per-cell information
class Cell(Mixture):
    def __init__(self, c, fW, fE, u0=0) -> None:
        super().__init__()
        self.center = c
        self.faces = [fW, fE]
        self.u = u0
        
    def __repr__(self) -> str:
        return f"Center: {self.center}; Faces: {self.faces}"
    
    def __lt__(self, other):
        return self.center < other.center
    
    def __gt__(self, other):
        return self.center > other.center
    
    # Set state of solution object with temperature, pressure and molar fractions
    def setStateX(self, T, P, X)->None:
        super().setStateX(T, P, X)

    # Set state of solution object with temperature, pressure and mass fractions    
    def setStateY(self, T, P, Y)->None:
        super().setStateY(T, P, Y)

    # Return density of gas with current parameters (T, P, Y) of solution object    
    def rho(self)->float:
        return super().rho()
    
    # Return diffusion coefficient of gas in m**2/s with current parameters (T, P, Y) of solution object
    def D(self)->float:
        return super().D()

    # Return mass fractions of solutions object   
    def Y(self)->np.ndarray:
        return super().Y()
    
    # Return molar fractions of solutions object 
    def X(self)->np.ndarray:
        return super().X()
    
    # Return current pressure of the solution object 
    def P(self)->float:
        return super().P()
    
    # Return current temperature of the solution object
    def T(self)->float:
        return super().T()

# Mesh class to combine all cells into one object
class Mesh(Cell):
    def __init__(self, uniform=True, dx=0.05, cells=250, centers_nonUniform=None) -> None:
        self.cells = []

        # Initialise mesh with uniform distancing
        if uniform: 
            centers = np.arange(dx, cells*dx+dx, dx)
            for i, c in enumerate(centers):
                self.cells.append(Cell(c, c-0.5*dx, c+0.5*dx)) # Create cell objects
        
        # Initialise mesh with pre-defined (non-uniform) cells centers
        else:
            for i, c in enumerate(centers_nonUniform):
                if i == 0:
                    fW = 0.5 * abs(c)
                    fE = c + 0.5 * abs(c-centers_nonUniform[i+1])
                elif i == len(centers_nonUniform)-1:
                    fW = c - 0.5 * abs(c-centers_nonUniform[i-1])
                    fE = c + 0.5 * abs(c-centers_nonUniform[i-1])
                else: 
                    fW = c - 0.5 * abs(c-centers_nonUniform[i-1])
                    fE = c + 0.5 * abs(centers_nonUniform[i+1]-c)
                self.cells.append(Cell(c, fW, fE)) # Create cell objects
        
        # Store cells in self.cells
        self.cells = np.array(self.cells)
    
    # Return an array of cell center points
    def centers(self)->np.array:
        return np.array([x.center for x in self.cells])
    
    # Return a 2D array of faces: row = cell, [0] = west, [1] = east
    def faces(self)->np.array:
        return np.array([x.faces for x in self.cells])

    # Return an array of internal cells (all apart from the boundary cells)
    def internalField(self)->np.ndarray:
        return self.cells[1:len(self.cells)-1]
    
    # Get the east neighbour cell of cell c
    def eastCell(self, c:Cell, offset=1)->Cell:
        id = np.where(self.cells == c)[0][0]
        target = id+offset
        if not target > len(self.cells)-1:
            return self.cells[target]   
        else:
            return self.cells[target-1] # Ghost Cell

    # Get the west neighbour cell of cell c
    def westCell(self, c:Cell, offset=1)->Cell:
        id = np.where(self.cells == c)[0][0]
        target = id-offset
        if not target < 0:
            return self.cells[target]   
        else:
            return self.cells[target+1] # Ghost Cell

    # Set initial square profile (top-hat)
    def initTopHatProfile(self, X0, X1, wJump, eJump, u0=1):
        stR = 3.5           # Stoichiometric Ratio

        for id, c in enumerate(self.cells):
            c.u = u0
            if id < wJump or id > eJump:
                c.setStateX(298, 101325, {"C2H6": X0, "O2": stR, "N2": 3.76*stR})
            else:
                c.setStateX(298, 101325, {"C2H6": X1, "O2": stR, "N2": 3.76*stR})
    
    # Update mass fractions Y with explicit Euler scheme
    def updateY(self, RHS, dt):
        for i, (c, rhs) in enumerate(zip(self.internalField(), RHS)):
            Y_new = np.zeros_like(c.Y())
            Y_new = c.Y() + dt * rhs/c.rho()

            c.setStateY(c.T(), c.P(), Y_new)
    
    # Update Boundary Conditions (with mode selection)
    def updateBC(self, mode="dirichlet"): 
        if mode.lower() == "dirichlet":
            stR = 3.5
            self.cells[0].setStateX(298, 101325, {"C2H6": 0, "O2": stR, "N2": 3.727*stR})
            self.cells[-1].setStateX(298, 101325, {"C2H6": 0, "O2": stR, "N2": 3.727*stR})
        elif mode.lower() == "zerogradient":
            self.cells[0].setStateX(298, 101325, self.cells[1].X())
            self.cells[-1].setStateX(298, 101325, self.cells[-2].X())
        elif mode.lower() == "periodic":
            self.cells[0].setStateX(298, 101325, self.cells[-2].X())
            self.cells[-1].setStateX(298, 101325, self.cells[1].X())


###########################################################################
###                             FUNCTIONS                               ###

def calcDiff(mesh:Mesh, nSpc):

    ###########################################################################
    # TODO 1: Implement the diffusion term.                                   #
    # Use the given array diff for the results.                               #
    # Use the given for loop for the iteration over the internal cells.       #
    #                                                                         #
    # Note: c is the currently calculated cell (as a Cell object)             #
    # Use the eastCell and westCell functions of the Mesh class to get the    #
    # neighbour cells. In order to access the required cell data (density,    #
    # diffusion coefficient, mass fractions) use the functions of the Cell    #
    # class.                                                                  #
    #                                                                         #
    # Calculate the different values of Gamma for each cell (W, C, E).        #
    # Use the mean value of two Gammas to calculate the value at an interface #
    # between two cells.                                                      #
    #                                                                         #
    # Calculate the diffusion term and store in in diff[i]                    #
    ###########################################################################


    diff = np.zeros((len(mesh.internalField()), nSpc))
    for i, c in enumerate(mesh.internalField()):
                
        # <YOUR CODE HERE>

        delta_x = abs(c.faces[0]-c.faces[1])
          
        diff[i] = # <YOUR CODE HERE>


    return diff

def UDS(u, val_W, val_E):
    ###########################################################################
    # TODO 3: Implement the UDS                                               #
    ###########################################################################
    
def CDS(val_W, val_E):
    return (val_E+val_W)/2

def TVD(mesh:Mesh, cell_c:Cell, cell_ref:Cell, u:float): # cell ref -> east or west of the current center cell

    ###########################################################################
    # TODO 4: Implement the UDS                                               #
    # Using the scheme below to get and idea of the cell naming for TVD       #
    #                     vel                        vel                      #
    #                     -->                        <--                      #
    #              |UU | U | D |                  | D | U |UU |               #
    #                      e                          e                       #
    #          |-x-|-x-|-x-|-x-|-x-|      |-x-|-x-|-x-|-x-|-x-|               #
    #          |WW | W | C | E |EE |      |WW | W | C | E |EE |               # 
    #          |i-2|i-1| i |i+1|i+2|      |i-2|i-1| i |i+1|i+2|               #
    #                                                                         #
    # Hint: You can use the < and > operators on cell objects to determine    #
    # their relative positions: West < East, East > West                      #
    #                                                                         #
    # Use the cells cell_c (centre cell) and cell_ref (east or west of cell_c)#
    # and use the four cases below to assign the cell names.                  #
    #                                                                         #
    # Use rhoPhi_D, rhoPhi_U and rhoPhi_UU to calculate B_r using Eq.(9)      #
    # Solve Eq.(8) and return its result.                                     #
    ###########################################################################

    if u > 0:                               
        if cell_c < cell_ref: 
            cell_D =  # <YOUR CODE HERE>
            cell_U =  # <YOUR CODE HERE>
            cell_UU = # <YOUR CODE HERE>
        else: 
            cell_D =  # <YOUR CODE HERE>
            cell_U =  # <YOUR CODE HERE>
            cell_UU = # <YOUR CODE HERE>
    else:
        if cell_c < cell_ref: 
            cell_D =  # <YOUR CODE HERE>
            cell_U =  # <YOUR CODE HERE>
            cell_UU = # <YOUR CODE HERE>
        else: 
            cell_D =  # <YOUR CODE HERE>
            cell_U =  # <YOUR CODE HERE>
            cell_UU = # <YOUR CODE HERE>

    rhoPhi_D = cell_D.rho()*cell_D.Y()
    rhoPhi_U = cell_U.rho()*cell_U.Y()
    rhoPhi_UU = cell_UU.rho()*cell_UU.Y()

    # Initialise B_r
    B_r = np.zeros_like(rhoPhi_U)

    # Calculate values of B_r using CHARM limiter
    for i, _ in enumerate(rhoPhi_U):
        if abs(rhoPhi_U[i]-rhoPhi_UU[i]) > 1e-10:
            r = # <YOUR CODE HERE>

            # <YOUR CODE HERE>
    
    return # <YOUR CODE HERE>

def calcConv(mesh:Mesh, nSpc, mode="CDS"):

    ###########################################################################
    # TODO 2: Implement the calcConv function                                 #
    # The general approach is the same as for TODO 1                          #
    # Compute the convection term using Eq.(4) and store the results in conv  #
    ###########################################################################

    # Init storage array with zeros
    conv = np.zeros((len(mesh.internalField()), nSpc))

    # Iteration over internal Field
    for i, c in enumerate(mesh.internalField()):
        rho_E = # <YOUR CODE HERE>
        rho_W = # <YOUR CODE HERE>
        rho_C = # <YOUR CODE HERE>
        Y_E =   # <YOUR CODE HERE>
        Y_W =   # <YOUR CODE HERE>
        Y_C =   # <YOUR CODE HERE>
        u_E =   # <YOUR CODE HERE>
        u_W =   # <YOUR CODE HERE>

        if mode == "UDS":
            rhoPhi_E = UDS(u_E, rho_C*Y_C, rho_E*Y_E)
            rhoPhi_W = UDS(u_W,  rho_W*Y_W, rho_C*Y_C)
        elif mode == "CDS":
            rhoPhi_E = CDS(rho_C*Y_C, rho_E*Y_E)
            rhoPhi_W = CDS(rho_W*Y_W, rho_C*Y_C)
        elif mode == "TVD":
            rhoPhi_E = TVD(mesh, c, mesh.eastCell(c), u_E)
            rhoPhi_W = TVD(mesh, c, mesh.westCell(c), u_W)
        else: raise TypeError(f"Mode {mode} is not available!")

        delta_x = abs(c.faces[0]-c.faces[1])

        conv[i] = # <YOUR CODE HERE>
    
    return conv

def plot(mesh, nt, stR=3.5):
    fig, ax = plt.subplots()

    X = np.array([c.X() for c in mesh.cells])

    mix = Mixture()
    id_c = mix.findSpeciesId("C2H6")
    id_o2 = mix.findSpeciesId("O2")

    ax.plot(mesh.centers(), X[:, id_c]/X[:, id_o2]*stR)

    ax.set_xlim((np.min(mesh.faces()), np.max(mesh.faces())))
    ax.set_ylim((-0.01, 1.01))
    ax.set_xlabel("Position $x$ in m")
    ax.set_ylabel("Equivalence Ratio $\phi_{A/F}$")
    ax.grid()
    ax.set_title(f"1D Transport $\phi_{{A/F}}$ @ {nt}th timestep")
    fig.tight_layout()
    plt.savefig(f"./Plots/cells_{len(mesh.cells)}_timeStep_{nt}.png")
    plt.show()

    

###########################################################################

###########################################################################
###                                MAIN                                 ###
if __name__ == '__main__':
    # Domain properties
    domainLength = 1e-3
    cells = 50
    # cells = 100
    dx = domainLength/cells
    
    # Create Mesh 
    mesh = Mesh(True, dx, cells)

    # Set initial concentration profile and cell velocities
    # Equivalence ratios phi
    outerPhi = 0
    innerPhi = 1
    mesh.initTopHatProfile(outerPhi, innerPhi, int(0.3*cells), int(0.7*cells), u0=1)

    # Set Iteration options
    dt = 2e-6
    # dt = 5e-8
    t = 0
    nt = 1
    ntMax = 500
    # ntMax = 2000
    tMax = 1e7
    ntPlot = 50
    # ntPlot = 250

    # Determine number of species from Cantera
    dummyMix = Mixture()
    nSpc = len(dummyMix.solution.species())

    # Iteration loop
    while t < tMax and nt <= ntMax:
        RHS = np.zeros((len(mesh.internalField()), nSpc))
        RHS = calcDiff(mesh, nSpc)                                  # Pure Diffusion
        # RHS =  - calcConv(mesh, nSpc, "CDS")                      # Pure Convection
        # RHS = calcDiff(mesh, nSpc) - calcConv(mesh, nSpc, "CDS")  # Diffusion and Convection with "UDS", "CDS", "TVD"
        mesh.updateY(RHS, dt)                                       # Update species mass fractions per cell
        mesh.updateBC("dirichlet")                                  # Update Boundary conditions with mode: "dirichlet", "zeroGradient" or "periodic"

        if nt % ntPlot == 0:                                        # Create Plot
            plot(mesh, nt)
        
        t += dt                                                     # Update iteration counters
        nt += 1
