
import numpy as np
import matplotlib.pyplot as plt
from newton_1D import newton_1D

################################################################################
# Ads.2 - Langmuir Isotherm
# User Input
Fdot = 5000  # kg/h
Sdot = 100  # kg/h

W_F = 0.08  # kg/kg_waste water
W_S = 0.0  # kg/kg_adsorption medium

w_fluid_n = 0.1 * 1e-6  # kg/kg after final stage

# Langmuir Parameter
c_T = 10000
X_mon_T = 0.02  # Concentration of Monolayer & and limiting loading

# Simulation settings
N_max = 1000
Plotting_stages = True

# Calculating initially necessary values
# Transforming the flow rates to inert flows
F_inert = Fdot / (1.0 + W_F)
S_inert = Sdot / (1.0 + W_S)

# Transforming the molar fraction to molar loadings
W_fluid_n = w_fluid_n / (1.0 - w_fluid_n)

# Starting Stage determination
# Initializing vectors necessary for plotting the stages of the process
W_flOut_vec = np.zeros(N_max)
W_flIn_vec = np.zeros(N_max)
W_AMOut_vec = np.zeros(N_max)

W_Fluid = np.linspace(0, 0.1, 20000)
W_AMOut = c_T * W_Fluid / (1.0 + c_T * W_Fluid) * X_mon_T

N_stages = 1
W_flOut_prev = W_F
W_flOut_stage = W_F

# Defining function for solver
def Langmuir(W_W):
    return c_T * W_W / (1.0 + c_T * W_W) * X_mon_T

def Langmuir_rev(W_A):
    return W_A / (X_mon_T * c_T - W_A * c_T)

# Starting while loop to determine necessary number of steps
while (W_flOut_stage > W_fluid_n) and (N_stages < N_max):
    # Defining the mass balance for the new stage
    def m_balance(W):
        return F_inert * W_flOut_prev + S_inert * W_S - F_inert * W - S_inert * Langmuir(W)
    
    # def m_balance(W):
    #     return 3 - W
    
    # Solving the mass balance for a new retentate mass fraction
    W_flOut_stage = newton_1D(m_balance, W_flOut_prev)
    W_AMOut_stage = Langmuir(W_flOut_stage)
    
    W_flOut_vec[N_stages] = W_flOut_stage
    W_flIn_vec[N_stages] = W_flOut_prev
    W_AMOut_vec[N_stages] = W_AMOut_stage
    
    N_stages += 1
    W_flOut_prev = W_flOut_stage
    print (N_stages)

# Plot of Construction
fsize = 20  # fontsize

# Plotting the isotherm curve
plt.figure(1)
plt.plot(W_Fluid, W_AMOut, color='b')
plt.ylabel('$Beladung Adsorptionsmittel W_A$', fontsize=fsize)
plt.xlabel('$Beladung Abwasser W_W$', fontsize=fsize)
plt.ylim([0.0, 0.03])
plt.xlim([0.0, 0.001])
plt.title('Diagramm der Beladung', fontsize=fsize+3)

# Plotting the theoretical stages
if Plotting_stages:
    plt.figure(2)
    plt.plot(W_AMOut, W_Fluid, color='b')
    plt.ylabel('$Beladung Abwasser W_W$', fontsize=fsize)
    plt.xlabel('$Beladung Adsorptionsmittel W_A$', fontsize=fsize)
    plt.ylim([0.0, 0.1])
    plt.xlim([0.0, 0.03])
    plt.title('Diagram von Gegenstrom Prozess', fontsize=fsize+3)
    
    txt_Nth = f'Number of theo. stages: N_th= {N_stages}'
    plt.text(0.001, 0.09, txt_Nth, fontsize=14)
    
    for i in range(N_stages-1):
        # diagonal line
        plt.plot([W_S, W_AMOut_vec[i]], [W_flIn_vec[i], W_flOut_vec[i]], color='k')
        # horizontal line
        plt.plot([W_S, W_AMOut_vec[i]], [W_flOut_vec[i], W_flOut_vec[i]], color=[0.5, 0.5, 0.5], linestyle=':')

plt.show()