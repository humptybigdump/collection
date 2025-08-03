
import numpy as np
import matplotlib.pyplot as plt
from newton_1D import newton_1D
from Ads3_fun import Ads3_fun

################################################################################
# Ads.3 - Freundlich Isotherm
# User Input
Fdot = 1000  # kg/h

w_F = 0.08  # kg/kg 
W_S = 0.0  # kg/kg_adsorption medium

w_R = 0.0001 * w_F  # kg/kg after final stage

# Freundlich Parameter
m = 2
n = 4  # Concentration of Monolayer & and limiting loading

# Calculating initially necessary values
# Transforming the flow rates to inert flows
F_inert = Fdot * (1.0 - w_F)

# Transforming the molar fraction to molar loadings
W_F = w_F / (1.0 - w_F)
W_R = w_R / (1.0 - w_R)

# Defining Isotherms for the solver
def Freundlich(X):
    return m * X**n

def Freundlich_rev(Y):
    return (Y / m)**(1/n)

# Starting Stage determination
S_inert = newton_1D(lambda S: Ads3_fun(F_inert, S, Freundlich_rev, W_F, W_S, W_R), W_F)

# Calculating the concentration of the stages for the plot
W_flIn_vec = np.zeros(3)
W_flOut_vec = np.zeros(3)
W_AMIn_vec = np.zeros(3)
W_AMOut_vec = np.zeros(3)

W_flOut_vec[0] = W_R
W_AMOut_vec[0] = Freundlich_rev(W_R)
W_flIn_vec[0] = (F_inert * W_R - S_inert * W_S + S_inert * W_AMOut_vec[0]) / F_inert
W_AMIn_vec[0] = W_S

W_flOut_vec[1] = W_flIn_vec[0]
W_AMOut_vec[1] = Freundlich_rev(W_flIn_vec[0])
W_flIn_vec[1] = (F_inert * W_flIn_vec[0] - S_inert * W_AMOut_vec[0] + S_inert * W_AMOut_vec[1]) / F_inert
W_AMIn_vec[1] = W_AMOut_vec[0]

W_flOut_vec[2] = W_flIn_vec[1]
W_AMOut_vec[2] = Freundlich_rev(W_flIn_vec[1])
W_flIn_vec[2] = (F_inert * W_flIn_vec[1] - S_inert * W_AMOut_vec[1] + S_inert * W_AMOut_vec[2]) / F_inert
W_AMIn_vec[2] = W_AMOut_vec[1]

# Plotting the Isotherm
fsize = 20  # fontsize

plt.figure(1)
W_x = np.linspace(0, 1, 200)
W_y = Freundlich(W_x)

plt.plot(W_x, W_y, color='b')
plt.xlabel('$Beladung Adsorptionsmittel W_A$', fontsize=fsize+4)
plt.ylabel('$Beladung Abwasser W_W$', fontsize=fsize+4)
plt.xlim([0.0, 1])
plt.ylim([0.0, 2])
plt.title('Diagram of Loading', fontsize=fsize+3)
plt.grid(True)

# Plotting the theoretical stages
Plotting_stages = True

if Plotting_stages:
    plt.figure(2)
    plt.plot(W_x, W_y, color='b')
    plt.ylabel('$Beladung in Gas W_W$', fontsize=fsize)
    plt.xlabel('$Beladung in Adsorptionsmittel W_A$', fontsize=fsize)
    plt.xlim([0.0, 0.45])
    plt.ylim([0.0, 0.1])
    plt.title('Diagram von Gegenstrom Prozess', fontsize=fsize+3)

    txt_Nth = f'Inerter Adsorptionsmittel Strom: S_inert= {S_inert}'
    plt.text(0.001, 0.09, txt_Nth, fontsize=14)

    plt.plot([W_S, W_AMOut_vec[2]], [W_R, W_F], color='r')

    for i in range(3):
        plt.plot([W_AMIn_vec[i], W_AMOut_vec[i]], [W_flOut_vec[i], W_flOut_vec[i]], color='k')
        plt.plot([W_AMOut_vec[i], W_AMOut_vec[i]], [W_flOut_vec[i], W_flIn_vec[i]], color='k')

plt.show()