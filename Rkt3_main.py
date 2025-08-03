
import numpy as np
import matplotlib.pyplot as plt
from newton_1D import newton_1D

################################################################################
# Rkt.3 - Multicomponent Rectification

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## User Input

Fdot      = 100 # kmol/h
split_E_3 = 0.9
split_B_4 = 0.85

x_F_i    = np.array([0.05, 0.15, 0.2, 0.2, 0.35, 0.05]) # [mol/mol]
n_comp   = x_F_i.size
alpha_i4 = np.array([1.80, 1.59, 1.59, 1.00, 0.89, 0.79])


#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- 
## Start of calculation
# The LKC is given as component 3 -> index 2
# The HKC is given as component 4 -> index 3

# Creating an array for the distillate composition and streams
x_E_i  = np.zeros(n_comp)
Edot_i = np.copy(x_E_i)

# Creating an array for the bottom product composition and streams
x_B_i = np.copy(x_E_i)
Bdot_i = np.copy(x_E_i)

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## Composition at the top and the bottom of the column according to Hengstebeck
# Composition of the feed, distillate and bottom product 

Fdot_i   = x_F_i * Fdot
Edot_i[2] = Fdot_i[2] * split_E_3
Bdot_i[3] = Fdot_i[3] * split_B_4

Bdot_i[2] = Fdot_i[2] - Edot_i[2]  # LKC
Edot_i[3] = Fdot_i[3] - Bdot_i[3]  # HKC

# Calculation of the two parameters A and C for the Hengstebeck equation
A_heng = np.log10(Edot_i[3]/Bdot_i[3])
C_heng = ( np.log10(Edot_i[2]/Bdot_i[2]) - A_heng ) / np.log10(alpha_i4[2])


# Calculation of the component streams and concentrations in distillate and bottom product
for j in range(n_comp):
    
    Bdot_i[j] = Fdot_i[j] / ( 10**( A_heng + C_heng * np.log10(alpha_i4[j]) ) + 1 )
    Edot_i[j] = Fdot_i[j] - Bdot_i[j]

Bdot = np.sum(Bdot_i)
Edot = np.sum(Edot_i)

for j in range(n_comp):
    
    x_B_i[j] = Bdot_i[j]/Bdot
    x_E_i[j] = Edot_i[j]/Edot


#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## Minimal number of stages according to Fenske

Nthmin_fensk = np.log( (x_E_i[2]/x_B_i[2]) * (x_B_i[3]/x_E_i[3]) ) / np.log( alpha_i4[2]/alpha_i4[3]) - 1 


#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## Minimal reflux ratio according to Underwood and the effective reflux ratio, if veff = 1.5*vmin
# Iterative calculation with Newton's algorithm

theta_init = np.mean([alpha_i4[2], alpha_i4[3]]) # alpha_HKC <= theta <= alpha_LKC


def f(theta):
    return np.sum(alpha_i4 * x_F_i / (alpha_i4 - theta) )

theta = newton_1D(f,theta_init)

vmin = sum( alpha_i4 * x_E_i / (alpha_i4 - theta) ) - 1
v    = vmin * 1.5

# ----------------------------------------------------------------------------------------------------------------------------------------------------------
# Number of theoretical stages using the diagram of Gilliland

# Gilliland correlation by Molokanov for the diagram
X_gilli_dia = np.linspace(0,1,1000)
Y_gilli_dia = 1 - np.exp(((1 + 54.4*X_gilli_dia)/(11 + 117.24*X_gilli_dia))*((X_gilli_dia - 1)/np.sqrt(X_gilli_dia)))

X_val = (v - vmin)/(v + 1)
Y_val = 1 - np.exp(((1 + 54.4*X_val)/(11 + 117.24*X_val))*((X_val - 1)/np.sqrt(X_val)))

Nth_gilli = (Nthmin_fensk + Y_val)/(1 - Y_val)

# ----------------------------------------------------------------------------------------------------------------------------------------------------------
# Optimal feed stage according to Kirkbride 

Nth_ratio = ( ( x_F_i[3]/x_F_i[2])*(x_B_i[2]/x_E_i[3] )**2 * ( Bdot / Edot ) )**0.206

Nth_AT = Nth_gilli / (1 + Nth_ratio)

Nth_F_optim = int(np.floor(Nth_AT))


# ----------------------------------------------------------------------------------------------------------------------------------------------------------
# Plot
plt.rcParams.update({'font.size': 13})
plt.plot(X_gilli_dia, Y_gilli_dia, color='blue', lw = 1)
plt.plot(X_gilli_dia, Y_gilli_dia, color='blue', lw = 1)
plt.plot([X_val, X_val],[0.01, Y_val], color='red', lw = 2)
plt.plot([0.01, X_val],[Y_val, Y_val], color='red', lw = 2)
plt.plot([0.1, 0.1],[0.01, 2], color='black', linestyle = 'dashed', lw = 1.5)
plt.plot([0.33, 0.33],[0.01, 2], color='black', linestyle = 'dashed', lw = 1.5)
plt.title('Diagram according to Gilliland') 
plt.xlabel(r'$\frac{v - v_{min}}{v + 1}$') 
plt.ylabel(r'$\frac{N_{th} - N_{th,min}}{N_{th} + 1}$')
plt.annotate('', xy=(0.1, 0.1), xytext=(0.33, 0.1), arrowprops = dict(arrowstyle = "<|-|>")) 
plt.annotate('EOR', xy=(0.16, 0.11)) 
plt.xscale('log')
plt.yscale('log')
plt.xlim([0.01, 1])
plt.ylim([0.01, 1])
plt.xticks([0.01,0.02,0.04,0.06,0.1,0.2,0.4,0.6,1.0],['0.01','0.02','0.04','0.06','0.1','0.2','0.4','0.6','1.0'])
plt.yticks([0.01,0.02,0.04,0.06,0.1,0.2,0.4,0.6,1.0],['0.01','0.02','0.04','0.06','0.1','0.2','0.4','0.6','1.0'])
plt.grid()
plt.show()