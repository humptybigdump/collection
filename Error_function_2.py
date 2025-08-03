import numpy as np
from scipy import special
import matplotlib.pyplot as plt

'''
Dual Step Scenario: Climate change
'''

thermal_conductivity = 2.2 #W/(m*K)
rho_cp = 2.2*10**6
heat_flow_density = 0.09 #W/m^2

surface_T0 = 10 #°C
delta_T1 = 0.5 #K
time1_yr = 60 #years
delta_T2 = 1.5 #K
time2_yr = 20 #years

#calculate:
kappa = 
time1_s =
time2_s = 
#denominator: 2*sqrt(kappa* t)
denominator1 = 
denominator2 = 

z = np.arange(0, 200)
#Temperature from fourier law:
fourier_T = 
# temperature changes from steps
step1_T = 
step2_T = 


#replace x,y in the plots
plt.figure()
plt.title('change due to Temperature step')
plt.ylabel('z [m]')
plt.xlabel('$\Delta$T [°C]')
plt.grid()
plt.plot( x, y)


plt.figure()
plt.title('underground Temperature')
plt.ylabel('z [m]')
plt.xlabel('T [°C]')
plt.grid()
plt.plot()
plt.plot( fourier_T, -z, label = 'steady')
plt.plot( x, y, label = 'transient')
plt.legend()


plt.show()
