import numpy as np
from scipy import special
import matplotlib.pyplot as plt

'''
Single Step Scenario: Climate change
'''

thermal_conductivity = 2.2 #W/(m*K)
rho_cp = 2.2*10**6
heat_flow_density = 0.09 #W/m^2

surface_T0 = 10 #°C

delta_T1 = 2 #K
time1_yr = 40 #years

#calculate:
kappa = 
time_s =  #s
#denominator: 2*sqrt(kappa* t)
denominator = 


z = np.arange(0, 200)
#Temperature from fourier law:
fourier_T = 
# temperature change from step
step_T = 


plt.figure()
plt.title('change due to Temperature step')
plt.ylabel('z [m]')
plt.xlabel('$\Delta$T [°C]')
plt.grid()
plt.plot( step_T, -z)

#replace x,y in the plot:
plt.figure()
plt.title('underground Temperature')
plt.ylabel('z [m]')
plt.xlabel('T [°C]')
plt.grid()
plt.plot()
plt.plot( fourier_T, -z, label = 'steady')
plt.plot( x,y , label = 'transient')
plt.legend()


plt.show()
