import numpy as np
from scipy import special
import matplotlib.pyplot as plt

'''
Dual Step Scenario: Ice age
'''

thermal_conductivity = 2.2 #W/(m*K)
rho_cp = 2.2*10**6
heat_flow_density = 0.09 #W/m^2

surface_T0 = 10 #°C

delta_T1 = -10 #K
time1_yr = 100000 #years
delta_T2 = 10 #K
time2_yr = 10000 #years

#calculate:
kappa =   #m^2/s

time1_s =   #s
time2_s =   #s
#denominator: 2*sqrt(kappa* t)
denominator1 = 
denominator2 = 

#depth
z = np.arange(0, 10000)
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
plt.plot( x,y , -z, label= 'total')
plt.plot( x,y , label = 'step 1')
plt.plot( x,y , label = 'step 2')

plt.figure()
plt.title('underground Temperature')
plt.ylabel('z [m]')
plt.xlabel('T [°C]')
plt.grid()
plt.plot()
plt.plot( x, y, label = 'steady')
plt.plot( x, y, label = 'transient')
plt.legend()


plt.show()
