import numpy as np
import matplotlib.pyplot as plt
from scipy.optimize import curve_fit

def linear_function(x, a, b):
    return x*a + b

depth = [0, -40, -60, -80, -100, -120, -300] #m
temperature = [np.nan, 14.9, 15.9, 17.2, 18.2, 19.1, 26.0] #°C

'''
            Depth   lambda(z)
Quartaire   0-20    1,2
Dogger      20-66   1,4
Lias        66-98   1,9
Keuper      98-220  3,5
Muschelkalk 220-303 3,4
'''

#calculations:
sum_dz_lambda = np.zeros(len(depth)-1)








print(sum_dz_lambda)

#plot results:
fig = plt.figure()
plt.plot(sum_dz_lambda, temperature[1:], marker = 'x')
plt.xlabel('-$\sum \Delta z_i / \lambda _i$')
plt.ylabel('temperature')
plt.show()

#fit regression line
'''
popt, _ = curve_fit(linear_function, sum_dz_lambda, temperature[1:], )
print('regression data:' , popt)
fig = plt.figure()
plt.plot(sum_dz_lambda, temperature[1:], marker = 'x')
plt.plot(sum_dz_lambda, linear_function(sum_dz_lambda, *popt), 'r--', label='fit: a=%5.3f, b=%5.3f' % tuple(popt))
plt.xlabel('-$\sum \Delta z_i / \lambda _i$')
plt.ylabel('temperature')
plt.legend()
plt.show()
'''