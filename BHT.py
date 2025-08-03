import numpy as np
import matplotlib.pyplot as plt
from scipy.optimize import curve_fit

def linear_fct(x, a, b):
    return a * x + b

'''
Borhole temperature calculations
T(t) = T0 - k * ln(1+t'/Delta t)

k           constant ~ lambda
t'          cooling period
T0          undisturbed rock temperatuer
Delta t     recovery period


Plot T(t) over log(1+t'/delta t)

logging tol	    Depth of measurement	measurement tim	    max. temp T(t)
SP/ISF	        2495	                11:00	            76
BHC/CNL	        2495	                14:40	            79
CAL	            2495	                19:45	            84
CST	            2495	                23:10	            85
			
Drilling end	    02. Jul	    07:30	
End of circulation	03. Jul	    01:00	

'''
T_measured = np.array([76, 79, 84, 85])
#calculate:
t_dash =  #s
delta_t = np.zeros(4)
delta_t[0] = 
delta_t[1] = 
delta_t[2] = 
delta_t[3] = 

#calculate the term: ln(1+t'/Delta t)
log_term = 

#linear regression:
popt, _ = curve_fit(linear_fct, log_term, T_measured)

plt.figure()
plt.plot(log_term, T_measured, marker = 'o', ls= 'none', label = 'measured T')
plt.plot(log_term, linear_fct(log_term, *popt), 'r-', label='fit: a=%5.3f, b=%5.3f' % tuple(popt))
plt.legend()
plt.ylabel('T measured')
plt.xlabel("ln(1+t'/ Delta t)")

plt.tight_layout()
plt.show()