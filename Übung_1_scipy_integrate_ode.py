import numpy as np
import matplotlib.pyplot as plt
import math
from scipy.integrate import odeint
from pylab import rcParams


################################################
# Hinweise:
# https://docs.scipy.org/doc/scipy/reference/generated/scipy.integrate.odeint.html

#################################################
def EMS(w, t, p):
    F1=np.cos(2*math.pi*freq*t)*load

    m1,  k1,  c1 = p        # Parameterset
    x1, y1, = w             # Verschiebungsgrößen

    ######################### System 1. Ordnung: f = (x1',y1'):
    # x   = x1
    # x'  = y1
    ##############################################################
    # x1' = y1
    # y1' = (-c1 * y1 - k1 * x1 +F1) / m1
    ##############################################################
    f = [y1,
          (-c1 * y1 - k1 * x1 +F1) / m1]

    return f

# def ZMS(w, t, p):
#     F1=np.cos(2*math.pi*freq*t)*load

#     x1, y1, x2, y2 = w
#     m1, m2, k1, k2, c1, c2 = p

#     f = [y1,
#          (-c1 * y1 - k1 * x1 + k2 * (x2 - x1) + c2 * (y2 - y1) + F1) / m1,
#          y2,
#          (-c2 * (y2-y1) - k2 * (x2 - x1)) / m2]
#     return f

#################################################
### Last/Zeit/Anfangsbedingunen ###
#################################################
freq=10 # Hz
load=0#1000  # N
fs=1000 # Achtung!
start=1
end=2
leng_t=10
t = np.arange(0,leng_t,1/fs)
x1, y1= 1,0                 # Startbedingungen der DGL

#################################################
### EMS ###
#################################################
m1=1811
k1=3306389
omega1=(k1/m1)**.5
f1=1/2/math.pi*(k1/m1)**.5       # Hz
D=0.015
c1=D*2*m1*omega1

##################################################################
# Lösung des Systems
##################################################################
p = [m1,  k1,  c1]           # Parameterset
w0 = [x1, y1]                # Verschiebungsgrößen

wsol = odeint(EMS, w0, t, args=(p,),) 
    # 1. Aufrufen der definition
    # 2. Einsetzen der Inputs
    #       1. w = w0
    #       2. t = t
    #       3. args: Zusätzliche Argumente zur Übergabe an die Funktion.
    #               Hier: das charakteristische Parameterset

##################################################################
# Verschiebung und Geschwindigkeit
##################################################################
x_1=wsol[0:,[0,0]]
x_1=x_1[0:,0]
y_1=wsol[0:,[1,0]]
y_1=y_1[0:,0]

# u_2=wsol[0:,[2,2]]
# u_2=u_2[0:,0]

##################################################################
# Darstellung
##################################################################
plt.plot(t, x_1)
# plt.plot(t, y_1)
plt.title('Simulation EMS')
plt.xlabel('t [s]')
plt.ylabel('Systemantwort (t)')
plt.legend(["x1: Verschiebung", "y1: Geschwindigkeit"])
plt.grid()
