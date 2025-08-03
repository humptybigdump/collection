import numpy as np
import matplotlib.pyplot as plt
from scipy import signal
from scipy.signal import find_peaks
from scipy import fftpack
from scipy.optimize import curve_fit
import math
import subprocess
from datetime import datetime
from scipy.integrate import odeint
import os

from matplotlib import font_manager
font_manager.findSystemFonts(fontpaths=None, fontext="ttf")
font_manager.findfont("latinmodernmath") # Test with "Special Elite" too


cm = 1/2.54
from pylab import rcParams
rcParams['figure.figsize'] = 5*cm, 5*cm
plt.rcParams['font.size'] = 8
rcParams['font.family'] = 'sans-serif'
rcParams['font.family'] = ['arial']


# Ausgangsparameter

F_0=5000 #N
f_M=6.8 # Hz 


#############
### Decke ###
#############
m1=1810#m11
k1=3300000#k11
omega1=(k1/m1)**.5
f1=1/2/math.pi*(k1/m1)**.5       # Hz
D=0#.015
c1=D*2*m1*omega1

#############
### Tilger ##
#############
nu=1/20
# print(nu)
m2=0.00000000000011
m2=m1*nu
k2=(2*math.pi*f1)**2*m2
zeta_T=(3*nu/(8*(1+nu)**3))**.5
# c2=2*m2*(2*math.pi*f1)*zeta_T
c2=0

f2=1/2/math.pi*(k2/m2)**.5       # Hz

####################################################
############# Initialization #############
####################################################

x1=1.00 # verschiebung Brücke
y1=0.00   # geschwindigkeit Brücke
x2=0.00  #Tilger
y2=0.00  #Tilger
x_init = [x1,y1,x2,y2] # Startbedingungen u1/v1 - u2/v2
   
####################################################
fs=1024
leng_t=20
t = np.arange(0,leng_t,1/fs)

####################################################
# Function that returns dx/dt
def vectorfield(w, t, p):
    if t<=leng_t/4:
        F1=F1=np.cos(2*math.pi*f_M*t)*F_0*np.sin(2*math.pi*(1/leng_t)*t)
    else:
        F1=np.cos(2*math.pi*f_M*t)*F_0
    F1=0#np.cos(2*math.pi*f_M*t)*F_0
    """
    Defines the differential equations for the coupled spring-mass system.

    Arguments:
        w :  vector of the state variables:
                  w = [x1,y1,x2,y2]
        t :  time
        p :  vector of the parameters:
                  p = [m1,m2,k1,k2,L1,L2,b1,b2]
    """
    x1, y1, x2, y2 = w
    m1, m2, k1, k2, c1, c2 = p

    # Create f = (x1',y1',x2',y2'):
    f = [y1,
         (-c1 * y1 - k1 * x1 + k2 * (x2 - x1) + c2 * (y2 - y1) + F1) / m1,
         y2,
         (-c2 * (y2-y1) - k2 * (x2 - x1)) / m2]
    return f
# Solve ODE

p = [m1, m2, k1, k2, c1, c2]
w0 = [x1, y1, x2, y2]

abserr = 1.0e-10
relerr = 1.0e-10
wsol = odeint(vectorfield, w0, t, args=(p,),
  atol=abserr, rtol=relerr)
####################################################
u_1=wsol[0:,[0,0]]
u_1=u_1[0:,0]

u_2=wsol[0:,[2,2]]
u_2=u_2[0:,0]
####################################################

nu=f_M/f1
V1=1/((1-nu**2)**2+4*D**2*nu**2)**.5
x_stat=F_0/k1
u_max=x_stat*V1
v_max=u_max*2*math.pi*f_M
a_max=v_max*2*math.pi*f_M
a_max_g=v_max*2*math.pi*f_M/9.81

a_lim=.045*9.81

u_lim=a_lim/(2*math.pi*f_M)/(2*math.pi*f_M)

####################################################

# fig, (ax1) = plt.subplots(1,1)      
# ax1.plot(t,u_2, color='tab:red',label='Tilger', linewidth=.25)
# ax1.plot(t,u_1, color='tab:blue',label='Brücke', linewidth=.25)

# max_part_u1=max(u_1[int(len(u_1)-fs*f1*3):len(u_1)-1])
# max_part_u2=max(u_2[int(len(u_2)-fs*f1*3):len(u_2)-1])
# print('max u1 - Brücke: ',round(max_part_u1,4),' m')
# print('max u2 - Tilger: ',round(max_part_u2,4),' m')

# ax1.set_xlim(0,20)
# # ax1.set_ylim(-max(u_1)*1.2,max(u_1)*1.2)

#######################################################
# Modal Analyse
######################################################

M=[[m1,0],[0,m2]]
K=[[k1+k2,-k2],[-k2,k2]]

p=(-m2*k1-m2*k2-k2*m1)/(m1*m2)
q=(k1*k2)/(m1*m2)

f1_rel=(-p/2+((-p/2)**2-q)**.5)**.5/2/math.pi
f2_rel=(-p/2-((-p/2)**2-q)**.5)**.5/2/math.pi

#######################################################
# FFT
######################################################

scale=1
u_FFT=u_1

#1 - fftpack.fft: returns complex ndarray
A_FFT = fftpack.fft(u_FFT)

#1 - fftpack.fftfreq: Array of length n containing the sample frequencies.
f_FFT = fftpack.fftfreq(len(u_FFT), d=1/fs) 

# #2 - Amplitude of FFT: sqrt(Re² + Im²) / n
A_FFT = np.abs(A_FFT)/len(u_FFT)

# # #3 - nur Auswahl des halben Spektrums -> Multiplikation mit "2"
A_FFT = A_FFT[0:int(len(u_FFT)/2)]*2
f_FFT = f_FFT[0:int(len(u_FFT)/2)] 

indices_1 = np.where(A_FFT == max(A_FFT))

#######################################################
# Plot
######################################################
fig, (ax1,ax2) = plt.subplots(2,1)
ax1.plot(t,u_1)
# ax1.stem(t,y)
ax2.plot(f_FFT, A_FFT)
# ax2.stem(f_FFT, A_FFT)
ax2.set_xlim(0,10)
ax1.set_xlim(0,5)
# ax2.set_ylim(0,.05)
ax2.axvline(f_M, linewidth=.5, color='black')
ax2.axvline(np.real(f1_rel), linewidth=.5, color='black')
ax2.axvline(np.real(f2_rel), linewidth=.5, color='black')
print('f1 - Brücke: ',round(np.real(f1_rel),3),' Hz')
print('f2 - Tilger: ',round(np.real(f2_rel),3),' Hz')
# ax2.set_ylim(0,1.2)