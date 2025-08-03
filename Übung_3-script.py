import numpy as np
import math
import matplotlib.pyplot as plt
from scipy import signal
from scipy.signal import find_peaks
from scipy import fftpack
from scipy.optimize import curve_fit

# Aufgabe:
# Funktion erstellen
# Funktion Plotten
# FFT Stückweise aufbauen
# Spielen mit Parameter
# Erstelen Padding für kurze Signale
# ifft -> neues schönes Signal
# Implikation in TMS von Übung 2


Start=0
End=1


f1=5
D1=0.0
A1=1

f_s_1=5*f1 # Abtastrate

# Funktion
omg_N1=2*math.pi*f1
omg_D1=omg_N1*(1-D1)**.5

dt=1/f_s_1
t_1 = np.arange(Start, End, dt)
# t_1=np.append(t_1,t_1[-1]+dt)

u_1 = A1*np.exp(-D1*t_1*omg_N1) * np.sin(omg_D1 * t_1+math.pi/2)

#######################################################
# Filter
######################################################

# sos2 = signal.butter(10, 10, 'hp', fs=int(f_s_1), output='sos')
# # u_1=signal.sosfilt(sos2, u_1)
# u_1=signal.sosfiltfilt(sos2, u_1, padlen=0)

#######################################################
# FFT
######################################################

scale=1
u_FFT=u_1
# Window

# window = np.hanning(len(u_FFT))
# u_FFT=u_FFT*window
# scale_h=len(window)/sum(window)
# u_FFT=u_FFT*window

# u_FFT=np.pad(u_FFT, (99, 99), 'constant')
# scale_p=len(u_FFT)/len(u_1)
scale_p=1

#1 - fftpack.fft: returns complex ndarray
A_FFT = fftpack.fft(u_FFT)

#1 - fftpack.fftfreq: Array of length n containing the sample frequencies.
f_FFT = fftpack.fftfreq(len(u_FFT), d=dt) 

# #2 - Amplitude of FFT: sqrt(Re² + Im²) / n
A_FFT = np.abs(A_FFT)/len(u_FFT)*scale_p

# # #3 - nur Auswahl des halben Spektrums -> Multiplikation mit "2"
A_FFT = A_FFT[0:int(len(u_FFT)/2)]*2
f_FFT = f_FFT[0:int(len(u_FFT)/2)]  

#######################################################
# Plot
######################################################
fig, (ax1,ax2) = plt.subplots(2,1)
# ax1.set_xlim(0,1)
ax1.plot(t_1, u_1)
ax1.stem(t_1, u_1)
# ax1.plot(t_2, u_2)

ax2.plot(f_FFT, A_FFT)
# ax2.stem(f_FFT, A_FFT)
# ax2.set_xlim(0, 10)
