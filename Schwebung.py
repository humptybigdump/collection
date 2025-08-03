#!/usr/bin/env python
# coding: utf-8

# # Überlagerung von Schwingungen
# Auslöschung und Schwebung

import numpy as np
import matplotlib
matplotlib.rcParams['text.usetex'] = True
#matplotlib.rcParams['figure.autolayout'] = True
matplotlib.rcParams['font.family'] ='sans-serif'
matplotlib.rcParams['font.sans-serif'] = 'Arial'
matplotlib.rcParams['font.size'] = 24
import matplotlib.pyplot as plt
from matplotlib.widgets import Slider

def harmonischer_oszillator( t, omega, x0, phi0 ):
    return x0 * np.cos( omega * t + phi0 )


# Kreisfrequenz
omega1 = 1
omega2 = 1
domega = 0.05

T1 = 2*np.pi/omega1
T2 = 2*np.pi/omega2

# Amplitude
A1 = 1
A2 = 1
dA = 0.05

# Phase
phi1 = 0
phi2 = 0.25*np.pi
dphi = 0.1*np.pi

tmax   = 20
tsteps = 500
dt     = 5

t = np.linspace( 0, tmax, tsteps )
hoz1 = harmonischer_oszillator( t, omega1, A1, phi1 )
hoz2 = harmonischer_oszillator( t, omega2, A2, phi2 )
summe = hoz1 + hoz2

# Plot Amplitude gegen Zeit
fig,ax1 = plt.subplots( figsize=(14,10) )
plt.subplots_adjust( bottom=0.33 )

l1, = ax1.plot( t, hoz1 )
l2, = ax1.plot( t, hoz2 )
ls, = ax1.plot( t, summe, linewidth=3 )
                  
# Verschönerung der Darstellung
ax1.set_xlabel( r'$t$' )
ax1.set_ylabel( r'Auslenkung' )
ax1.set_ylim( -1.1*(A1+A2), 1.1*(A1+A2) )
ax1.grid( True )

# Funkion zum Update der Darstellung, wenn Schieberegler verstellt wurden
def update_sliders( val ):

	t_neu = np.linspace( 0, st.val, tsteps )
	
	omega1_neu = so1.val
	A1_neu     = sA1.val
	phi1_neu   = sp1.val
	 
	omega2_neu = so2.val
	A2_neu     = sA2.val
	phi2_neu   = sp2.val
       	
	hoz1_neu = harmonischer_oszillator( t_neu, omega1_neu, A1_neu, phi1_neu )
	hoz2_neu = harmonischer_oszillator( t_neu, omega2_neu, A2_neu, phi2_neu )
	summe_neu = hoz1_neu + hoz2_neu

	Amax = 1.05*(A1_neu+A2_neu)
	ax1.set_xlim( 0, st.val )
	ax1.set_ylim( -Amax, Amax )	
	
	l1.set_xdata( t_neu )
	l1.set_ydata( hoz1_neu )
	l2.set_xdata( t_neu )	
	l2.set_ydata( hoz2_neu )
	ls.set_xdata( t_neu )
	ls.set_ydata( summe_neu )
    
	fig.canvas.draw_idle()
    
# Achsen für Schieberegler
axt  = plt.axes([0.20, 0.22, 0.60, 0.02])    

axo1 = plt.axes([0.20, 0.18, 0.60, 0.02])
axA1 = plt.axes([0.20, 0.15, 0.60, 0.02])
axp1 = plt.axes([0.20, 0.12, 0.60, 0.02])

axo2 = plt.axes([0.20, 0.08, 0.60, 0.02])
axA2 = plt.axes([0.20, 0.05, 0.60, 0.02])
axp2 = plt.axes([0.20, 0.02, 0.60, 0.02])

# Schieberegler
st  = Slider( axt, r'$t_\mathsf{max}$', 0, 100, valinit=tmax, valstep=dt )

so1 = Slider( axo1, r'$\omega_{1}$', 0.0, 2.0, valinit=omega1, valstep=domega )
sA1 = Slider( axA1, r'$A_{1}$', 0.0, 2.0, valinit=A1, valstep=dA )
sp1 = Slider( axp1, r'$\varphi_{1}$', -np.pi, np.pi, valinit=phi1, valstep=dphi )

so2 = Slider( axo2, r'$\omega_{2}$', 0.0, 2.0, valinit=omega2, valstep=domega )
sA2 = Slider( axA2, r'$A_{2}$', 0.0, 2.0, valinit=A2, valstep=dA )
sp2 = Slider( axp2, r'$\varphi_{2}$', -np.pi, np.pi, valinit=phi2, valstep=dphi )

# Trigger für Änderung der Schieberegler
st.on_changed( update_sliders )

so1.on_changed( update_sliders )
sA1.on_changed( update_sliders )
sp1.on_changed( update_sliders )

so2.on_changed( update_sliders )
sA2.on_changed( update_sliders )
sp2.on_changed( update_sliders )

# Grafik abspeichern und zeigen
plt.show()



