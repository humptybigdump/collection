
import numpy as np
import matplotlib
matplotlib.rcParams['text.usetex'] = True
#matplotlib.rcParams['figure.autolayout'] = True
matplotlib.rcParams['font.family'] ='sans-serif'
matplotlib.rcParams['font.sans-serif'] = 'Arial'
matplotlib.rcParams['font.size'] = 24
import matplotlib.pyplot as plt
from matplotlib.widgets import Slider

def harmonic_oscillator( t, omega, x0, phi0 ):
    return x0 * np.cos( omega * t + phi0 )


# starting values for angular frequencies
omega1 = 1
omega2 = 1
domega = 0.05

T1 = 2*np.pi/omega1
T2 = 2*np.pi/omega2

# starting values for amplitudes
A1 = 1
A2 = 1
dA = 0.05

# starting values for phases
phi1 = 0
phi2 = 0.25*np.pi
dphi = 0.1*np.pi

tmax   = 20
tsteps = 500
dt     = 5

t = np.linspace( 0, tmax, tsteps )
ho1 = harmonic_oscillator( t, omega1, A1, phi1 )
ho2 = harmonic_oscillator( t, omega2, A2, phi2 )
sum = ho1 + ho2

# display displacement as a function of time
fig,ax1 = plt.subplots( figsize=(14,10) )
plt.subplots_adjust( bottom=0.33 )

l1, = ax1.plot( t, ho1 )
l2, = ax1.plot( t, ho2 )
ls, = ax1.plot( t, sum, linewidth=3 )
                  
# nice labels
ax1.set_xlabel( r'Time $t$' )
ax1.set_ylabel( r'Displacement $x$' )
ax1.set_ylim( -1.1*(A1+A2), 1.1*(A1+A2) )
ax1.grid( True )

# function to update display if slider is operated
def update_sliders( val ):

	t_new = np.linspace( 0, st.val, tsteps )
	
	omega1_new = so1.val
	A1_new     = sA1.val
	phi1_new   = sp1.val
	 
	omega2_new = so2.val
	A2_new     = sA2.val
	phi2_new   = sp2.val
       	
	ho1_new = harmonic_oscillator( t_new, omega1_new, A1_new, phi1_new )
	ho2_new = harmonic_oscillator( t_new, omega2_new, A2_new, phi2_new )
	sum_new = ho1_new + ho2_new

	Amax = 1.05*(A1_new+A2_new)
	ax1.set_xlim( 0, st.val )
	ax1.set_ylim( -Amax, Amax )	
	
	l1.set_xdata( t_new )
	l1.set_ydata( ho1_new )
	l2.set_xdata( t_new )	
	l2.set_ydata( ho2_new )
	ls.set_xdata( t_new )
	ls.set_ydata( sum_new )
    
	fig.canvas.draw_idle()
    
# axes for sliders
axt  = plt.axes([0.20, 0.22, 0.60, 0.02])    

axo1 = plt.axes([0.20, 0.18, 0.60, 0.02])
axA1 = plt.axes([0.20, 0.15, 0.60, 0.02])
axp1 = plt.axes([0.20, 0.12, 0.60, 0.02])

axo2 = plt.axes([0.20, 0.08, 0.60, 0.02])
axA2 = plt.axes([0.20, 0.05, 0.60, 0.02])
axp2 = plt.axes([0.20, 0.02, 0.60, 0.02])

# slider objects
st  = Slider( axt, r'$t_\mathsf{max}$', 0, 100, valinit=tmax, valstep=dt )

so1 = Slider( axo1, r'$\omega_{1}$', 0.0, 2.0, valinit=omega1, valstep=domega )
sA1 = Slider( axA1, r'$A_{1}$', 0.0, 2.0, valinit=A1, valstep=dA )
sp1 = Slider( axp1, r'$\varphi_{1}$', -np.pi, np.pi, valinit=phi1, valstep=dphi )

so2 = Slider( axo2, r'$\omega_{2}$', 0.0, 2.0, valinit=omega2, valstep=domega )
sA2 = Slider( axA2, r'$A_{2}$', 0.0, 2.0, valinit=A2, valstep=dA )
sp2 = Slider( axp2, r'$\varphi_{2}$', -np.pi, np.pi, valinit=phi2, valstep=dphi )

# trigger for changes of any slider
st.on_changed( update_sliders )

so1.on_changed( update_sliders )
sA1.on_changed( update_sliders )
sp1.on_changed( update_sliders )

so2.on_changed( update_sliders )
sA2.on_changed( update_sliders )
sp2.on_changed( update_sliders )

# show plot again
plt.show()