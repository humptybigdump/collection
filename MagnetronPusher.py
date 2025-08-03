
# -----------------------------------------------------------------------------
# KIT
# HMT Lecture WS24/25 
#
# Prof. J. Jelonnek, A. Marek, L. Feuerstein
# -----------------------------------------------------------------------------

# import basic modules
import numpy as np

from matplotlib import pyplot as plt
from matplotlib import animation as anim

from scipy.interpolate import BSpline
from scipy.special import jnp_zeros, jv, jvp
from scipy.integrate import solve_ivp, cumulative_trapezoid
from scipy.constants import speed_of_light, electron_mass, elementary_charge



###############################################################################
# define functions

def pushParticle(dt, r0, v0, Efun, Bfun):
        """
        Push the particles using Boris' method: updates position and speed of
        the particle.
        Input:  
            dt: Timestep (float32)
            r0: position (Numpy((3), dtype='float32'))
            v0: velocity (Numpy((3), dtype='float32'))
            Efun: function that returns the electric field at the particle's position (Numpy((3), dtype='float32'))
            Efun: function that returns the magnetic field at the particle's position (Numpy((3), dtype='float32'))
        Return :
            r: new position (Numpy((3), dtype='float32'))
            v: new velocity (Numpy((3), dtype='float32'))
        """
        #Get the E and B field at the initial position
        E = Efun(r0)
        B = Bfun(r0)

        ## Adding half of the magnetic impulse
        v = v0 - elementary_charge * E * dt / (2 * electron_mass)

        ## Rotation of speed according to B
        t = -elementary_charge * dt * B / (2 * electron_mass)
        v1 = v + np.cross(v, t)
        v += np.cross(v1, 2 /(1 + np.abs(t)**2) * t)

        ## Adding second half of the magnetic impulse
        v -= elementary_charge * E * dt / (2 * electron_mass)

        # Updating position
        r = r0 + dt * v

        return r, v


def larmorRadius(B_z, E_kin=0, v_t=0):
    """
    Calculates the Larmor radius and gyration velocity of an electron in a static magnetic field
    Input:
        B_z: in T, static magnetic field (transversal to the electron velocity)
    Provide nly ONE of the following arguments:
        E_kin: in keV, kinetic energy of the electron (it is assumed that the electron has only a velocity component trnsversal to the field)
        v_t: in m/s, transversal velocity of the electron
    Return:
        r_l: in m, Larmor radius
        v_t: in m/s, gyration velocity
    """
    if E_kin<=0:
         lorentz_factor = 1.0 / np.sqrt(1.0 - v_t**2 / speed_of_light**2)
    else:
         lorentz_factor = 1 + E_kin / 511
         v_t = np.sqrt(1 - 1 / lorentz_factor**2) * speed_of_light # in m/s
    
    omega_cl = 2 * np.pi * 28 * B_z * 1e9 / lorentz_factor  # Cyclotron Angular Frequency
    r_l = v_t / omega_cl # in m, Larmour Radius
    return r_l, v_t


def plotVectorFieldXY(r, getField, plt):
    meshX, meshY = np.meshgrid(np.arange(-r, r, 2*r/10), np.arange(-r, r, 2*r/10))

    Ex = np.zeros((np.size(meshX, 0), np.size(meshX, 1)))
    Ey = np.zeros((np.size(meshX, 0), np.size(meshX, 1)))
    Ez = np.zeros((np.size(meshX, 0), np.size(meshX, 1)))

    for ix in range(np.size(meshX, 0)):
        for iy in range(np.size(meshX, 1)):
            E = getField(np.array([meshX[ix,iy], meshY[ix,iy], 0]))
            Ex[ix,iy] = E[0]
            Ey[ix,iy] = E[1]
            Ez[ix,iy] = E[2]

    plt.quiver(meshX, meshY, Ex, Ey, color='g')


###############################################################################
# Initial Parameter

B0 = 1 # in T, absolut B-field
def getBfield_static_Bz(r0):
     return np.array([0,0,B0])
getBfield = getBfield_static_Bz


def getEfield_static_Ex(r0):
     return np.array([E0,0,0])

def getEfield_static_Ey(r0):
     return np.array([0,E0,0])

def getEfield_static_Er(r0):
     phi = np.arctan2(r0[1],r0[0])
     return E0*np.array([np.cos(phi),np.sin(phi),0])


# Case 1:
# E0 = 0 # V/m, absolut E-field
# getEfield = getEfield_static_Ex
# r_l, v_t = larmorRadius(B0, E_kin=1e-10)
# r0 = np.array([-r_l,0,0])  # in m/s
# v0 = np.array([0,-v_t,0])  # in m/s

# Case 2: 
# E0 = -15 # V/m, absolut E-field 
# getEfield = getEfield_static_Ey
# r_l, v_t = larmorRadius(B0, E_kin=1e-10)
# r0 = np.array([-r_l,0,0])  # in m/s
# v0 = np.array([0,-v_t,0])  # in m/s

# Case 3: 
# E0 = -15 # V/m, absolut E-field
# getEfield = getEfield_static_Ey
# r_l, v_t = larmorRadius(B0, v_t=E0/B0)
# r0 = np.array([-3*r_l,0,0])  # in m/s
# v0 = np.array([0,0,0])  # in m/s

# Case 4: 
# E0 = -15 # V/m, absolut E-field
# getEfield = getEfield_static_Ey
# r_l, v_t = larmorRadius(B0, v_t=E0/B0)
# r0 = np.array([-3*r_l,0,0])  # in m/s
# v0 = np.array([v_t,0,0])  # in m/s

# Case 5:
# E0 = -15 # V/m, absolut E-field
# getEfield = getEfield_static_Ey
# r_l, v_t = larmorRadius(B0, v_t=E0/(2*B0))
# r0 = np.array([-4*r_l,0,0])  # in m/s
# v0 = np.array([E0/(2*B0),0,0])  # in m/s

# Case 6:
E0 = -30 # V/m, absolut E-field
E0 = -60 # V/m, absolut E-field
getEfield = getEfield_static_Er
r_l, v_t = larmorRadius(B0, E_kin=1e-10)
r0 = np.array([-r_l,0,0])  # in m/s
v0 = np.array([0,0,0])  # in m/s



###############################################################################
# Simulation

dt = 1/140e9/40 # in s, simulation time step
steps = 700 # number of time steps which should be simulated

x = np.zeros(steps)
y = np.zeros(steps)

for tStep in range(0, steps):
    r0, v0 = pushParticle(dt, r0, v0, getEfield, getBfield)
    x[tStep] = r0[0]
    y[tStep] = r0[1]



###############################################################################
# Plot the results

fig, ax = plt.subplots()
line = ax.plot(r0[0], r0[1], "o")
#line = ax.plot(x, y, "o")

def update(i):
    #line[0].set_xdata([x[i]])
    #line[0].set_ydata([y[i]])
    line[0].set_xdata(x[range(0, i)])
    line[0].set_ydata(y[range(0, i)])


ax.set_aspect(1)
ax.add_artist(plt.Circle((0.0, 0.0), r_l, fill=False, color="red", alpha=0.2))
if r_l:
    ax.set_xlim(-r_l - r_l * 2, r_l + r_l * 2)
    ax.set_ylim(-r_l - r_l * 2, r_l + r_l * 2)
else:
    ax.set_xlim(-1e-4,1e-4)
    ax.set_ylim(-1e-4,1e-4)
ax.set_xlabel("x in m")
ax.set_ylabel("y in m")

anim_obj = anim.FuncAnimation(
    fig, update, frames=steps, interval=10, repeat=False, blit=False
)

#plot the E-Field direction
plotVectorFieldXY(4*r_l, getEfield, plt)
ax.axis([-4*r_l, 4*r_l, -4*r_l, 4*r_l])

plt.show()

