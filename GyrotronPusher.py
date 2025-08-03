
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


##############################################################################
# define global variables
time = 0 #in ns, 


#numerical parameters
dt = 1/25e9/200 # in s, simulation time step
steps = 1500 # number of time steps which should be simulated

#Nparticles = 12 # number of particles which are simulated
Nparticles = 25 # number of particles which are simulated

###############################################################################
# define functions

def pushParticle(dt, nt, r0, v0, Efun, Bfun):
        """
        NON-Relativistic particle pusher using Boris' method: updates position and speed of
        the particle.
        Input:  
            dt: in s, step-widthfor one timestep (float32)
            nt: timestep
            r0: in m, position (Numpy((3), dtype='float32'))
            v0: in m/s, velocity (Numpy((3), dtype='float32'))
            Efun: function that returns the electric field at the particle's position (Numpy((3), dtype='float32'))
            Bfun: function that returns the magnetic field at the particle's position (Numpy((3), dtype='float32'))
        Return :
            r: new position (Numpy((3), dtype='float32'))
            v: new velocity (Numpy((3), dtype='float32'))
        """
        #Get the E and B field at the initial position
        E = Efun(dt*nt)
        B = Bfun(dt*nt)

        ## Adding half of the magnetic impulse
        v = v0 + elementary_charge * E * dt / (2 * electron_mass)

        ## Rotation of speed according to B
        t = elementary_charge * dt * B / (2 * electron_mass)
        v1 = v + np.cross(v, t)
        v += np.cross(v1, 2 /(1 + np.abs(t)**2) * t)

        ## Adding second half of the magnetic impulse
        v += elementary_charge * E * dt / (2 * electron_mass)

        # Updating position
        r = r0 + dt * v

        return r, v, 1.0


def pushParticleRel(dt, nt, r0, v0, Efun, Bfun):
        """
        Relativistic particle pusher using Boris' method: updates position and speed of
        the particle.
        Input:  
            dt: in s, step-widthfor one timestep (float32)
            nt: timestep
            r0: in m, position (Numpy((3), dtype='float32'))
            v0: in m/s, velocity (Numpy((3), dtype='float32'))
            Efun: function that returns the electric field at the particle's position (Numpy((3), dtype='float32'))
            Bfun: function that returns the magnetic field at the particle's position (Numpy((3), dtype='float32'))
        Return :
            r: new position (Numpy((3), dtype='float32'))
            v: new velocity (Numpy((3), dtype='float32'))
        """

        #calculate the Lorenz factor and the relativistic velocity of the particle
        vAbs = np.linalg.norm(v0)
        gamma = 1.0/np.sqrt(1.0 - (vAbs/speed_of_light)**2)
        u = v0*gamma #relativistic momentum

        #calc the E-field influences
        E = Efun(dt*nt)
        eps = (elementary_charge/(2.0*electron_mass) * dt) * E

        #first half step pushing in the E-field
        u_m = u + eps

        #calc rotation in the B field
        B = Bfun(dt*nt)
        Babs = np.linalg.norm(B)

        theta = (elementary_charge*dt) / (electron_mass*gamma) * Babs
        trans = (np.tan(theta/2.0) / Babs) * B
        u_bar = u_m + np.cross(u_m, trans)
        u_p = u_m + 2.0/(1.0+np.linalg.norm(trans)**2) * np.cross(u_bar, trans)

        #second half step pushing in the E-field
        u_new = u_p + eps
        gamma_new = np.sqrt(1.0 + np.linalg.norm(u_new)**2/(speed_of_light*speed_of_light))

        #calculate new position and acceleration
        r_new = r0 + (dt/gamma_new)*u_new
        v_new = u_new / gamma_new

        #return the new properties of the particle
        return r_new, v_new, gamma_new


def larmorRadius(B_z, E_kin, alpha):
    """
    Calculates the Larmor radius and gyration velocity of an electron in a static magnetic field
    Input:
        B_z: in T, static magnetic field (transversal to the electron velocity)
        E_kin: in keV, kinetic energy of the electron (it is assumed that the electron has only a velocity component trnsversal to the field)
        alpha: pitch factor 
    Return:
        r_l: in m, Larmor radius
        v_t: in m/s, gyration velocity
        omega_cl: in rad*Hz, cyclotron frequency
    """
    beta2 = 1.0 - 510.99895**2 / (510.99895+E_kin)**2
    gamma = 1.0/np.sqrt(1.0-beta2)
    v_z = speed_of_light * np.sqrt( beta2 / (alpha**2 + 1.0) )
    v_t = speed_of_light * np.sqrt( beta2 / (1.0/(1+alpha**2) + 1.0) )

    omega_cl = 2 * np.pi * 28 * B_z * 1e9 / gamma  # in rad*Hz, Cyclotron Angular Frequency
    r_l = v_t / omega_cl # in m, Larmour Radius
    return r_l, omega_cl, v_t, v_z


###############################################################################
# Initial Parameter

# define the B-field
B0 = 1 # in T, absolut B-field
def getBfield_static_Bz(t):
     return np.array([0,0,B0])
getBfield = getBfield_static_Bz


# define the particle position and velocities
eKin0 = 80.0 #in keV, initial kinetic energy
r_l, omega_cl, v_t, v_z0 = larmorRadius(B0, eKin0, 1.5)

x0 = r_l * np.cos(np.arange(0, 2 * np.pi, 2 * np.pi / Nparticles))
y0 = r_l * np.sin(np.arange(0, 2 * np.pi, 2 * np.pi / Nparticles))

v_x0 = v_t * np.sin(np.arange(0, 2 * np.pi, 2 * np.pi / Nparticles))
v_y0 = -v_t * np.cos(np.arange(0, 2 * np.pi, 2 * np.pi / Nparticles))


# define the E-field
#E0 = 0 # in V/m, absolut E-field
E0 = 5.0e6 # in V/m, absolut E-field 

omega = 1.10 * omega_cl # in rad/s, angular frequency of the RF E-field
#omega = 0.90 * omega_cl # in rad/s, angular frequency of the RF E-field

#omega = 1.077 * omega_cl # in rad/s, angular frequency of the RF E-field
#omega = 0.923 * omega_cl # in rad/s, angular frequency of the RF E-field

#omega = 1.0 * omega_cl # in rad/s, angular frequency of the RF E-field

def getEfield_rotatingField(t):
     phi = -omega * t
     #phi = omega * t
     return E0*np.array([np.cos(phi),np.sin(phi),0])

def getEfield_linearField(t):
     phi = omega * t
     return E0*np.array([0,np.cos(phi),0])


getEfield = getEfield_rotatingField
#getEfield = getEfield_linearField


###############################################################################
# Simulation

x = np.zeros((steps, Nparticles))
x[0,:] = x0
y = np.zeros((steps, Nparticles))
y[0,:] = y0
v_x = np.zeros((steps, Nparticles))
v_x[0,:] = v_x0
v_y = np.zeros((steps, Nparticles))
v_y[0,:] = v_y0
v_z = np.zeros((steps, Nparticles))
v_z[0,:] = v_z0
eKin = np.zeros(steps)
eKin[0] = eKin0


for tStep in range(1, steps):
    #push the particles to their new position 
    for pStep in range(0, Nparticles):
         r, v, g = pushParticleRel(dt, tStep, np.array([x[tStep-1,pStep],y[tStep-1,pStep],0]), 
                                   np.array([v_x[tStep-1,pStep],v_y[tStep-1,pStep],v_z[tStep-1,pStep]]), getEfield, getBfield)
         x[tStep, pStep] = r[0]
         y[tStep, pStep] = r[1]
         v_x[tStep, pStep] = v[0]
         v_y[tStep, pStep] = v[1]
         v_z[tStep, pStep] = v[2]

         eKin[tStep] += 0.5 * electron_mass * g * np.linalg.norm(v)**2 * 6.242e+15 # in keV
    eKin[tStep] /= Nparticles





###############################################################################
# Plot the results

def getEfieldXY(meshX, meshY, getField, t):
    E0 = getField(t)
    Ex = E0[0]*np.ones((np.size(meshX, 0), np.size(meshX, 1)))
    Ey = E0[1]*np.ones((np.size(meshX, 0), np.size(meshX, 1)))

    return Ex, Ey

fig, ax = plt.subplots()

#plot the initial particle positions
line = ax.plot(x[0, :], y[0, :], "o")

#plot the E-field at time=0
plotRange = 2.5*r_l
meshX, meshY = np.meshgrid(np.arange(-plotRange, plotRange, 2*plotRange/6), np.arange(-plotRange, plotRange, 2*plotRange/6))
Ex, Ey = getEfieldXY(meshX, meshY, getEfield, 0)
Qplot = plt.quiver(meshX, meshY, Ex, Ey, color='g')
ax.axis([-plotRange, plotRange, -plotRange, plotRange])


text_box = ax.text(0.05, 0.95, r'$av. E_{kin}=%.2f keV$' % (0, ), transform=ax.transAxes, fontsize=14, verticalalignment='top', bbox=dict(boxstyle='round', facecolor='wheat', alpha=0.5))

def update(i):
    #line[0].set_xdata(x[range(0, i)])
    #line[0].set_ydata(y[range(0, i)])
    line[0].set_xdata(x[i, :])
    line[0].set_ydata(y[i, :])
    Ex, Ey = getEfieldXY(meshX, meshY, getEfield, i*dt)
    Qplot.set_UVC(Ex,Ey)
    text_box.set_text(r'$av. E_{kin}=%.2f keV$' % (eKin[i], ))



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

plt.show()


#Plot the kinetic energy over the time
plt.figure()
plt.plot(dt*np.arange(0.0, steps)*1.0e9, eKin)
plt.xlabel("t in ns")
plt.ylabel("E_{kin} in keV")

plt.show()
