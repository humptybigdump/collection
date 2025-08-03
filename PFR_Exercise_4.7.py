import numpy as np
from scipy.integrate import odeint
import matplotlib.pyplot as plt
from matplotlib.ticker import AutoMinorLocator

def GoverningEquations(c_i,tau,k_p,k_y,k_x):
    """
    Parameters
    ----------
    c_i : concentration array [kmol/m^3]
    tau :Residence time [min]
    k_p : reaction constant A->P [min]
    k_y : reaction constant A->Y [min]
    k_x : reaction constant P->X [min]
    
    Returns
    -------
    dc_id_tau: Array with ODEs (dc/dtau =)
    """
    c_a, c_p,c_y,c_x, = c_i
    dc_id_tau = np.zeros(4)
    #Define equation of species conservation for each species
    dc_id_tau[0] = -(k_y+k_p)*c_a #a
    dc_id_tau [1] = k_p*c_a - k_x*c_p #p   
    dc_id_tau[2] = k_x*c_p #x
    dc_id_tau[3] = k_y*c_a #y
    return(dc_id_tau)

#Parameters
c_i_0 = [1,0,0,0] #a,p,x,y kmol/m³
tau_end = 20
tau = np.linspace(0,tau_end,10000) #min
k_p = 0.2 #1/min 
k_x = 0.1 #1/min
k_y = 0.05 #1/min
#Solving ode system (Livermore Solver for Ordinary Differential Equations implemented in Scipy)
sol = odeint(GoverningEquations, c_i_0, tau, args=(k_p,k_y,k_x)) 
#Calculating conversion, selectivity and yield
X_a = (c_i_0[0]-sol[:,0])/c_i_0[0]
S_p =(sol[1:,1]-c_i_0[1])/(c_i_0[0]-sol[1:,0])
Y_p = sol[:,1]/c_i_0[0]
#Calculate tau_opt
tau_opt_analytical = 1/(k_x-k_y-k_p) * np.log(k_x/(k_y+k_p))
tau_opt_numerical = tau[np.where(Y_p == max(Y_p))][0]
print("Optimal residence time analytical[min]:", tau_opt_analytical)
print("Optimal residence time numerical [min]:", tau_opt_numerical)
#Plot data
#Font and lable settings      
colors = plt.cm.Dark2(np.linspace(0, 1, 8))   
fig_w = 5.2
fig_h = 6
fig_pad = 0.1
plt.rcParams['figure.figsize']= (fig_w, fig_h)
plt.rcParams['axes.linewidth'] = 1 #set the value globally
plt.rc('xtick', labelsize=14)
plt.rc('ytick', labelsize=12)
plt.rc('axes', labelsize=14)
plt.rc('legend', fontsize=14)
plt.rcParams['lines.markersize'] = 5
plt.rcParams['lines.linewidth'] = 1
plt.rcParams['xtick.direction']='out'
plt.rcParams['ytick.direction']='out'
plt.rcParams['xtick.major.size']=5
plt.rcParams['xtick.major.width']=1
plt.rcParams['xtick.minor.size']=3
plt.rcParams['xtick.minor.width']=1
plt.rcParams['ytick.major.size']=5
plt.rcParams['ytick.major.width']=1
plt.rcParams['ytick.minor.size']=3
plt.rcParams['ytick.minor.width']=1
plt.rcParams['legend.edgecolor']='k'
plt.rcParams['axes.unicode_minus']=False
plt.rcParams["legend.framealpha"] = 1
plt.rcParams['xtick.major.pad'] = 8
plt.rcParams['ytick.major.pad'] = 8
plt.rcParams['legend.handletextpad']=0.4
plt.rcParams['legend.columnspacing']=0.5
plt.rcParams['legend.labelspacing']=0.3
plt.rcParams['legend.title_fontsize'] = 14
#Generate plot
fig, axs = plt.subplots(2,1,constrained_layout=True, sharex=True)
#Concentrations
axs[0].set_xlim(0,tau_end)
axs[0].set_ylim(0,1)
axs[1].set_xlabel(r'$\tau \/\mathregular{min}$')
axs[0].set_ylabel(r'$c_{i} \ /\mathregular{kmol\,m^{-3}}$')
axs[0].yaxis.set_minor_locator(AutoMinorLocator(n=5))
axs[0].xaxis.set_minor_locator(AutoMinorLocator(n=5))
axs[0].plot(tau,sol[:,0],color = colors[0], label = r'$c_{\mathregular{a}}$')
axs[0].plot(tau,sol[:,1],color = colors[1],label = r'$c_{\mathregular{p}}$')
axs[0].plot(tau,sol[:,2],color = colors[2],label = r'$c_{\mathregular{x}}$')
axs[0].plot(tau,sol[:,3],color = colors[4],label = r'$c_{\mathregular{y}}$')
axs[0].plot([tau_opt_analytical,tau_opt_analytical],[0,1],color = colors[7], linestyle= 'dotted')
axs[0].legend(frameon = True, fancybox = False,loc = 'upper center',bbox_to_anchor=(0.55, 0.85, 0.2, 0.1), ncol = 2,framealpha = 1 , edgecolor = 'black')  
#Yield
axs[1].set_ylabel(r'$X_{\mathregular{a}}, \ S_{\mathregular{p}}, Y_{\mathregular{p}}$')
axs[1].set_ylim(0,1)
axs[1].yaxis.set_minor_locator(AutoMinorLocator(n=5))
axs[1].plot(tau,X_a,color = colors[0],label = r'$X_{\mathregular{a}}$')
axs[1].plot(tau[1:],S_p,color = colors[1],label = r'$S_{\mathregular{p}}$')
axs[1].plot(tau,Y_p,color = colors[3],label = r'$Y_{\mathregular{p}}$')
axs[1].plot([tau_opt_analytical,tau_opt_analytical],[0,1],color = colors[7], linestyle= 'dotted', label = "Optimum")
axs[1].legend(frameon = True, fancybox = False,loc = 'upper right', bbox_to_anchor=(0.8, 0.75, 0.2, 0.1),ncol = 2,framealpha = 1 , edgecolor = 'black')  