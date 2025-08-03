from scipy.optimize import fsolve
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.ticker import AutoMinorLocator


def CSTR_cascade(c_a_0,k,reaction_order,V_dot,V_r,N): 
    """
    Parameters
    ----------
    c_a_0 : inlet concentration [mol/m^3]
    k : reaction constant [1/(mol/m^3)(reaction_order-1)]
    reaction_order : reaction order
    V_dot : volume flow rate m^3/s
    V_r : overall reactor volume
    N : number of CSTRs with equal volume
    Returns
    -------
    c_a : array of concentrations for cascade plot
    V_sec: array with volume of each reactor for cascade plot 
    """
    #Calculate tau
    tau = (V_r/N)/V_dot
    #Define system of equations describing the CSTR cascade
    def func (c_a):
     eqSys = np.zeros(N) 
     for i in range(N):
         if i == 0:
             eqSys[0] = -c_a_0 + c_a[0] + tau * k* c_a[0]**reaction_order
         else:    
             eqSys[i] = -c_a[i-1] + c_a[i] + tau * k* c_a[i]**reaction_order
     return eqSys
    #Solve the system of equations
    c_a_i = fsolve(func,np.linspace(c_a_0, 1, N))
    #Calculate conversion at the end of the cascade
    X = (c_a_0 - c_a_i[-1])/c_a_0
    print("Final conversion:", X)
    #Sort c_a und V_sec (cummulated volume of the CSTRs) array for plotting 
    V_sec = []
    c_a = []
    c_a.append(c_a_0)
    for i in range (N):
       V_i = 0 + (i * V_r/(N) )
       V_sec.append(V_i)
       V_sec.append(V_i)   
       c_a.append(c_a_i[i])
       c_a.append(c_a_i[i])
    V_sec.append(V_r)   
    c_a = np.array(c_a)
    V_sec = np.array(V_sec)
    return(c_a,V_sec)  

#Plot data
#Font and lable settings      
colors = plt.cm.Dark2(np.linspace(0, 1, 8))   
fig_w = 5
fig_h = 5
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

#Constant parameter
c_a_0 = 20
k = 5
reaction_order = 2
V_r_single = 4

#Generate plot
fig, ax = plt.subplots(1,1,constrained_layout=True)
ax.set_xlabel(r'$\sum_{i=0}^{N}V_{i}$')
ax.set_ylabel(r'$c_{\mathregular{A}}$')
ax.yaxis.set_minor_locator(AutoMinorLocator(n=5))
ax.xaxis.set_minor_locator(AutoMinorLocator(n=5))
ax.set_xlim(-0.2,16)
ax.set_ylim(0,c_a_0)

#1 CSTR
V_dot = 1
N = 1
V_r = V_r_single
c_a, V_sec = CSTR_cascade(c_a_0,k,reaction_order,V_dot,V_r,N)
ax.plot(V_sec,c_a,label = r'$N =$' + str(N) + r', $\dot{V}=$' + str(V_dot) + r'$\mathregular{m^{3}s^{-1}}$' )  
ax.legend(frameon = True, fancybox = False,loc = 'upper right', ncol = 1,framealpha = 1 , edgecolor = 'black')  
#plt.show()

#2 CSTR
V_dot = 9.71
N = 2
V_r = 2*V_r_single
c_a, V_sec = CSTR_cascade(c_a_0,k,reaction_order, V_dot,V_r,N)
ax.plot(V_sec,c_a,label = r'$N =$' + str(N) + r', $\dot{V}=$' + str(V_dot) + r'$\mathregular{m^{3}s^{-1}}$')
ax.legend(frameon = True, fancybox = False,loc = 'upper right', ncol = 1,framealpha = 1 , edgecolor = 'black')  
#plt.show()

#3 CSTR
V_dot = 40
N = 4
V_r = 4*V_r_single
c_a, V_sec = CSTR_cascade(c_a_0,k,reaction_order, V_dot,V_r,N)
ax.plot(V_sec,c_a,label = r'$N =$' + str(N) + r', $\dot{V}=$' + str(V_dot) + r'$\mathregular{m^{3}s^{-1}}$' )
ax.legend(frameon = True, fancybox = False,loc = 'upper right', ncol = 1,framealpha = 1 , edgecolor = 'black')  
#plt.show()

#1000 CSTR (=PFR)
V_dot = 20.5
N = 1000
V_r = V_r_single
c_a, V_sec = CSTR_cascade(c_a_0,k,reaction_order, V_dot,V_r,N)
ax.plot(V_sec,c_a,label = r'$N =$' + str(N) + r', $\dot{V}=$' + str(V_dot) + r'$\mathregular{m^{3}s^{-1}}$') 
ax.legend(frameon = True, fancybox = False,loc = 'upper right', ncol = 1,framealpha = 1 , edgecolor = 'black')  
plt.show()

#Legend
ax.legend(frameon = True, fancybox = False,loc = 'upper right', ncol = 1,framealpha = 1 , edgecolor = 'black')    
