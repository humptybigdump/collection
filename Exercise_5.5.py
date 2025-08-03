import matplotlib.pyplot as plt
from matplotlib.ticker import AutoMinorLocator
import numpy as np

#Parameter
T_0 = 273.15 #K
tau = 3 #s
w_e = 0.01123
deltaH_r = -6.1*10**4 #kJ/kmol
deltaHs = 6010 #kJ/kmol
M_H2O = 18
c_a_0 = 1 #kmol/m^3
rho = 1000 #kg/m^3
c_p = 4.2 #kJ/kgK
T_min = 260  #K
T_max = 330 #K


#Calculate k and dkdTr
T_r = np.linspace(T_min, T_max, 100)
k = 4.92 *10**(30) * np.exp(-20000/T_r)
dkdTr = 4.92*10**(30) * 20000/T_r**2 * np.exp(-20000/T_r)
#Calculate Hp, dHpdTr, Hw and dHwdTr curve
Hp = -deltaH_r * c_a_0/rho * k*tau/(1+k*tau)
dHpdTr = -deltaH_r * c_a_0/rho * dkdTr*tau/(1+k*tau)**2
Hw = c_p * (T_r - T_0) + w_e * deltaHs/M_H2O
dHwdTr = c_p * np.ones(shape = len(T_r))
#Plot data
#Font and lable settings      
colors = plt.cm.Dark2(np.linspace(0, 1, 8))   
fig_w = 5.2
fig_h = 6
fig_pad = 0.1
plt.rcParams['figure.figsize']= (fig_w, fig_h)
plt.rcParams['axes.linewidth'] = 1 #set the value globally
plt.rc('xtick', labelsize=12)
plt.rc('ytick', labelsize=12)
plt.rc('axes', labelsize=12)
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
axs[0].plot(T_r,Hp, label = r'$Hp$')
axs[0].plot(T_r,Hw,label = r'$Hw$')
axs[1].plot(T_r,dHpdTr,label = r'$\dfrac{dHp}{dT_{\mathregular{r}}}$')
axs[1].plot(T_r,dHwdTr,label = r'$\dfrac{dHw}{dT_{\mathregular{r}}}$')
#Axis
axs[0].set_xlim(T_min,T_max)
axs[0].yaxis.set_minor_locator(AutoMinorLocator(n=5))
axs[0].xaxis.set_minor_locator(AutoMinorLocator(n=5))
axs[1].yaxis.set_minor_locator(AutoMinorLocator(n=5))
axs[1].set_xlabel(r'$T_{\mathregular{r}} \/\mathregular{/K}$')
axs[0].set_ylabel(r'$Hp, \, Hw \ /\mathregular{kJ \, kg^{-1}}$')
axs[1].set_ylabel(r'$\dfrac{d\left(Hp \, Hw \right)}{dT_{\mathregular{r}}} \ /\mathregular{kJ \, \left( kg \, K \right)^{-1}}$')
axs[0].legend(frameon = True, fancybox = False,loc = 'upper right', bbox_to_anchor=(0.1, 0.85, 0.2, 0.1),ncol = 1,framealpha = 1 , edgecolor = 'black') 
axs[1].legend(frameon = True, fancybox = False,loc = 'upper right', bbox_to_anchor=(0.8, 0.75, 0.2, 0.1),ncol = 1,framealpha = 1 , edgecolor = 'black')   