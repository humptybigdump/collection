# Import the necessary libraries
import numpy as np
import matplotlib.pyplot as plt
import time as tm

# Enable to get TeX fonts in plots
#plt.rc('text', usetex=True)
#plt.rc('font', family='serif')

# Economic parameters
betta = 0.95
alpha = 0.3
delta = 0.1
gamma = 2.0
rho = 0.9
sigma = 0.23
# Grid parameters
k_min = 0.1
k_max = 6.0
nk = 200
nz = 2
# Technical parameters
maxiter = 10000
epsi = 1e-5

# Initialization
kgrid = np.linspace(k_min,k_max,nk)
v_guess = np.zeros((nk,nz))
val = np.zeros(nk)
v_new = np.zeros((nk,nz))
kprime = np.zeros((nk,nz))
kp_ind = np.zeros((nk,nz),dtype=int)

# Set up Markov process
z = np.array([np.exp(-sigma),np.exp(sigma)])
zprob = np.array([(1+rho)/2,(1-rho)/2,(1-rho)/2,(1+rho)/2])
zprob.shape = (2,2)


# Start the clock
start = tm.time()

# Outer loop (convergence)
for iter1 in range(maxiter):
  # Loop over both states
  for iz in range(nz):
    for ik in range(nk):
    
      # Compute consumption for each k' given (k,z)
      con = z[iz]*kgrid[ik]**alpha+(1.0-delta)*kgrid[ik]-kgrid
      # Compute per-period utility of consumption
      # if implied con is not positive, assign large negative value
      con_i = (con>0.0)*(con**(1.0-gamma)-1.0)/(1.0-gamma) \
    		+ (con<=0.0)*(-1000000.0)
      # Compute the RHS of the Bellman equation
      val = con_i + betta*v_guess@np.transpose(zprob[iz,:])
      
      # Choose the k’ that maximizes val; store the function value,
      # the grid index, and the grid value for every (k,z)
      v_new[ik,iz] = np.amax(val)
      kp_ind[ik,iz] = np.argmax(val)
      kprime[ik,iz] = kgrid[kp_ind[ik,iz]]
      
  # Compute the distance between your old guess
  # and the obtained function values
  metric = np.amax(np.abs(v_new-v_guess))
    
  # Print metric to see if the algorithm converges
  print(iter1,metric)
    
  # Check for convergence
  if metric < epsi:
    break
      
  # Set the new guess for V
  v_guess[:] = v_new
  
# Stop the clock and print elapsed time
end = tm.time()
print('Elapsed time', end-start, 'seconds')

## Plots

# Plot the resulting policy function for next period's capital
fig, ax = plt.subplots(figsize=(12,7))
ax.plot(kgrid, v_new[:,0],label="$z_{low}$")
ax.plot(kgrid, v_new[:,1],label="$z_{high}$", color='r')
ax.set_ylabel("Value function $\mathcal{V}(K_t)$", fontsize=22)
ax.set_xlabel("Capital stock $K_t$", fontsize=22)
ax.tick_params(axis="x", labelsize=12)
ax.tick_params(axis="y", labelsize=12)
ax.legend(loc='best',fontsize=20)
plt.show()
#fig.savefig("v_conv.pdf", bbox_inches='tight')

# Plot the resulting policy function for next period's capital
fig, ax = plt.subplots(figsize=(12,7))
ax.plot(kgrid, kgrid, label="45° line",color='lightgrey')
ax.plot(kgrid, kprime[:,0],label="$z_{low}$")
ax.plot(kgrid, kprime[:,1],label="$z_{high}$",color='r')
ax.set_ylabel("Capital choice $K_{t+1}$", fontsize=22)
ax.set_xlabel("Capital stock $K_t$", fontsize=22)
ax.tick_params(axis="x", labelsize=12)
ax.tick_params(axis="y", labelsize=12)
ax.legend(loc='best',fontsize=20)
plt.show()
#fig.savefig("kp_pol.pdf", bbox_inches='tight')


