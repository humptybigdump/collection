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
# Grid parameters
k_min = 0.1
k_max = 5.0
nk = 200
# Technical parameters
maxiter = 10000
epsi = 1e-5

# Initialization
kgrid = np.linspace(k_min,k_max,nk)
v_guess = np.zeros(nk)
v_new = np.zeros(nk)
kprime = np.zeros(nk)
kp_ind = np.zeros(nk,dtype=int)

# Initialize temporary array for the value function
val = np.zeros(nk)

# Start the clock
start = tm.time()

# Outer loop (convergence)
for iter1 in range(maxiter):
  # Inner loop (capital grid)
  for ik in range(nk):
  
    # Loop over (discretized) capital choice
    for ikk in range(nk):
      # Compute consumption for each k' given k
      con = kgrid[ik]**alpha+(1.0-delta)*kgrid[ik]-kgrid[ikk]
      # Compute the RHS of the Bellman equation for each k’;
      # if implied con is not positive, assign large negative value
      if (con <= 0.0):
        val[ikk] = -1000000.0
      else:
        val[ikk] = (con**(1.0-gamma)-1.0)/(1.0-gamma) \
        		+ betta*v_guess[ikk]
      
    # Choose the k’ that maximizes this expression; store the
    # function value, the grid index, and the corresponding grid value
    v_new[ik] = np.amax(val)
    kp_ind[ik] = np.argmax(val)
    kprime[ik] = kgrid[kp_ind[ik]]
      
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
ax.plot(kgrid, v_new, color='black')
ax.set_ylabel("Value function $\mathcal{V}(K_t)$", fontsize=22)
ax.set_xlabel("Capital stock $K_t$", fontsize=22)
ax.tick_params(axis="x", labelsize=12)
ax.tick_params(axis="y", labelsize=12)
plt.xlim(0,4)
plt.ylim(-31,3)
plt.show()
#fig.savefig("v_conv.pdf", bbox_inches='tight')

# Plot the resulting policy function for next period's capital
fig, ax = plt.subplots(figsize=(12,7))
ax.plot(kgrid[:], kgrid[:], label="45° line",color='lightgrey')
ax.plot(kgrid[:], kprime[:],label="$K_{t+1}$")
ax.set_ylabel("Capital choice $K_{t+1}$", fontsize=22)
ax.set_xlabel("Capital stock $K_t$", fontsize=22)
ax.tick_params(axis="x", labelsize=12)
ax.tick_params(axis="y", labelsize=12)
plt.xlim(0,4)
plt.ylim(0,4.1)
ax.legend(loc='best',fontsize=20)
plt.show()
#fig.savefig("kp_pol.pdf", bbox_inches='tight')


## Compute Euler equation errors

# Create many points
neval = 1000
# Set RNG seed for replicability
np.random.seed(30042024)
# Draw uniformly distributed random numbers in [0,1] and
# transform them such that they are sorted and in [kmin,kmax]
eval_pts = np.sort(k_min+np.random.rand(neval)*(k_max-k_min))
# Compute the capital choices implied by the approximated policy
kp_int = np.interp(eval_pts,kgrid,kprime)
# Compute the respective next period’s capital choices
kpp_int = np.interp(kp_int,kgrid,kprime)
# Compute today’s and next period’s consumption
c_pol = eval_pts**alpha + (1.0-delta)*eval_pts - kp_int
cp_pol = kp_int**alpha + (1.0-delta)*kp_int - kpp_int

# Initialize the array to store the Euler eq errors
EErrors = np.zeros(neval)
# Compute consumption implied by the Euler equation
# and the Euler error for every evaluation point
for ie in range(neval):
  c_imp = (betta*((alpha*kp_int[ie]**(alpha-1.0)+1.0-delta) \
  		*cp_pol[ie]**(-gamma)))**(-1.0/gamma)
  EErrors[ie] = np.abs(c_pol[ie]/c_imp - 1.0)
  
# Maximum Euler error
max_error = np.amax(EErrors)
# Average Euler error
avg_error = np.mean(EErrors)

print('Max error: ', max_error)
print('Avg error: ', avg_error)

# Plot Euler errors
fig, ax = plt.subplots(figsize=(12,7))
ax.plot(eval_pts, EErrors)
ax.set_ylabel("Euler error $\mathcal{E}_i$", fontsize=22)
ax.set_xlabel("Capital stock $K_i$", fontsize=22)
ax.tick_params(axis="x", labelsize=12)
ax.tick_params(axis="y", labelsize=12)
plt.show()


