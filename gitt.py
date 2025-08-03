#
# GITT Charge
#
import pybamm
import numpy as np
import matplotlib.pyplot as plt

pybamm.set_logging_level("INFO")
experiment = pybamm.Experiment(
    [("Charge at C/40 for 1 hour", "Rest for 1 hour")] * 40,
)

#parameter_values = pybamm.ParameterValues("OKane2022")

model = pybamm.lithium_ion.DFN()
sim = pybamm.Simulation(model, experiment=experiment, solver=pybamm.CasadiSolver())
sim.solve(initial_soc=0.001)
sim.plot()

t = sim.solution["Time [s]"].entries
I = sim.solution["Current [A]"].entries
V = sim.solution["Terminal voltage [V]"].entries
Cap = sim.solution["Discharge capacity [A.h]"].entries


dI = np.diff(np.abs(I))
an = np.where(dI>0)[0]#weil die ströme neg sind ...
aus = np.where(dI<0)[0]

'''
a,b = 8,10
plt.plot(t[aus[a]:aus[b]],V[aus[a]:aus[b]])
plt.show()

E0,E1 = V[aus[a]-1],V[aus[a]+1]
E2,E3 = V[an[a]-1],V[an[a]+1]
E4 = V[aus[a+1]-1]
Deq = ((E4-E0)/(E2-E1))**2

'''

Deqs = []
SOCs = []
for a,b in zip(range(len(aus)),range(len(an))):
    E0,E1 = V[aus[a]-1],V[aus[a]+1]
    E2,E3 = V[an[a]-1],V[an[a]+1]
    E4 = V[aus[a+1]-1]
    Deq = ((E4-E0)/(E2-E1))**2
    Deqs.append(Deq)
    SOCs.append(Cap[aus[a]])


plt.plot(SOCs,Deqs)
plt.show()


# Challenge:
# Implementiert ein oder zwei mögliche weitere Methoden aus DOI: 10.1149/1945-7111/ab9404

import itertools as it 
a = [g for g in it.product([1,2,3],repeat=2)]


plt.figure()
plt.plot()
plt.savefig('.svg')


a = dict()
a[0] = [0,1,2,3]
a[1] = [4,5]

