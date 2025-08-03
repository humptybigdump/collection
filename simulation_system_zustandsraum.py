import numpy as np
import matplotlib.pyplot as plt
from scipy import signal

# Systemparameter
R = 1.0  # Ohm
L = 0.5  # Henry
J = 0.5  # kg*m^2
d = 0.5  # N*m*s
k_m = 0.1  # N*m/A
T_a = 0.01  # Abtastzeit
soll_wert = 2.0  # Sollwert für die Winkelgeschwindigkeit

# PI-Reglerparameter (z.B. aus der Ziegler-Nichols-Methode)
K_p = 0.8
T_i = 0.5

# Soll der PI-Regler genutzt werden?
use_controller = True

sim_time = 60  # Simulationszeit in Sekunden

# Zustandsraumdarstellung
A = np.array([[-R / L, 0],
              [k_m / J, -d / J]])
B = np.array([[1 / L],
              [0]])
C = np.array([[0, 1]])
D = np.array([[0]])

# Diskretisierung
system_continuous = signal.StateSpace(A, B, C, D)
system_discrete = system_continuous.to_discrete(T_a)

# Simulationseinstellungen
t = np.arange(0, sim_time, T_a)
# Einheitssprung als Eingangsgröße
u = np.ones_like(t)  

# Speicher für Zustände und Ausgang
x = np.zeros((2, len(t)))
y = np.zeros(len(t))
u_regler = np.zeros(len(t))
e = np.zeros(len(t))

# Simulation
# Totzeit von 2 Schritten
for k in range(2, len(t)):
    # Fehlerberechnung
    e[k] = soll_wert - y[k-1]
    
    # PI-Regler
    u_regler[k] = K_p * e[k] + K_p * (T_a / T_i - 1) * e[k-1] + u_regler[k-1]
    
    # Zustandsraum-Update
    if use_controller:
        x[:, k] = system_discrete.A @ x[:, k-1] + system_discrete.B.flatten() * u_regler[k]
        y[k] = system_discrete.C @ x[:, k] + system_discrete.D * u_regler[k]
    else:
        x[:, k] = system_discrete.A @ x[:, k-1] + system_discrete.B.flatten() * u[k]
        y[k] = system_discrete.C @ x[:, k] + system_discrete.D * u[k]
    # print(u_regler[k])

# Plotten der Ergebnisse
plt.figure(figsize=(10, 5))
plt.plot(t, y, label='Winkelgeschwindigkeit omega')
plt.xlabel('Zeit [s]')
plt.ylabel('Winkelgeschwindigkeit [rad/s]')

if use_controller:
    plt.title('Systemantwort einer Drehstrommaschine mit PI-Regler und Totzeit')
else:
    plt.title('Systemantwort einer Drehstrommaschine ohne Regler und mit Totzeit')

plt.grid(True)
plt.legend()
plt.show()

