import numpy as np
import matplotlib.pyplot as plt
from scipy import signal

# Annahmen für die Parameter
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
use_controller = False

# Simulationszeit in Sekunden
sim_time = 60

# Übertragungsfunktion G(s) = (k_m/R)/(1 + sL/R) * (1/d)/(1 + sJ/d)
numerator = [k_m / (R * d)]
denominator = [L * J / (R * d), (L / R + J / d), 1]
system_continuous = signal.TransferFunction(numerator, denominator)

if use_controller:
    # Hinzufügen des Integrationsanteils des PI-Reglers K_p (1 + 1/(sT_i))
    numerator_PI = [K_p * T_i, K_p]
    denominator_PI = [T_i, 0]

    numerator_total = np.polymul(numerator_PI, numerator)
    denominator_total = np.polymul(denominator_PI, denominator)

    # Open-loop Übertragungsfunktion
    open_loop = signal.TransferFunction(numerator_total, denominator_total)

    # Closed-loop Übertragungsfunktion T(s) = L(s) / (1 + L(s))
    closed_loop_numerator = open_loop.num
    closed_loop_denominator = np.polyadd(open_loop.den, open_loop.num)
    system_continuous = signal.TransferFunction(closed_loop_numerator, closed_loop_denominator)

# Berechne die Pole des Systems
poles = system_continuous.poles
print("Poles of the continuous system:", poles)

# Diskretisierung mit Tustin-Approximation (ohne Totzeit)
system_discrete = system_continuous.to_discrete(T_a, method='bilinear')

# Zeitvektor für Simulation
t = np.arange(0, sim_time, T_a)
# Einheitssprung als Eingangsgröße
u = np.ones_like(t)
if use_controller:
    u *= soll_wert

# Simulation des diskretisierten Systems
t_out, y_out = signal.dlsim(system_discrete, u, t)

# Berücksichtigung der Totzeit (e^(-s2T_a))
# Die Totzeit von 2T_a entspricht einer Verzögerung um 2 Zeitschritte
y_out_with_delay = np.concatenate((np.zeros((2, 1)), y_out[:-2]))

# Plotten der Ergebnisse
plt.figure(figsize=(10, 5))

plt.plot(t_out, y_out_with_delay, label='Winkelgeschwindigkeit omega')

plt.xlabel('Zeit [s]')
plt.ylabel('Winkelgeschwindigkeit [rad/s]')
if use_controller:
    plt.title('Systemantwort einer Drehstrommaschine mit PI-Regler und Totzeit')
else:
    plt.title('Systemantwort einer Drehstrommaschine mit Totzeit')
plt.grid(True)
plt.legend()
plt.show()
