function res = Fun(n_H2O, T, p, R, k, m_kat, n_syngas)

% Gesamtstroffstrom mit Wasser
n_syngas(1) = n_syngas(1)+n_H2O;

% Laufvariable für ODE-Solver
x_span=[0 m_kat];

% Lösen der DGL   
[x,n] = ode45(@(x,n)DGL(x, n, T, p, R, k),x_span, n_syngas);

% Plotten
plot(x,n(:,3)./n(:,2))
xlabel('Katalysatormasse m_{Kat} in [kg]','FontSize',12)
ylabel('H_{2}/CO-Verhältnis V in [-]','FontSize',12)
drawnow

% Fehlergleichung
res = n(end,3)/n(end,2) - 2;

end