%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Signalverarbeitung in der Geodaesie - SoSe2021
%%% Alexandra Heck
%%%
%%% Uebung 4 - Aufgabe b
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clear all;
close all;

N = 6864;                   % Anzahl Datenpunkte
dt = 1;                     % Sampling Interval in h
t = (0:N-1)*dt;
f_g = 0.015;                % Grenzfrequenz in 1/h
f_ny=1/(2*dt)               % Nyquistfrequenz

tau = ((-N/2):(N/2)-1)*dt;  % Zeitachse tau

% Formel (9.7) Skript und Formel (1) der Aufgabenstellung
h_t = transpose(1./(pi * tau) .* (sin(2*pi*tau*f_ny)-sin(2*pi*tau*f_g)));
syms x;
h_t(find(isnan(h_t))) = limit(1./(pi * x) .* (sin(2*pi*x*f_ny)-sin(2*pi*x*f_g)),x,0);

figure
plot(tau,h_t)
xlabel('tau in h')
ylabel('Impulsanwort h(tau)')

h_t_shift = ifftshift(h_t);

figure
plot(t,h_t_shift)
xlabel('tau in h')
ylabel('Impulsanwort h(tau)')