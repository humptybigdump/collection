% VES - Uebung 4 Aufgabe 3

clear variables;
close all;
clc;
set(0,'DefaultaxesFontsize',15)
set(0,'DefaultlineLinewidth',2)

%% Parameter
rho = 0:0.02:1;
mu = 2;

%% Aufgabenteil b) - Theoretische mittlere Systemzeit
tau_d = 1 / mu * (2 + 2 * rho - rho.^2) ./ (2 + 4 * rho + 3 * rho.^2) ./ (1 - rho);
tau_d3 = 1 ./ (mu - rho * mu);

% Ergebnis darstellen
plot(rho,tau_d);
hold on;
plot(rho,tau_d3,'r-');
axis([0 1 0 5]);
xlabel('\it\rho\rm');
ylabel('\it\tau\rm_d in s');
legend('M/M/3-Warteschlangensystem','3x M/M/1 parallel','Location','NorthWest');

%% Aufgabenteil c) - Simulation

% Simulationsparameter
iterationen = length(rho);

% Variablen fuer Ergebnisse
tau_d = zeros(1,iterationen);
tau_d1 = zeros(1,iterationen);
tau_d2 = zeros(1,iterationen);
tau_d3 = zeros(1,iterationen);

% Simulink-Modell oeffnen
mdl = 'VES_UE4_A3_modell';
isModelOpen = bdIsLoaded(mdl);
open_system(mdl);

% Simulation fuer jeden Parameter rho durchfuehren
for idx = 2:iterationen % rho = 0 nicht simulieren
    load_system(mdl);
    set_param([mdl '/Arrivals'], 'eventrate', num2str(rho(idx) * 3 * mu));
    simout = sim(mdl,'SimulationMode','normal');
    tmp_d = get(simout,'tau_d');
    tmp_d1 = get(simout,'tau_d1');
    tmp_d2 = get(simout,'tau_d2');
    tmp_d3 = get(simout,'tau_d3');
    tau_d(idx) = tmp_d(end);
    tau_d1(idx) = tmp_d1(end);
    tau_d2(idx) = tmp_d2(end);
    tau_d3(idx) = tmp_d3(end);
end

% Simulink-Modell schliessen
if(~isModelOpen)
    close_system(mdl, 0);
end

% Ergebnis darstellen
plot(rho(2:end),tau_d(2:end),'c');
plot(rho(2:end),(tau_d1(2:end)+tau_d2(2:end)+tau_d3(2:end))/3,'m');
hold off;
legend('M/M/3-Warteschlangensystem','3x M/M/1 parallel','M/M/3 Simulation','3x M/M/1 Simulation','Location','NorthWest');
