% VES - Uebung 3 Aufgabe 4

% WICHTIGER HINWEIS: Zunächst muss das Simulink-Modell
% VES_UE4_Modell.slx ausgeführt werden!

close all;
clc;
set(0,'DefaultaxesFontsize',20)
set(0,'DefaultlineLinewidth',3)


%% Parameter

% Anzahl Bins
N = 100;

% Anzahl Bins + Zwischenzeitpunkte
Nt = 2*N+1;

% Zeitachse
t = linspace(0,10,Nt);

% Histogrammbins
bins = t(2:2:Nt-1);

% Ankunftsrate aus Simulink-Daten auslesen
lambda = lambda(1);


%% Aufgabenteil b) - Systemzustand

tt = cumsum(tau);
figure; stairs(tt,0:numel(tau) - 1,'LineWidth',3);
xlabel('Zeit \itt\rm in s');
ylabel('\itx\rm(\itt\rm)');


%% Aufgabenteil b) - Systemzustand (Vergleich)

hold on;
plot([0 tt(end)],[0 tt(end)*lambda],'r');
hold off;
legend('\itx\rm(\itt\rm)','\it\lambdat','Location','NorthWest');


%% Aufgabenteil b) - Histogramm

figure; hist(tau,bins);
h = findobj(gca,'Type','patch');
set(h,'FaceColor',[0 0.4470 0.7410],'EdgeColor','w')
xlabel('\it\tau\rm in s')
ylabel('\ith\rm(\it\tau\rm)');


%% Aufgabenteil b) - Histogramm (Vergleich)

hold on;
plot(bins,numel(tau) * (-exp(-lambda * t(3:2:Nt)) + exp(-lambda * t(1:2:Nt-2))),'r');
hold off;
legend('\ith\rm(\it\tau\rm)','\itf\rm(\it\tau\rm)');


