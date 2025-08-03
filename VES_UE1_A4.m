%%VES - Uebung 1 Aufgabe 4


%% Aufgabenteil a) - Plotten der abgelesenen Werte

close all
clear
clc

set(0,'DefaultaxesFontsize',20)
set(0,'DefaultlineLinewidth',3)

% Zeitintervalle und Ausfaelle
time = [0.0 5 10 15 20 22];
failure_1 = [0 0.07 0.11 0.14  0.15 0.152];
failure_2 = [0 0.02 0.04 0.065 0.09 0.099];

% plot
verteilung = figure('Position',[200 200 600 350], 'WindowStyle','normal');
plot(time,failure_2,'bo','Markersize',10,'DisplayName','Telefon 2');
hold on
plot(time,failure_1,'rx','Markersize',15,'DisplayName','Telefon 1');
xlabel('\tau')
ylabel('F(\tau)')
set(gca,'XLim',[0 22]);
legend(gca,'show','Location','NorthWest')


%% Aufgabenteil b)/c) - Definition Weibull-Verteilung und Plotten des angenäherten Verlaufs

% Anonyme Funktion - Weibull-Verteilung
modelFun_Verteilung = @(p,x) (1 - exp(-(x ./ p(1)).^(p(2))));

% Startwerte
startingVals = [600 0.5];

% Regressions-Berechnung der Parameter fuer Telefon 1
coefEsts_1 = nlinfit(time, failure_1, modelFun_Verteilung,...
                            startingVals);

% Plot der Wahrscheinlichkeitsverteilung
xgrid = linspace(0,22,100);
line(xgrid, modelFun_Verteilung(coefEsts_1, xgrid),...
        'LineStyle','--', 'Color','red','DisplayName','Telefon 1');

% Regressions-Berechung der Parameter fuer Telefon 2
coefEsts_2 = nlinfit(time, failure_2, modelFun_Verteilung,...
                            startingVals);

% Plot der Wahrscheinlichkeitsverteilung
line(xgrid, modelFun_Verteilung(coefEsts_2, xgrid),...
        'Color','blue','DisplayName','Telefon 2');

disp(['Eta_1: ' , num2str(coefEsts_1(1)),'; Beta_1: ', num2str(coefEsts_1(2))])
disp(['Eta_2: ' , num2str(coefEsts_2(1)),'; Beta_2: ', num2str(coefEsts_2(2))])


%% Aufgabenteil d) - Definition Ausfallrate

% Anonyme Funktion
modelFun_Ausfallrate = @(p,x) (p(2)/p(1) * ...
                                (x./p(1)).^(p(2)-1));


%% Plotten der Ausfallrate

ausfallrate = figure('Position',[200 200 600 350],...
                            'WindowStyle','normal');
line(xgrid, modelFun_Ausfallrate(coefEsts_2, xgrid),...
                            'Color','blue',...
                            'DisplayName','Telefon 2');
xlabel('\tau')
ylabel('\lambda ( \tau )')
hold on
line(xgrid, modelFun_Ausfallrate(coefEsts_1, xgrid),...
                            'LineStyle','--', 'Color','red',...
                            'DisplayName','Telefon 1');
set(gca,'XLim',[0 22]);
title('Ausfallraten')
legend(gca,'show')

