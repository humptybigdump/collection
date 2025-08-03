
%% Bestimmung der Antoine Konstanten für MARLOHC 18/90
clc
clear 
close all

%% Experimentelle Dampfdruckdaten als Vektoren definieren 

T = [433.48 438.71 441.89 448.26 453.28 467.62 475.39 483.38 490.97 493.6 509.94 512.73 527.15 536.21 552.84 557.35 568.78 588.6];  %Gemessene Temperatur /K
p = [100,130,160,220,290,570,800,1130,1590,1710,3220,3540,5690,7560,12110,13810,18450,30610]; %Gemessender Druck /Pa

%% Plotten der Experimentellen Daten 
plot(T,p,'r*');
hold on 
xlabel('Temperatur K');
ylabel('Druck Pa');

%% Fitten der Arrhenius Parameter A, B & C nach der Methode der kleinsten Fehlerquadrate

Antoine = @(c0,T) 10.^(c0(1)-(c0(2)./(T+c0(3)))); %Antoine Gleichung PLV [Pa] = 10^(A-(B/T[K]+C))definieren

options = optimoptions('lsqcurvefit', 'MaxFunctionEvaluations', 1000);
c0 = [12 6000 150]; %Startwerte A,B & C (c0(1),B c0(2),C c0(3)) erzeugen 
[c0,resnorm] = lsqcurvefit(Antoine, c0,T,p, [], [], options); %Fitten der Konstanten nach der Methode der kleinsten Fehlerquadrate

%% Plotten der Dampfdrücke berechnet aus den gefitteten Antoine Parametern

p_Fit = 10.^(c0(1)-(c0(2)./(T+c0(3)))); %Dampfdrücke mit gefitteten Antoine Parametern berechnen
plot(T, p_Fit); %Plotten der Drücke über der Temperatur