%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Signalverarbeitung in der Geodaesie - SoSe21
%%% Alexandra Heck
%%%
%%% Template zur Uebung 3
%%%
%%% Bearbeitet von:
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clear all; close all;

%% Definition von dummy-Parametern fuer das Template,
%  diese sind bei der Bearbeitung zu ersetzen.
%  Entsprechend ist "dummy" auch in den Beschriftungen der Abbildungen zu
%  ersetzen.

x_dummy = 0:0.01:2000;
y_dummy = 0.1*x_dummy;
dummy = 0.1;

%% Einlesen der Daten

load 'IBISKinematic_example_data';

% Bestimmung des mittleren Abtastintervalls
dt = max(Time)/(length(Time)-1);

% Definition des Zeitvektors
t = 0:dummy:max(Time);

%% Aufgabenteil 1

% "Vorprozessierung" laut Folie 8,Foliensatz "Kap3_Leistungsdichtespektrum"
% (Bereinigung des Mittelwertes)
Displacement(:,1)=Displacement(:,1)-dummy;
Displacement(:,2)=Displacement(:,2)-dummy;

% Berechnung der Autokorrelationsfunktion und Kreukorrelation mittels xcorr()
[d1_autoc,lag] = xcorr(Displacement(:,1),'coeff');
d2_autoc = xcorr(Displacement(:,2),'coeff');
d1_xcorr = xcorr(Displacement(:,1),Displacement(:,2),'coeff');

lag = lag * dt;

figure;
plot(x_dummy,y_dummy)
hold all
plot(x_dummy,y_dummy)
xlabel('Verschiebung (lag) in dummy')
ylabel('Normierte Korrelation');
axis tight
legend('Autokorrelation 1 (19,3m)','Autokorrelation 2 (31,9m)');

%% Aufgabenteil 2

% Laenge des Hanningfensters
L = [2^8 2^12 2^16 nt];
Lzeit = round(L*dt) ; % in s

nover = 0;
fs = 1/dt;

%% LDS fuer Displacement 19,3m Hoehe

% (Falls jemand eine "elegantere" Loesung fuer die naechten vier Zeilen
% hat, waere ich sehr daran interessiert!)
[G1_1,f1_1] = pwelch(y_dummy,hann(L(1)),nover,[],fs);
[G2_1,f2_1] = pwelch(y_dummy,hann(L(2)),nover,[],fs);
[G3_1,f3_1] = pwelch(y_dummy,hann(L(3)),nover,[],fs);
[G4_1,f4_1] = pwelch(y_dummy,hann(L(4)),nover,[],fs);

figure;
plot(x_dummy,y_dummy) % G4
hold all;
plot(x_dummy,y_dummy) % G3
plot(x_dummy,y_dummy,'LineWidth',2) % G2
plot(x_dummy,y_dummy,'LineWidth',2) % G1
xlabel('Frequenz in dummy')
ylabel('LDS in dummy')
legend(['L = ',num2str(L(4)),' --> ',num2str(Lzeit(4)),' s'],...
       ['L = ',num2str(L(3)),' --> ',num2str(Lzeit(3)),' s'],...
       ['L = ',num2str(L(2)),' --> ',num2str(Lzeit(2)),' s'],...
       ['L = ',num2str(L(1)),' --> ',num2str(Lzeit(1)),' s']);
%xlim([5 50]);
title('Zeitreihe 1 (h = 19,3m)')

%% LDS fuer Displacement 31.9m Hoehe

[G1_2,f1_2] = pwelch(y_dummy,hann(L(1)),nover,[],fs);
[G2_2,f2_2] = pwelch(y_dummy,hann(L(2)),nover,[],fs);
[G3_2,f3_2] = pwelch(y_dummy,hann(L(3)),nover,[],fs);
[G4_2,f4_2] = pwelch(y_dummy,hann(L(4)),nover,[],fs);

figure;
plot(x_dummy,y_dummy) % G4
hold all;
plot(x_dummy,y_dummy) % G3
plot(x_dummy,y_dummy,'LineWidth',2) % G2
plot(x_dummy,y_dummy,'LineWidth',2) % G1
xlabel('Frequenz in dummy')
ylabel('LDS in dummy')
legend(['L = ',num2str(L(4)),' --> ',num2str(Lzeit(4)),' s'],...
       ['L = ',num2str(L(3)),' --> ',num2str(Lzeit(3)),' s'],...
       ['L = ',num2str(L(2)),' --> ',num2str(Lzeit(2)),' s'],...
       ['L = ',num2str(L(1)),' --> ',num2str(Lzeit(1)),' s']);
%xlim([5 50]);
title('Zeitreihe 2 (h = 31,9m)')

%% Aufgabenteil 3

alpha = dummy;
p=1-alpha;

[G1_1,f1_1,G1_95] = pwelch(Displacement(:,1),hann(L(1)),nover,[],fs,'ConfidenceLevel',p);
[G2_1,f2_1,G2_95] = pwelch(Displacement(:,1),hann(L(2)),nover,[],fs,'ConfidenceLevel',p);
[G3_1,f3_1,G3_95] = pwelch(Displacement(:,1),hann(L(3)),nover,[],fs,'ConfidenceLevel',p);
[G4_1,f4_1,G4_95] = pwelch(Displacement(:,1),hann(L(4)),nover,[],fs,'ConfidenceLevel',p);

figure;
plot(x_dummy,y_dummy,'k--');
hold all;
plot(x_dummy,y_dummy);
title('Konfidenzintervall 95%')
xlabel('Frequenz in dummy')
ylabel('LDS in dummy')
legend('untere Schranke','obere Schranke','LDS')
%xlim([5 50]);


