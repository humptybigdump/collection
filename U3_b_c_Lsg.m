%% Skript zur Berechnung der Batteriespannung
clear all
close all
keyboard
load('messdaten.mat');

% Plot von U_Batterie_Mess, I_Batterie über der Zeit t
%Anregung
t=messdaten.t_in_s; %Vektor mit den Abgetasteten Zeitpunkten
I=messdaten.I_in_A; %Vektor mit den Stromwerten zu einem bestimmten Zeitpunkt

figure
ax1=subplot(2,1,1);%Strom
ax2=subplot(2,1,2);%Spannung
plot(ax1, t, I, 'b');
title(ax1, 'Strom');
ylabel(ax1, 'I / A');
ylim(ax1,[-0.1 1.1]);
plot(ax2, t, messdaten.U_in_V, 'k:' );
title(ax2, 'Spannung');
ylabel(ax2, 'U / V');
ylim(ax2,[3.88 4.05]);
legend(ax2, 'U_{Modell}', 'U_{Messung}');
keyboard

%%Parameter
%Bauteile
R0=0.01; %in Ohm
R1=0.1; %in Ohm
C1=100; %in F
R2=0.03; %in Ohm
C2=6600; %in F
Cdiff=10000; %in F



%Anregung
t=messdaten.t_in_s; %Vektor mit den Abgetasteten Zeitpunkten
I=messdaten.I_in_A; %Vektor mit den Stromwerten zu einem bestimmten Zeitpunkt

%%Berechnung der Batteriespannung
[U_Bat] = U_Batterie( t, I, R0, R1, C1, R2, C2, Cdiff ); %Berechnung des Spannungsvektors

keyboard

%Berechnung der Fit Residuen und Plot 
[ Error] = Residuen( U_Bat, messdaten.U_in_V );



% Plot von U_Batterie_Modell und U_Batterie_Mess, I_Batterie und dem Fehler über der
% Zeit t
figure
ax1=subplot(3,1,1);%Strom
ax2=subplot(3,1,2);%Spannung
ax3=subplot(3,1,3);%Residuen
plot(ax1, t, I, 'b');
title(ax1, 'Strom');
ylabel(ax1, 'I / A');
ylim(ax1,[-0.1 1.1]);
plot(ax2, t, U_Bat,'k', t, messdaten.U_in_V, 'k:' );
title(ax2, 'Spannung');
ylabel(ax2, 'U / V');
ylim(ax2,[3.88 4.05]);
legend(ax2, 'U_{Modell}', 'U_{Messung}');
plot(ax3, t, Error, 'r');
title(ax3, 'Fehler');
ylabel(ax3, 'Error / %');
xlabel(ax3,'t / s');



