%% Skript zur Berechnung der Batteriespannung
clear all
close all
load('messdaten.mat');
%%Parameter
%Bauteile
R0=[]; %in Ohm
R1=[]; %in Ohm
C1=[]; %in F
R2=[]; %in Ohm
C2=[]; %in F
Cdiff=[]; %in F


%Anregung
t=messdaten.t_in_s; %Vektor mit den Abgetasteten Zeitpunkten
I=[]; %Vektor mit den Stromwerten zu einem bestimmten Zeitpunkt

%Berechnung der Batteriespannung
[U_Bat] = []; %Berechnung des Spannungsvektors durch Aufruf Ihrer Funktion aus Teilaufgabe a)

% plotten der Modellspannungsantwort und der Spannungsantwort der Messdaten 
figure
subplot([]);
plot([]);
title('');
ylabel('');
ylim([]); % definiert die skalierung der y-Achse
%...





%% Erst für Teilaufgabe c)
%Berechnung der Fit Residuen und Plot 
[ Error] = []; %Berechnung des Fehlers durch Aufruf Ihrer Funktion aus Teilaufgabe c)

% Plot von U_Batterie_Modell und U_Batterie_Mess, I_Batterie und dem Fehler über der
% Zeit t
figure
ax1=subplot(3,1,1); % Eine Zuweisung des Subplots zu einer Variable macht die spätere Manipulation der Graphik einfacher. So kann in späteren plot-Befehlen spezifiziert werden, auf welches Plotfenster sich der Befehl bezieht.
ax2=subplot([]);
ax3=subplot([]);
plot(ax1, t, I, 'b');
title(ax1, 'Strom');
ylabel(ax1, 'I / A');
ylim(ax1,[-0.1 1.1]);
plot(ax2, [] ); % Hinweis, es können auch 2 Kurven gleichzeitig mit einem plot-Befehl erzeugt werden. Siehe auch "help plot"
title(ax2, '');
ylabel(ax2, '');
ylim(ax2,[]);
legend(ax2, 'U_{Modell}', 'U_{Messung}');
%...



