%Start Hauptprogramm "ebenerSpalt_Nusselt.m"
 
%Aufrufen des PDE-Solver und Berechnungen zum Ermitteln der Nusselt-Zahl
 
clear 
clc
close all

% Festlegen der Diskretisierung des Rechengitters - analog "pdex1.m"
y = linspace(-0.005,0.005,500);   %(untere Grenze y, obere Grenze y, Anzahl Diskretisierungen) 
z = linspace(0,1,500);            %(untere Grenze y, obere Grenze y, Anzahl Diskretisierungen) 
uz_quer = 0.006;                  % mittlere Str�mungsgeschwindigkeit in [m/s]
 
%L�sen der partiellen DGL durch Aufruf der Funktion pdex1 aus der Datei "pdex1.m"
%Als Ergebnis wird die Matrix u zur�ckgegeben, welche T(y,z) enth�lt
u = pdex1;        

%Schleife zur Berechnung der mittleren Temperatur �ber den Querschnitt bezogen auf den
%Fl�ssigkeitsstrom
for j = 1:500   % Berechnung der mittleren Temperatur f�r jede z-Koordinate
  Tm(j,1) = 0;  
    for i = 1:500    
% Berechnung der einzelnen Terme in Abh�ngigkeit von y an der festen Stelle z = i.
%Temperatur[u(j,i)] * Geschwindigkeit an Stelle y = i.
      Tm_y(j,i) = (u(j,i)+10)*(3/2)*(1-(y(1,i)/0.005)^2);  
      Tm(j,1) = Tm(j,1) + Tm_y(j,i)/500;   %Mittelwertbildung der einzelnen Terme
    end 
end    
 
%Schleife zur Berechnung von alpha an der Stelle z aus der mittleren Temperatur 
for j = 1:499          
 alpha(j,1) = 252/2 * (Tm(j+1,1)-Tm(j,1))/(10-Tm(j,1))/(z(1,j+1)-z(1,j));
 Nu(j,1)= alpha(j,1)*2*0.01/0.6;
 zquer(j,1) = (z(1,j+1)+z(1,j))/2;  %z_quer: Bestimmung der Mitte des Volumens, �ber dem alpha berechnet wurde 
end
 
%Ausgabe der der kalorischen Mitteltemperatur entlang der z-Achse
figure(4)
plot(z(1,:),Tm(:,1))
title('Verlauf der kalorischen Mitteltemperatur entlang der z-Achse')
xlabel('z / m')
ylabel('T_{quer} / �C')

%Ausgabe der Nusseltzahl entlang der z-Achse
figure(5)
plot(zquer(:,1),Nu(:,1))
title('Verlauf der Nusselt-Zahl entlang der z-Achse')
xlabel('z / m')
ylabel('Nu-Zahl')

%Ende Programm "ebenerSpalt_Nusselt.m"


