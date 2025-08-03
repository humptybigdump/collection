%Start Programm "pdex1.m"
 
% Programm zum Aufrufen des Solvers, Festlegen von
% Paramtern und Ausgabe der Ergebnisse der Lösung der patiellen DGL in Diagrammform. 
 
function sol = pdex1()
 
clear
clc
close all

%Festlegen des Gitters zum Berechnen des Temperaturverlaufes in meter
% x-Richtung_Breite b = 0,20 m
% z-Richtung_Länge L = 1 m
% y-Richtung_Höhe H = 0,01 m
m = 0;  % gibt die Symmetrie an, 0 = Spalt (siehe Hilfe)
y = linspace(-0.005,0.005,500);   %(untere Grenze y, obere Grenze y, Anzahl Diskretisierungen) 
z = linspace(0,1,500);   %(untere Grenze z, obere Grenze z, Anzahl Diskretisierungen) 
 
% Aufrufen des Solvers "pdepe"; Die Rückgabe "sol" stellt den Lösungsvektor dar  
% Als Input werden m,
% die Form der Differentialgleichung "pdex1pde" (siehe extra Funktion)
% die Randbedingungen "pdex1ic" und "pdex1bc" (siehe extra Funktionen)
% und das Rechengitter, gegeben durch y und z, benötigt.
sol = pdepe(m,@pdex1pde,@pdex1ic,@pdex1bc,y,z);
% Speichern des Lösungsvektors zur weiteren Verwendung in der Variablen u
u = sol(:,:,1);
 
% Erstellen des Oberflächenplots - nicht relevant für die eigentliche Lösung
figure(1)
surf(y,z,u)
title('Temperaturprofil ebener Spalt')
xlabel('y / m')
ylabel('z / m')
zlabel('(T(y,z) - T_W) / K')
 
% Diagrammerstellung des T-Profils am Ende des Kanals 
figure(2)
plot(y,u(end,:))
title('Temperaturprofil am Ende des Kanals Z = 1 m')
xlabel('y / m')
ylabel('(T(y,z=1) - T_W) / K')

 
% Diagrammerstellung des T-Profils entlang der Symmetrieachse 
figure(3)
plot(z,u(:,250))
title('Temperaturprofil entlang der Symmetrieachse y = 0 m')
xlabel('z / m')
ylabel('(T(y=0,z) - T_W) / K')

end
 
% Festlegen der Differentialgleichung aus der allgemeinen Form durch Setzten der Größen c, f und s (siehe Hilfe)
% und durch Angabe der Parameter a = lamba/(cp*rho) und uz(y)
function [c,f,s] = pdex1pde(y,z,u,DuDx)
uz_quer = 0.006;   % mittlere Strömungsgeschwindigkeit in [m/s]
a = 0.143E-6;       % Temperaturleitfähigkeit von Wasser bei 20°C [m^2/s]
uz = (3/2)*uz_quer*(1-(y/0.005)^2); % Geschwindigkeitsprofil der Strömung
c = uz/a;
f = DuDx;
s = 0;
end
 
% Festlegen der Randbedingungen für z = 0, Gl. 20
function u0 = pdex1ic(y)
u0 = 10; %Angabe der Eintritstemperatur T_ein als Differenz zu T_W: 20 °C - 10 °C
end
 
% Festlegen der Randbedingungen für die Wand
function [pl,ql,pr,qr] = pdex1bc(xl,ul,xr,ur,z)
%Randbedingung für Wandunterseite, Gl. 19 (Syntax siehe Hilfe)
pr = ur;
qr = 0;
%Randbedingung für Wandoberseite, Gl. 18 (Syntax siehe Hilfe)
pl = ul;
ql = 0;
end
%Ende Programm "pdex1.m"
