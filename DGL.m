% Funktion DGL.m

% W�hrend der ode-Solver das DGL-System l�st, ruft er wiederholt diese
% Funktion G auf, �bergibt dabei den Zeitpunkt t und den Momentanzustand Z.

% Innerhalb der Funktion "DGL" muss man aus den Werten von t und Z alle
% Zeitableitungen Zp, d.h. die momentanen "�nderungsraten" berechnen und 
% auf Zp schreiben.

function Zp=DGL(t,Z ,k1, k2, k3)

% Innerhalb der Funktion werden die gew�nschten Konstanten bzw. Variablen
% aufgerufen und zugewiesen. Falls auf Konstanten, Daten oder Variablen 
% aus dem Hauptprogramm zugegriffen werden sollen, m�ssen diese �bergeben
% werden

% Zustandsg��en aus dem Zustandsvektor Z entnehmen (optional):
CA=Z(1);
CB=Z(2);
CC=Z(3);
CD=Z(4);
CE=Z(5);

% Gleichungen f�r Hilfsgr��en aufstellen und berechnen:
r1=k1*CA^2*CB;
r2=k2*CC*CB;
r3=k3*CB*CD^2;

% Die zu l�senden DGL-Gleichungen(�nderungsraten) aufstellen und berechnen:
CAp=-2*r1;
CBp=r1-r2-r3;
CCp=r1-r2;
CDp=r2-2*r3;
CEp=r3;

% Zuletzt R�ckgabevektor zusammenstellen
Zp=[CAp; CBp; CCp; CDp; CEp];

end

