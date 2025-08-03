% BMML �bung 5 L�sungsskript (WS18/19)

%% a)
% Das gegebene Modell der reelen Elektrode ist mit Elektrischen Elementen
% dargestellt. Zu analytischen Zwecken muss das Modell mathematisch
% dargestellt werden. Dies ermöglicht Berechnungen, Simulationen und 
% Approximationen.
% Ein Weg das Verhalten eines beliebigen Modells zu untersuchen ist im 
% Impedanzspektrum. Die Berechnung des Impedanzspektrums wird, anhand von 
% den in der Aufgabe gegebenen Werte, durchgeführt und das Ergebnis im
% Nyquist Diagramm dargestellt.
% Dazu muss vorerst ein Frequenzbereich gewählt werden, der später weiter
% angepasst werden kann.

omega = []; % Definition Frequenzbereich
R_elektrolyt = [];
R_ct = [];
C_dl = [];
Z_diff = [];
C_diff = [];


Z = []; % Berechnen der Modellimpedanz

% Ploten der Impedanz. Nutzen Sie hier Teile Ihres Codes aus verganenen �bungen 



    
%% b)
% Die Darstellung eines Warburgelemts (das fraktionale 
% Diffusionsimpedanzelement) im Zeitbereich ist nicht möglich.
% D.h. wir müssen diesen Abschnitt bestmöglich mit zeitl. einsetzbaren 
% Komponenten approximieren. Das kann beliebig genau gemacht werden.
% In diesem Fall konzentrieren wird uns auf einen in Reihegeschalteten 
% Ohmschen Widerstand und ein RC-Glied.
% Zuerst muss das Impedanzmodell angepasst werden.
% Die Berechnung erfolgt analog zur a).

R_approx = [];
C_approx = [];
R_ohm_approx = [];
omega = []; % Definition Frequenzbereich

Z_approx = [];

figure(2)
% plotten von Z_approx und Z_diff in ein gemeinsames Diagramm

%% c)
% Eine diskrete Übertragungsfunktion kann in Matlab mit dem Befehl
% tf(Zaehler,Nenner) aufgestellt werden.
% Der Stromimpuls wird in einem neuen Vektor manuell erzeugt und simuliert.
% Der Befehl "lsim" übernimmt dabei die Auswertung der diskreten
% Übertragungsfunktion auf den simulierten Stromimpuls.

Ts = []; %Schrittweite
T_puls = []; %Pulsdauer
t = []; % Zeitvektor der Simulation
I = []; % Stromvektor

% Erstellung der �bertragungsfunktion. Hinweis: tf([R_ct],[R_ct*C_dl 1])
% gibt beispielsweise die �bertragungsfunktion eines RC-Elementes aus
G = [];

U = lsim([]);

% plotten des Strompulses und der Spannungsantwort
figure(3)
    subplot(2,1,1);
%...


%% d)
% Die Messdaten m�ssen eingelesen werden und die richtigen Datenreihen
% ausgew�hlt werden. Im 2. Schritt wird die SOC auf eine Skala von 0..1 
% normiert.
% (Lade/Entladekurven von Batterien werden oft �ber SOC angegeben)



%% e)

t = []; % es muss ein neuer Zeitvektor zur Simulation �ber 30min erstellt werden.
I = []; % Berechnung eines Stromvektors, dessen Stromst�rke einer Entladerate von 1C entspricht

% Modell ohne differentieller Kapazität
G_ohne_diff_Kap = []; %Erzeugung einer neuen �bertragungsfunktion ohne differentieller Kapazit�t 
U_entladung_dynamik = % Bestimmung des Spannungsanteils aus der Spannungsdynamik, die sp�ter zur offenen Zellspannung aufaddiert wird.

% OCV der Entladung in 30min.
Q_entladung = cumtrapz([]); % Aufintegrierung des Stroms mit Bezug auf den Zeitvektor mittels Trapezregel
OCV_entladung = interp1([]); 

U_sim = OCV_entladung-U_entladung_dynamik;

% plotten der Ergebnisse