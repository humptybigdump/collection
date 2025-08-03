% BMML Übung 5 Lösungsskript (WS18/19)

%% a)
% Das gegebene Modell der reelen Elektrode ist mit Elektrischen Elementen
% dargestellt. Zu analytischen Zwecken muss das Modell mathematisch
% dargestellt werden. Dies ermÃ¶glicht Berechnungen, Simulationen und 
% Approximationen.
% Ein Weg das Verhalten eines beliebigen Modells zu untersuchen ist im 
% Impedanzspektrum. Die Berechnung des Impedanzspektrums wird, anhand von 
% den in der Aufgabe gegebenen Werte, durchgefÃ¼hrt und das Ergebnis im
% Nyquist Diagramm dargestellt.
% Dazu muss vorerst ein Frequenzbereich gewÃ¤hlt werden, der spÃ¤ter weiter
% angepasst werden kann.

omega = []; % Definition Frequenzbereich
R_elektrolyt = [];
R_ct = [];
C_dl = [];
Z_diff = [];
C_diff = [];


Z = []; % Berechnen der Modellimpedanz

% Ploten der Impedanz. Nutzen Sie hier Teile Ihres Codes aus verganenen Übungen 



    
%% b)
% Die Darstellung eines Warburgelemts (das fraktionale 
% Diffusionsimpedanzelement) im Zeitbereich ist nicht mÃ¶glich.
% D.h. wir mÃ¼ssen diesen Abschnitt bestmÃ¶glich mit zeitl. einsetzbaren 
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
% Eine diskrete Ãœbertragungsfunktion kann in Matlab mit dem Befehl
% tf(Zaehler,Nenner) aufgestellt werden.
% Der Stromimpuls wird in einem neuen Vektor manuell erzeugt und simuliert.
% Der Befehl "lsim" Ã¼bernimmt dabei die Auswertung der diskreten
% Ãœbertragungsfunktion auf den simulierten Stromimpuls.

Ts = []; %Schrittweite
T_puls = []; %Pulsdauer
t = []; % Zeitvektor der Simulation
I = []; % Stromvektor

% Erstellung der Übertragungsfunktion. Hinweis: tf([R_ct],[R_ct*C_dl 1])
% gibt beispielsweise die Übertragungsfunktion eines RC-Elementes aus
G = [];

U = lsim([]);

% plotten des Strompulses und der Spannungsantwort
figure(3)
    subplot(2,1,1);
%...


%% d)
% Die Messdaten müssen eingelesen werden und die richtigen Datenreihen
% ausgewählt werden. Im 2. Schritt wird die SOC auf eine Skala von 0..1 
% normiert.
% (Lade/Entladekurven von Batterien werden oft über SOC angegeben)



%% e)

t = []; % es muss ein neuer Zeitvektor zur Simulation über 30min erstellt werden.
I = []; % Berechnung eines Stromvektors, dessen Stromstärke einer Entladerate von 1C entspricht

% Modell ohne differentieller KapazitÃ¤t
G_ohne_diff_Kap = []; %Erzeugung einer neuen Übertragungsfunktion ohne differentieller Kapazität 
U_entladung_dynamik = % Bestimmung des Spannungsanteils aus der Spannungsdynamik, die später zur offenen Zellspannung aufaddiert wird.

% OCV der Entladung in 30min.
Q_entladung = cumtrapz([]); % Aufintegrierung des Stroms mit Bezug auf den Zeitvektor mittels Trapezregel
OCV_entladung = interp1([]); 

U_sim = OCV_entladung-U_entladung_dynamik;

% plotten der Ergebnisse