%% Messdaten erfassen/simulieren
PT1_Generiere_Messdaten_Zustandsraum; %simulation mit Anfangswert verschiedene Ruhelagen m�glich


%% Parameteridentifikation allgemein Arbeitspunkt/Ruhelage (Kleinsignalverhalten)!
%1. Arbeitspunkt/Ruhelage abziehen
y = y_mess - y_e;
u = u_mess - u_e;

%% Parameteridentifikation LS-Gleichungsfehler DGL (zeit-kontinuierlich)  PT1 ohne Anfangswertsch�tzung
% Modell DGL: dy/dt = - 1/T*y + K/T*u
% theta = [1/T, K/T]

% Zustandsvariablenfilter (ZVF) initialisieren
T_filter = 10;
f_0 = 1/T_filter^2;
f_1 = 2/T_filter;
A = [0 1; -f_0 -f_1];
b = [0; f_0];
C = eye(2);
d = 0;
ZVF = ss(A,b,C,d);

% zeitdiskrete Realisierung des ZVF f�r Eingang
ZVFdu = c2d(ZVF, T_A, 'zoh'); %'zoh' wegen Annahme, u ist st�ckweise konstant

% zeitdiskrete Realisierung des ZVF f�r Ausgang
ZVFdy = c2d(ZVF, T_A, 'foh'); %'foh' wegen Annahme, y ist glatt

% ZVF anwenden
% Problem hier, Anfangswert des Systems muss bekannt, wenn Experiment nicht aus Ruhelage, dann Fehler 
% (Alternativ kann Systemanfangswert mitgesch�tzt werden s.u.)
y_f = lsim(ZVFdy,y,t,[0; 0]);
%y_f = lsim(ZVF,y(:,1),t,[0; 0]); %evtl. etwas besser, da akausale
%Signalrekonstruktion durch lsim m�glich
u_f = lsim(ZVFdu,u,t,[0; 0]);


% gefilteret Signale f�r Least-Squares nutzen
y_N = y_f(:,2);                %Messvektor 
S_N = [-y_f(:,1) u_f(:,1)];    %Datenmatrix
theta = pinv(S_N)*y_N;    %Least-Squares-L�sung  

disp('Gleichungsfehlerbasierte Least-Squares zeitkontinuierlich ohne Anfangswertsch�tzung:')
K_est = theta(2)/theta(1)    %Gesch�tzte Strecken-Verst�rkung
T_est = 1/theta(1)           %Gesch�tzte Strecken-Zeitkonstante

%% Parameteridentifikation LS-Gleichungsfehler DGL (zeit-kontinuierlich)  PT1 mit Anfangswertsch�tzung

% f�r Sch�tzung Anfangswert y_0
eins_f = lsim(ZVFdu,ones(size(y)),t,[0; 0]);

% gefilteret Signale f�r Least-Squares nutzen
y_N = y_f(:,2);                %Messvektor 
S_N = [-y_f(:,1) u_f(:,1) eins_f(:,2)];    %Datenmatrix erweitert
theta = pinv(S_N)*y_N;    %Least-Squares-L�sung  

disp('Gleichungsfehlerbasierte Least-Squares zeitkontinuierlich mit Anfangswertsch�tzung:')
K_est = theta(2)/theta(1)    %Gesch�tzte Strecken-Verst�rkung
T_est = 1/theta(1)           %Gesch�tzte Strecken-Zeitkonstante
y_0_est = theta(3)           %Gesch�tzter Anfangswert


 %% Parameteridentifikation LS-Gleichungsfehler (zeit-diskret)
% Modell Differenzengleichung: y[k] = a*y[k-1] + b*u[k-1]
% a = exp(-T_A/T)
% b = (1-a)*K
% theta = [a, b]

y_v = y(2:length(y));
S = [y(1:length(y)-1) u(1:length(y)-1)];

theta = pinv(S)*y_v;    %Least-Squares-L�sung   
a = theta(1);
b = theta(2);

disp('Gleichungsfehlerbasierte Least-Squares zeitdiskret:')
% Physikalische Parameter K,T m�ssen aus den gesch�tzten berechnet werden
K_est = b/(1-a)      %Gesch�tzte Strecken-Verst�rkung 
T_est = -T_A/log(a)  %Gesch�tzte Strecken-Zeitkonstante

% Sch�tzung bei vorliegen von Messrauschen i.Allg. schlechter als bei
% zeitkontinuierlich

% Sch�tzung wird schlechter, wenn mit h�herer Abtastfrequenz (kleiners T_A)
% gemessen wird

%% Parameteridentifikation �ber Ausgangsfehlerminimierung
% Strukturell bekannte zeitkontinuierliche L�sung: y = K*(1-exp(-t/T))
% theta = [K, T]
%separate Funktion implementieren, die zu einem gegebenen Parametersatz theta den
%Fehler zwischen Modell und Messung zur�ckliefert:


%PT1
 theta_0=[5,100]; %Startwert f�r Optimierung muss ungef�hr stimmen, sonst lokales Minimum
 disp('Ausgangsfehlerbasierte Least-Squares zeitkontinuierlich unter Annahme Anfangswert x_0 = 0:')
 theta_opt = fminsearch(@PT1_Model_Error, theta_0)
 
 disp('Ausgangsfehlerbasierte Least-Squares zeitkontinuierlich mit Sch�tzung des Anfangswerts:')
 theta_opt = fminsearch(@PT1_Model_Error_x0, [theta_0, 5])
  


