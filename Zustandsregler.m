close all
%% a) System erstellen und Eigenbewegung ohne Regler simulieren
A=[0 1;-1 -0.1];
B=[0;1];
C=[1 0];
D=0;
x0=[0;1];
sys=ss(A,B,C,D);

%Simulation Strecke mit u=0
t=[0:0.1:100]';
u=zeros(1001,1);
[y,t,x]=lsim(sys,u,t,x0);

figure; %Zeitverlaufsbild
plot(t,x(:,1),t,x(:,2),'linewidth',2);
legend('Weg x_1','Geschwindigkeit x_2');
xlabel('Zeit in s');
ylabel('Weg in m und Geschwindigkeit in m/s');
figure; %Phasenportr�t
plot(x(:,1),x(:,2));
xlabel('Weg in m');
ylabel('Geschwindigkeit in m/s');

%% b) LQ-Regler entwerfen
Q = eye(2);
%Q=C'*C; % wenn bzgl. Ausgangsfehler optimiert werden soll.
R = 1;
%R = 0.1 % es werden gr��ere Reglerverst�rkungen und damit gr��ere Stellgr��en generiert
%R = 10  % es werden kleinere Reglerverst�rkungen und damit kleinere Stellgr��en generiert
K = lqr(A,B,Q,R)
F = inv((C-D*K)*inv(-A+B*K)*B+D)

% Simulation des Zustandsregelkreises (Stabilisierung)
sys_RK=ss(A-B*K,B*F,C-D*K,D*F); % Siehe Folie 9 vd = F*y_d
t=[0:0.1:14]'; %kuerzere Zeit, da Regelkreis schneller
yd=zeros(141,1); %Einregeln nach Anfangsst�rung (Anfangsgeschwindigkeit)
[y,t,x]=lsim(sys_RK,yd,t,x0);

figure; %Zeitverlaufsbild
plot(t,x(:,1),t,x(:,2),'linewidth',2);
xlabel('Zeit in s');
ylabel('Weg in m und Geschwindigkeit in m/s');
figure; %Phasenportr�t
plot(x(:,1),x(:,2),'linewidth',2);
xlabel('Weg in m');
ylabel('Geschwindigkeit in m/s');
figure; %Stellverlauf
plot(t,-x*K'+F*yd,'r','linewidth',2);
xlabel('Zeit in s');
ylabel('Kraft (u) in N');

%% c) Simulation des Zustandsregelkreises (F�hrungsverhalten) ohne Modellfehler
x0 =[0;0];
yd=ones(141,1);
[y,t,x]=lsim(sys_RK,yd,t,x0);

figure; %Zeitverlaufsbild
plot(t,x(:,1),t,x(:,2),'linewidth',2);
xlabel('Zeit in s');
ylabel('Weg in m und Geschwindigkeit in m/s');
figure; %Stellverlauf
plot(t,-x*K'+F*yd,'r','linewidth',2);
xlabel('Zeit in s');
ylabel('Kraft (u) in N');
%Jetzt ist es an der Zeit, den Einfluss von R zu untersuchen.
%Hierzu ist oben R=0.1 und R=10 einzuklammern.
%Kleine R bedeuten geringe Bestrafung des Stellaufwands, also sportliche
%Regler
%Wer Lust hat, dreht jetzt am Q
%Q=C'*C bewertet den quadratische Ausgangsfehler, oben ausklammern
%Der flei�ige Student vergleicht die Ausgangsfehlerquadratfl�chen
%F�r Q=C'*C ist die Fl�che kleiner.


%% d) Simulation des Zustandsregelkreises (F�hrungsverhalten) mit Modellfehlern
%Identifiziertes Modell, das leicht von tats�chlichem System abweicht:
A_dach=[0 1;-1.1 -0.11];
B_dach=[0;0.9]; 
C_dach=[1 0];
D_dach=0;

%Reglerentwurf auf Basis fehlerhaftem Modell --> fehlerhafter Regler:
K_dach=lqr(A_dach,B_dach,Q,R)
F_dach=inv((C_dach-D_dach*K_dach)*inv(-A_dach+B_dach*K_dach)*B_dach+D_dach)

%Simulation des geschlossenen Regelkreises mit fehlerhaftem Regler
sys_RKmitFehlern=ss(A-B*K_dach,B*F_dach,C-D*K_dach,D*F_dach);
% Hinweis: F�r den Reglerentwurf (inkl. Vorfilter) liegt nur das
% fehlerbehaftete Modell vor. Der Regler wird aber dann auf das echte
% System angewendet bzw. hier mit echtem System simuliert. 
[y,t,x]=lsim(sys_RKmitFehlern,yd,t,x0);

figure; %Zeitverlaufsbild
plot(t,x(:,1),t,x(:,2),'linewidth',2);
xlabel('Zeit in s');
ylabel('Weg in m und Geschwindigkeit in m/s');
figure; %Stellverlauf
plot(t,-x*K_dach'+F_dach*yd,'r','linewidth',2);
xlabel('Zeit in s');
ylabel('Kraft (u) in N');
%deutlich zu erkennen ist, dass keine station�re Genauigkeit mehr vorliegt

%% e) Zustandsregler mit Integrator (PI-Regler) mit Modellfehlern
% Strecke virtuell um Integrator erweitern (Folie 34)
K_pireg = lqr([A_dach zeros(2,1);C_dach 0],[B_dach; D_dach],blkdiag(Q,1),R) 
%(Zustandsgroessenwichtung auch fuer Integrator --> Q um 1 erweitern)
K_x = K_pireg(1:2); 
K_xi = K_pireg(3); %Plus, da Regelfehler bei uns definiert mit y-yd

%Alternative �ber lqi-Befehl:
%K_pireg = lqi(ss(A_dach,B_dach,C_dach,D_dach),[Q,[0;0];0,0,1],R);
%K_x = K_pireg(1:2); 
%K_xi = -K_pireg(3); %Minus, da Regelfehler in Matlab definiert mit yd-y

F_pireg=inv((C_dach-D_dach*K_x)*inv(-A_dach+B_dach*K_x)*B_dach+D_dach)
%F_pireg=inv((C-D*K_x)*inv(-A+B*K_x)*B+D)

% Simulation geschlossener Regelkreis mit Zustands-PI-Regler
sys_RKPI=ss([A-B*K_x -B*K_xi; C-D*K_x -D*K_xi],[B*F_pireg; D*F_pireg-eye(1)],[C-D*K_x -D*K_xi], D*F_pireg);
[y,t,x]=lsim(sys_RKPI,yd,t,[x0;0]); %Integratoranfangswert auf null

figure; %Zeitverlaufsbild
plot(t,x(:,1),t,x(:,2),'linewidth',2);
xlabel('Zeit in s');
ylabel('Weg in m und Geschwindigkeit in m/s');
figure; %Stellverlauf
plot(t,-x*K_pireg'+F_pireg*yd,'r','linewidth',2);
xlabel('Zeit in s');
ylabel('Kraft (u) in N');
figure; %Integratoranteil
plot(t,-x(:,3)*K_xi,'c','linewidth',2);
xlabel('Zeit in s');
ylabel('Integratoranteil in N');
%deutlich zu erkennen ist, dass station�re Genauigkeit vorliegt
%Wird F_pireg aus den wahren Streckenmatrizen berechnet, steht im Integrator tats�chlich station�r eine Null
%Dieser Fall kann durch L�schen des Kommentars vor F_pidreg �berpr�ft
%werden

% Wegen PI-Zustandsregler ist bei Stellbergenzungen noch eine AW-Ma�nahme zu entwerfen
% Das geht dann einfacher mit Simulink. 
% Siehe dazu Simulink-Modell: Zustandsregker_mit_Integratro_AWR


%% f) Zustandsregler mit Beobachter mit Eigenwertplatzierung auf [-2,-6]
% RK-Eigenwerte bei -0.68+-1i also Faktor 3 und 9 f�r Realteil 
L = acker(A',C',[-2 -6])'

% Zustandsraummodell f�r Regler mit Beobachter
% (Realisierung siehe Folie 24 "Beobachter-Entwurf")
sys_RegBeob = ss(A-L*C-(B-L*D)*K, [(B-L*D)*F,L], -K, [F,0]);      

% Zustandsraummodell f�r Regelkreis mit Regler mit Beobachter 
% (Realisierung siehe Folie 25 "Beobachter-Entwurf")
sys_Durchgriff = ss(0,0,0,1);
sys_Erweitert = append(sys_Durchgriff,sys);
feedin=2;
feedout=[1 2];
Cloop=feedback(sys_Erweitert,sys_RegBeob,feedin,feedout,+1);

% Simulation des F�hrungsverhaltens des Regelkreises mit Beobachter
yd=ones(141,1);
[y,t,x]=lsim(Cloop,[yd, zeros(141,1);],t, [0 0 0 1 1]);

figure; %Zeitverlaufsbild
plot(t,x(:,2),t,x(:,3),'linewidth',2);
xlabel('Zeit in s');
ylabel('Weg in m und Geschwindigkeit in m/s');
figure; %Stellverlauf
plot(t,-x(:,4:5)*K'+F*yd,'r','linewidth',2);
xlabel('Zeit in s');
ylabel('Kraft (u) in N');


%% g) Zur�ck auf Los (ideale Streckenkenntnis) - Folgeregler
%Simulation des Zustandsregelkreises (F�hrungsverhalten)
%So sieht der Zustandsregler f�r yd=sin(t) aus
sys_RK=ss(A-B*K,B*F,C-D*K,D*F);
yd=sin(t);
[y,t,x]=lsim(sys_RK,yd,t,x0);

figure; %Zeitverlaufsbild
plot(t,x(:,1),t,yd,'linewidth',2);
xlabel('Zeit in s');
ylabel('Soll- und Istverlauf in m');
figure;
plot(t,-x*K'+F*yd,'linewidth',2);
xlabel('Zeit in s');
ylabel('Kraft in N');
%ein wahrlich schlechtes Ergebnis
%aus dem Stellverlauf lesen wir eine Amplitude von 0.1 ab
%Das ist auch richtig, denn die Streckenvorsteuerung ist $ud=0.1*cos(t), da
%G(s)=1/(s^2+0.1s+1) ist, also [sin(t)]''+0.1[sin(t)]'+sin(t)=ud

%% h) Hilfloser Versuch das Ergebnis durch anderen Entwurf zu verbessern
K_acker=acker(A,B,[-15, -15]);
F_acker=inv((C-D*K_acker)*inv(-A+B*K_acker)*B+D);
sys_RK=ss(A-B*K_acker,B*F_acker,C-D*K_acker,D*F_acker);
yd=sin(t);
[y,t,x]=lsim(sys_RK,yd,t,x0);

figure; %Zeitverlaufsbild
plot(t,x(:,1),t,yd,'linewidth',2);
xlabel('Zeit in s');
ylabel('Soll- und Istverlauf in m');
figure;
plot(t,-K_acker*x'+F_acker*yd','linewidth',2);
xlabel('Zeit in s');
ylabel('Kraft in N');
%optisch sieht das Ergebnis deutlich besser aus
%Der Stellwertverlaufist bei t=0.1 aber eine Katastrophe!!!!


%% i) Jetzt soll der Einfluss von Messfehlern bei der Erfassung der aktuellen Zustandsgr��en x1 und x2 bei diesen hohen Verst�rkungen gezeigt werden. 
% Hierzu wird ein zus�tzlicher Eingang f�r die Zustands-Messst�rung z ben�tigt, der zwei dimensionen hat, da es zwei Zustandsgr��en gibt.
% Setzt man das um die Messst�rung erweiterte Reglergesetz u = -K*(x+z)+F*yd in die Strecke ein, erh�lt man f�r den Regelkreis: 
sys_RK=ss(A-B*K_acker,[B*F_acker,-B*K_acker],[C-D*K_acker],[D*F_acker,-D*K_acker]);

% h�here Zeitaufl�sung f�r Simulation, um h�herfrequente Rauschen nachbilden zu k�nnen
t=[0:0.01:14]';
yd=sin(t);
z=0.01*randn(length(t),2); %Die St�rung ist etwa ein Hunderstel der Zustandsamplituden (entspricht 1 Prozent Fehler)
[y,t,x]=lsim(sys_RK,[yd,z],t,x0);

figure; %Zeitverlaufsbild
plot(t,x(:,1),t,yd,'linewidth',2);
xlabel('Zeit in s');
ylabel('Soll- und Istverlauf in m');
figure;  %Stellverlaufsbild
plot(t,-K_acker*(x'+z')+F_acker*yd','linewidth',2);
xlabel('Zeit in s');
ylabel('Kraft in N');
%Der Stellverlauf ist eine Katastrophe (Stellglied nach 10 Sekunden kaputt)!!!!


%% j) 2DOF-Zustandsregler mit Regelkreisvorsteuerung
% Streckenvorsteuerung (Folie 5) nicht sinnvoll, da xd und ud separat berechnet werden m�ssten
% Regelkreisvorsteuerung (Folie 6): statisch (Festwertregelung) vd = F*yd*, 
% F�r Folgeregelung: vd aus inverser des geschlossenen Regelkreises bestimmen
sys_RK_2DOF=ss(A-B*K,B,C-D*K,D);
tf(sys_RK_2DOF) %Die Vorsteuerung ist f�r den Regelkreis (nicht die Strecke) zu entwerfen
%zum Gl�ck treten hier keine Z�hlernullstellen auf, die Regelkreisvorsteuerung vd kann 
%einfach durch Einsetzen des Sollsignals in die DGL berechnet werden
[num, den]=tfdata(sys_RK_2DOF);
% Damit direkt die Vorsteuerung berechnet werden kann, wird der Faktor vor u auf 1 normiert:
nenner=den{1}/num{1}(3);

%f�r Sollsignal yd = sin(t):
yd=sin(t);
vd=nenner(3)*sin(t)+nenner(2)*cos(t)-nenner(1)*sin(t);
x0=[0;0]; % Nullruhelage Anfangswert) nicht konsistent mit yd = sin(t)
%x0=[0;1]; % konsistenter Anfangswert f�r yd = sin(t)

%f�r Sollsignal yd = cos(t):
%yd=cos(t);
%vd=nenner(3)*cos(t)-nenner(2)*sin(t)-nenner(1)*cos(t);
%x0=[1;0]; %konsistenter Anfangswert f�r yd = cos(t)

[y,t,x]=lsim(sys_RK_2DOF,vd,t,x0);

figure; %Zeitverlaufsbild
plot(t,x(:,1),t,yd,'linewidth',2);
xlabel('Zeit in s');
ylabel('Soll- und Istverlauf in m');
figure;  %Stellverlaufsbild
plot(t,-K*x'+vd','linewidth',2);
xlabel('Zeit in s');
ylabel('Kraft in N');
%hier hatten wir Gl�ck, dass die Anfangswerte x0=[0;1] f�r den Sinus konsistent
%waren. 
% Wie sehen konsistente Anfangswerte und die Vorsteuerung f�r ein
% Sollsignal yd=cos(t) aus? --> x0=[1;0]


%% k) 2DOF-Zustandsregler, jetzt mit Modellfehler
% Vorsteuerung auf Basis des RK mit fehlerhaftem Modell 
sys_RK_2DOF_dach=ss(A_dach-B_dach*K_dach,B_dach,C_dach-D_dach*K_dach,D_dach);
tf(sys_RK_2DOF_dach) 
[num, den]=tfdata(sys_RK_2DOF_dach);
nenner=den{1}/num{1}(3);
yd=sin(t);
% Regelkreisvorsteuersignal auf Basis fehlerhaftem Modell
vd_dach=nenner(3)*sin(t)+nenner(2)*cos(t)-nenner(1)*sin(t);

% Simulation der echten Strecke mit Regler und Vorsteuerung auf Basis fehlerhaftem Modell
sys_RK_2DOF=ss(A-B*K_dach,B,C-D*K_dach,D);
x0=[0;1];
[y,t,x]=lsim(sys_RK_2DOF,vd_dach,t,x0);

figure; %Zeitverlaufsbild
plot(t,x(:,1),t,yd,'linewidth',2);
xlabel('Zeit in s');
ylabel('Soll- und Istverlauf in m');


%% l) 2DOF-Zustandsregler, jetzt mit Modellfehler plus St�rung am Ausgang nach 3 Sekunden

sys_RK_2DOF=ss(A-B*K_dach,[B zeros(2,1)],C-D*K_dach,[D 1]); %�ber die 1 im Durchgriff wirkt die St�rung auf y
% Standard-Regelkreis gem�� Folie 9 mit zus�tzlicher Ausgangsst�rung za als zus�tzlicher
% Eingang u2 = za. Die Ausgangsst�rung muss nur additiv in der Ausgangsgleichung
% durch die zus�tzliche 1 in [D 1] ber�cksichtigt werden. In der
% Systemgleichung muss nur die Dimension von B wegen des zus�tzlichen Eingangs f�r die Ausgangsst�rung erweitert werden. 
% Das aber durch eine Nullspalte, da die Ausgangsst�rung ja nicht auf den Zustand wirkt. 

% Vorsteuerung wieder auf Basis des RK mit fehlerhaftem Modell 
sys_RK_2DOF_dach=ss(A_dach-B_dach*K_dach,B_dach,C_dach-D_dach*K_dach,D_dach);
tf(sys_RK_2DOF_dach) 
[num, den]=tfdata(sys_RK_2DOF_dach);
nenner=den{1}/num{1}(3);
yd=sin(t);
vd=nenner(3)*sin(t)+nenner(2)*cos(t)-nenner(1)*sin(t);

% Simulation wieder mit echter Strecke (fehlerfrei)
x0=[0;1];
% Ausgangsst�rung geht nach 3 Sekunden von 0 auf den Wert 1
z=[zeros(301,1); ones(1100,1)];
[y,t,x]=lsim(sys_RK_2DOF,[vd,z],t,x0);

figure; %Zeitverlaufsbild
plot(t,y,t,yd,'linewidth',2);
xlabel('Zeit in s');
ylabel('Soll- und Istverlauf in m');
%der Zustandsregler sieht die Ausgangsst�rung nicht und kann sie daher
%nicht bek�mpfen, auch wenn er sie sehen w�rde, ist er nur ein P-Regler und
%w�rde i.Allg. keine station�re Genauigkeit erreichen 


%% m) Um Ausgangsst�rung und Modellfehler kompensieren zu k�nnen, wird eine Ausgangsr�ckf�hrung mit PI-Regler ben�tigt
%PI-Reglerentwurf wieder mit fehlerbehaftetem Modell (exakt wir oben)
K_pireg = lqr([A_dach zeros(2,1);C_dach 0],[B_dach; D_dach],blkdiag(Q,1),R)
K_x = K_pireg(1:2); 
K_xi = K_pireg(3);

% Regelkreisvorsteuerung entwerfen (exakt wie oben)
sys_RK_vorst_dach=ss(A_dach-B_dach*K_x,B_dach,C_dach-D_dach*K_x,D_dach);
tf(sys_RK_vorst_dach) 
[num, den]=tfdata(sys_RK_vorst_dach);
nenner=den{1}/num{1}(3);
%f�r Sollsignal yd = sin(t):
yd=sin(t);
vd=nenner(3)*sin(t)+nenner(2)*cos(t)-nenner(1)*sin(t);

% System f�r Simulation aus Original-Strecke und eben entworfenem PI-Regler mit den Eing�ngen [vd; yd; z]
% (Herleitung siehe Musterl�sung)
sys_RKPI=ss([A-B*K_x -B*K_xi; C-D*K_x -D*K_xi],[[B, zeros(2,1); D -1] [zeros(2,1);1]],[C-D*K_x -D*K_xi], [D 0 1]);

%Simulation System mit Vorsteuerung 
%(sowohl Regler als auch Vorsteuerung mit fehlerbehaftetem Modell entworfen)
x0=[0;1];
%Ohne St�rung -> Modellfehler f�hren zu Abweichung, da I-Regler f�r Sinus nicht optimal
z = zeros(1401,1);
%Mit St�rung wie oben -> St�rung wird durch Ausgangsr�ckf�hrung mit
%I-Anteil weggeregelt (hierf�r nachfolgende Zeile aktivieren)
z=[zeros(301,1); ones(1100,1)];
[y,t,x]=lsim(sys_RKPI,[vd,yd,z],t,[x0;0]); %Integratoranfangswert auf null
figure; %Zeitverlaufsbild
plot(t,y,t,yd,'linewidth',2);
xlabel('Zeit in s');
ylabel('Weg in m und Geschwindigkeit in m/s');


%% Das Ganze nun mit St�rgr��enaufschaltung /-reduktion
% hier ist der flei�ige Studierende gefragt


