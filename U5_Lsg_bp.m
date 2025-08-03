% BMML Übung 5 Lösungsskript (WS18/19)
clear all
close all
%% a)
% Das gegebene Modell der reelen Elektrode ist mit Elektrischen Elementen
% dargestellt. Zu analytischen Zwecken muss das Modell mathematisch
% dargestellt werden. Dies ermoeglicht Berechnungen, Simulationen und 
% Approximationen.
% Ein Weg das Verhalten eines beliebigen Modells zu untersuchen ist im 
% Impedanzspektrum. Die Berechnung des Impedanzspektrums wird, anhand von 
% den in der Aufgabe gegebenen Werte, durchgefuehrt und das Ergebnis im
% Nyquist Diagramm dargestellt.
% Dazu muss vorerst ein Frequenzbereich gewaehlt werden, der spaeter weiter
% angepasst werden kann.

omega = logspace(-3,4,100); % Definition Frequenzbereich
R_elektrolyt = 3;
R_ct = 5;
C_dl = 3e-3;
Z_diff = 10*tanh(sqrt(1i*omega*1e3))./sqrt(1i*omega*1e3);
C_diff = 50;

Z = R_elektrolyt+R_ct./(1+1i*omega*R_ct*C_dl)+Z_diff+1./(1i*omega*C_diff); % 1i = 0 + 1
figure(1)
    plot(Z,'b');
    title('Nyquist-Diagramm des Impedanzspektrums');
    set(gca,'ydir','reverse'); % Es wird kapazitives Verhalten erwartet
    xlabel('Re(Z)');
    ylabel('Im(Z)');
    axis equal;
    grid on;

keyboard      
%% b)
% Die Darstellung eines Warburgelemts (das fraktionale 
% Diffusionsimpedanzelement) im Zeitbereich ist nicht moeglich.
% D.h. wir muessen diesen Abschnitt bestmoeglich mit zeitl. einsetzbaren 
% Komponenten approximieren. Das kann beliebig genau gemacht werden.
% In diesem Fall konzentrieren wird uns auf einen in Reihegeschalteten 
% Ohmschen Widerstand und ein RC-Glied.
% Zuerst muss das Impedanzmodell angepasst werden.
% Die Berechnung erfolgt analog zur a).

R_approx = 8.4; % 8.4
C_approx = 5e1; % 5e1
R_ohm_approx = 1.6; % 1.6
omega = logspace(-5,4,100); % Definition Frequenzbereich

Z_diff = 10*tanh(sqrt(1i*omega*1e3))./sqrt(1i*omega*1e3);
Z_approx = R_ohm_approx+R_approx./(1+1i*omega*R_approx*C_approx);

figure(2)
    semilogx(omega,imag(Z_diff),'g');
    xlabel('omega');
    ylabel('Im(Z)');
    grid on;
keyboard

figure(3)
    plot(Z_diff,'b');
    xlabel('Re(Z)');
    ylabel('Im(Z)');
    set(gca,'ydir','reverse');
    axis equal;
    grid on;
    hold on;
keyboard
    plot(Z_approx,'r:','linewidth',2.5);
    legend('Original','Approximation');
    title('Nyquist-Diagramm der Impedanzspektren');
keyboard

figure(4)
    semilogx(omega,imag(Z_diff),'g');
    hold on;
    semilogx(omega,imag(Z_approx),'r:','linewidth',2.5);
    xlabel('omega');
    ylabel('Im(Z)');
    grid on;
keyboard


%% c)
% Eine diskrete Uebertragungsfunktion kann in Matlab mit dem Befehl
% tf(Zaehler,Nenner) aufgestellt werden.
% Der Stromimpuls wird in einem neuen Vektor manuell erzeugt und simuliert.
% Der Befehl "lsim" uebernimmt dabei die Auswertung der diskreten
% Uebertragungsfunktion auf den simulierten Stromimpuls.

Ts = 1e-3; % 1ms
T_puls = 200; % [Sekunden]
t = 0:Ts:1000; % [Sekunden]
I = zeros(1,length(t));
I(t>0 & t<=T_puls) = 1e-3;

figure(5)
	plot(t,I,'r');
	title('Simulierter Stromimpuls');
	ylabel('I / A');
	grid on;

keyboard        

G = R_elektrolyt+tf([R_ct],[R_ct*C_dl 1])+R_ohm_approx+tf([R_approx],[R_approx*C_approx 1])+tf([1],[C_diff 0]);

U = lsim(G,I,t);

figure(6)
    subplot(2,1,1);
        plot(t,I,'r');
        title('Simulierter Stromimpuls');
        ylabel('I / A');
        grid on;
    subplot(2,1,2);
        plot(t,U,'b');
        title('Spannungsantwort auf den Stromimpuls');
        grid on;
        xlabel('t / s');
        ylabel('U / V');

keyboard        
%% d)
% Die Messdaten muessen eingelesen werden und die richtigen Datenreihen
% ausgewaehlt werden. Im 2. Schritt wird die SOC auf eine Skala von 0..1 
% normiert.
% (Lade/Entladekurven von Batterien werden oft ueber SOC angegeben)

load lmo.mat;

figure(7)
    plot(Q,OCV,'b');
    title('Reale OCV ueber die Ladung');
    xlabel('Q / As');
    ylabel('U / V');
    grid on;

keyboard     

SOC = 1-Q/Q(end);

figure(8)
    plot(SOC,OCV,'b');
    title('Reale OCV ueber den Ladezustand');
    xlabel('SOC');
    ylabel('U / V');
    grid on;
    
keyboard 
%% e)
% Gegeben ist ein realer OCV Verlauf einee Elektrode ueber die Ladung Q.
% Dieser soll mit einem simuliertem Strom entladen werden.
% Der Strom hat den konstanter Wert bei dem SOC = 1, also der Wert beim
% vollgeladenem Zustand des Vektors Q.
% 
% Da die differentielle Kapazitaet nur fuer EINEN Arbeitspunkt gegeben ist,
% muss diese fuer jeden Ladungszustand (ueber Q oder t) neu berechnet werden. 
% Dies entspricht der Ableitung der Spannung ueber Q an jedem Punkt.
% Dazu muss das Modell ohne die gegebene differentielle Kapazitaet neu 
% berechnet werden, da diese durch die Ableitung ersetzt werden kann.
% Die Ladung und Spannung bei der Simulierten Entladung werden durch 
% Integration und Interpolation angepasst und der
% Spannungsverlauf abzueglich der Dynamik (innere Verluste) berechnet.

t = 0:Ts:1800; % Simulation ueber 30min.
I = ones(length(t),1) *Q(end)/3600; % [Q]=As. Batteriekennzeichnung typisch in Amperstunde.

% Modell ohne differentieller Kapazitaet
G_ohne_diff_Kap = R_elektrolyt+tf([R_ct],[R_ct*C_dl 1])+R_ohm_approx+tf([R_approx],[R_approx*C_approx 1])
U_entladung_dynamik = lsim(G_ohne_diff_Kap,I,t);

figure(9)
    subplot(2,1,1);
        plot(t,I,'r');
        title('U Entladung');
        ylabel('I / A');
        grid on;
    subplot(2,1,2);
        plot(t, U_entladung_dynamik,'k')
        grid on;
        xlabel('t / s');
        ylabel('U / V');
        legend('U Entladung');

keyboard      

% OCV der Entladung in 30min.
Q_entladung = cumtrapz(t,I); % Aufintegrierung vom Strom mit Bezug auf den Zeitvektor mittels Trapezregel
OCV_entladung = interp1(Q,OCV,Q_entladung); 

U_sim = OCV_entladung-U_entladung_dynamik;

figure(10)
    subplot(2,1,1);
        plot(t,I,'r');
        title('Vergleich der OCV und der Spannung bei simulierter Entladung');
        ylabel('I / A');
        grid on;
    subplot(2,1,2);
        plot(t, OCV_entladung,'k')
        hold on;
        plot(t,U_sim,'b')
        grid on;
        xlabel('t / s');
        ylabel('U / V');
        legend('OCV Entladung','Simulierte Entladung');
keyboard 
figure(11)
    plot(Q,OCV,'k');
    title('Vergleich der OCV und der Spannung bei simulierter Entladung');
    hold on;
    plot(Q_entladung,U_sim,'b');
    xlabel('Q / As');
    ylabel('U / V');
    grid on;
    legend('OCV der Entladung','Entladung ueber die Elektrode');
    