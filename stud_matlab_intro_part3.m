%STUD_MATLAB_INTRO_PART3 Einführung in Matlab/Simulink.
%
%  Description:
%          In diesem Musterfile wird hauptsächlich die Funktionalität der
%          Control System Toolbox (CST) näher erläutert. Diese findet vor
%          allem in regelungstechnischen Aufgabenstellungen Anwendung.
%          Ausgangspunkt für die Verwendung der CST ist ein mathematisches
%          Modell der betrachteteten Strecke. Es werden nur lineare
%          zeitkontinuierliche und zeitdiskrete Probleme betrachtet.
%
%  Authors: Jan Reinhold (ACON Kiel), Thomas Meurer (KIT)
%  Email: thomas.meurer@kit.edu
%  Website: https://www.mvm.kit.edu/dpe.php
%  Creation date: 01.04.2019
%  Last revision date: 31.10.2023
%  Last revision author: Thomas Meurer (KIT)
%
%  Copyright (c) 2023, DPE/MVM, KIT
%  All rights reserved.
%
%  See also stud_matlab_intro_part1, stud_matlab_intro_part2, stud_mittelwert.

clear variables
close all
clc
%%
help control  % Wichtig: Die Hilfe zur Control System Toolbox

%%

% Erzeugen von Datenobjekten in der CST
% Es wird ein einfaches PT2-Glied betrachtet (siehe Vorlesung RT I)

T = 0.5;
xi = 0.2;
V = 2;

% 1. Möglichkeit: Übertragungsfunktion

G_tf = tf([V],[T^2 2*xi*T 1])

% Zugriff auf das Datenobjekt

G_tf.num{1}     % Zähler
G_tf.den{1}     % Nenner
G_tf.Variable   % Variable

% Alternative (ist bevorzugt zu verwenden)

s = tf('s')     % Erzeugen der komplexen Variablen s
G_tf1 = V/(1+2*xi*T*s+s^2*T^2)


% 2. Möglichkeit: über Pol-Nullstellen

p1 = (-xi+sqrt(xi^2-1))/T;
p2 = (-xi-sqrt(xi^2-1))/T;
G_zp = zpk([],[p1 p2],V/T^2)

% Zugriff auf das Datenobjekt

G_zp.z{1}       % Nullstellen
G_zp.p{1}       % Polstellen
G_zp.k          % Verstärkungsfaktor
G_zp.Variable   % Variable


% 3. Möglichkeit: Zustandsraumdarstellung

A = [0 1; -1/T^2 -2*xi/T];
b = [0;V/T^2];
c = [1;0];
d = 0;

sys = ss(A,b,c',d)


% Zugriff auf das Datenobjekt

sys.a           % Dynamikmatrix
sys.b           % Eingangsmatrix
sys.c           % Ausgangsmatrix
sys.d           % Durchschaltmatrix
sys.StateName   % Name der Zustände


% 4. Möglichkeit FRD-Modelle: Numerische Eingabe von gemessenen
% Übertragungsfunktionen, wird hier nicht näher betrachet

% Umwandeln der Objekte in andere Darstellungsformen

G_ss2tf = tf(sys)
G_spk2tf = tf(G_zp)

sys_zpk2ss = ss(G_zp)
sys_tf2ss = ss(G_tf)

G_ss2zpk = zpk(sys)
G_tf2zpk = zpk(G_tf)

% Zusätzliche Möglichkeiten und Information
help ss2tf
help ss2zp
help zp2tf
help zp2ss
help tf2zp
help tf2ss

%%
% Diskrete Modelle
% Diese können prinzpiell wie vorher erzeugt werden, ledglich muss noch
% eine Abstastzeit vorgegeben werden

Ta = 0.2; % Vorgabe einer Abtastzeit

% Direktes Erzeugen

z = tf('z',Ta)
Gzt = 1/(z-0.5);    % Als Transferfunktion
sysd = ss(1,1,1,0,Ta) % Als State-Space (hier: Diskreter Integrator)

% Aus bestehendem kontinuierlichen Modell

Gz = c2d(G_tf,Ta,'zoh')    % Hier wird ein Halteglied nullter Ordnung verwendet
sys_z = c2d(sys,Ta,'zoh')

% Modelle im Tustin Bereich (q-Bereich)

Gq = d2c(Gz,'tustin')
sys_q = d2c(sys_z,'tustin')
%%

% Modelle mit Totzeiten

G_tzi = tf(1,[1 1],'inputdelay',0.5)   % Eingangsverzögerung
G_tzo = tf(1,[1 1],'outputdelay',0.5)  % Ausgangsverzögerung
G_tzio = tf(1,[1 1],'iodelay',0.5)     % Eingangs-Ausgangsverzögerung


%%

% Verknüpfung von Modellen
G2 = 1/s;

Gsum = G_tf+G2          % Summe der Modelle
Gdiff = G_tf-G2         % Differenz der Modelle
Gprod = G_tf*G2         % Hintereinanderschlatung der Modelle
Gfb = feedback(G_tf,G2) % Rückkopplung

%%

% Analyse von LTI-Modellen

dcgain(G_tf)            % Verstärkungsfaktor bestimmen
damp(G_tf)              % Natürliche Frequenzen und Dämpfungen
bandwidth(G_tf)         % Bandbreite

pole(G_tf)              % Pole der Übertragungsfunktion
zero(G_tf)              % Nullstellen der Übertragunsgfunktion

pzmap(G_tf*(s-.1))      % Pol-Nullstellendiagramm
grid on

figure
impulse(G_tf,10)        % Impulsantwort für 10 Sekunden
hold on
step(G_tf,10)           % Sprungantwort für 10 Sekunden

% Systemantwort auf beliebiges Eingangssignal

[u,t] = gensig('square',1,10);   % Erzeugung eines Testsignals
[y,t] = lsim(G_tf,u,t);          % Simulation des Systems mit dem Eingangssignal

figure
plot(t,u,t,y)
grid on

% Lösung im eingeschwungenen Zustand

H = freqresp(G_tf,2)     % Auswertung an der Stelle omega = 2

% Bodediagramm

figure
bode(G_tf)
[abs,pha,omega] = bode(G_tf);   % Auslesen der Daten aus dem Bodediagramm
[abs,pha,omega] = bode(G_tf,2); % Bodediagramm an der Stelle omega = 2

% Nyquistdiagramm

figure
nyquist(G_tf)
grid on

% Ordnungsreduktion von Systemen

minreal((s+1)/(s+1)/s)  % Durchführen von Pol-Nullstellen-Kürzungen

%%

% Regler- und Beobachterentwurf: In dieser Einführung sollen nur die
% grundlegenden Bergiffe für Regler- und Beobachterentwurf genannt werden,
% die Anwendung erfolgt dann in den späteren Kapiteln

% Analyse des offenen Regelkreises

% Kennwerte für den offenen Kreis beim Frequenzkennlinienverfahren
allmargin(G_tf/s)


A = [-1 2; 0 -2];
b = [0;1];
c = [1;0];
d = 0;

sysr = ss(A,b,c',d)

ctrb(A,b)  % Erreichbarkeitsmatrix
obsv(A,c') % Beobachtbarkeitsmatrix

gram(sysr,'c') % Gramsche Erreichbarkeitsmatrix
gram(sysr,'o') % Gramsche Beobachtbarkeitsmatrix

acker(A,b,[-10 -11]) % Polvorgabe
place(A,b,[-10 -11]) % Polvorgabe
