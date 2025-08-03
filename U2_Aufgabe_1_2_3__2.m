clear all
close all



%% Aufgabe 1

%% Nützliche Befehle
% logspace - erzeugt einen logarithmischen Vektor
% zeros - erzeugt eine Matrix mit allen Einträgen = 0
% length - gibt die länge eines Vektors aus
% help collon - ruft die Hilfe zum "collon" Befehl auf. Der ":"- Operator
% ist wichtig zur Spalten-/Zeilenweisen Abfrage und Zuweisung von Matrizen
% die []- Einträge sind entsprechend mit Inhalt zu füllen


f= [];%%definieren eines logarithmischen Vektors 
EIS_test= []; %Vordefinieren einer aus Nullen bestehenden Matrix mit Zeilenzahl = Länge des Frequenzvektors und Spaltenzahl = 3;
EIS_test([])= [];%befüllen der ersten Spalte von EIS_test mit dem Frequenzvektor f

%% Parametrierung
R_R=5;
L=1e-7;
C=10;
R_RC=10;
tau_RC=1e-4;

%% Berechnung
%Ohmscher Widerstand:
EIS_R=EIS_test; % Initialisieren der Impedanz des ohmschen Widerstandes
EIS_R(:,2)=R_R; % Zuweisen des Realteils der Impedanz

%Induktivität:
EIS_L=[];


%Kapazität:
EIS_C=[];


%RC-Element:
EIS_RC=[];

%% Aufgabe 2
figure; %erzeugt eine neue figure
map=colormap(winter(5)); %erzeugt eine colormap für 5-Werte
m = ['.','p','d','*','o']; %erzeugt Marker
plot(EIS_R(:,2),EIS_R(:,3),[m(1)],'Color',map(1,:));
hold on
plot([]); %...

axis equal %gleich skaliert
set(gca,'YDir','reverse'); %Umkehren der Y-Achse
xlabel('Z` / \Omega'); %Beschriftung x-Achse
ylabel('Z`` / \Omega'); %Beschriftung y-Achse
legend('Z_R','Z_L','Z_C','Z_R_C'); %Erzeugen der Legende

%% Bonus Bode
%===============================================================================
%Bode-Plot
% Hier können Sie im wesentlichen die gleiche Skriptstruktur nutzen, wie
% bei der Erstellung des Nyquist-Plots
figure; %erzeugt eine neue figure
map=colormap(winter(5)); %erzeugt eine colormap für 5-Werte
subplot(2,1,1); % Erstellt ein geteiltes Ausgabefenster (2x1) und plottet in den ersten Abschnitt dieses Fensters
plot([]);
%...
subplot(2,1,2); % Plottet in den zweiten Abschnitt des geteilten Ausgabefensters
plot([]);


%================================================================================
%% Aufgabe 3
%Berechnung R-C
EIS_R_C=EIS_test;
EIS_R_C(:,2:3)=EIS_R(:,2:3)+EIS_C(:,2:3);

%Berechnung L-R
EIS_L_R=[];


%Berechnung R-RC
EIS_R_RC=[];


%Berechnung L-R-RC-C
EIS_L_R_RC_C=[];


%Plot R-C
figure; %erzeugt eine neue figure
%...

%Plot L-R
figure; %erzeugt eine neue figure
%...

%Plot R-RC
figure; %erzeugt eine neue figure
%...

%Plot L-R-RC-C
figure; %erzeugt eine neue figure
%...