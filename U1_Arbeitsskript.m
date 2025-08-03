clear all
close all
%% Input
% Lade Gleichgewichtsspannung aus Matlab-File in "OCV"
% .matfiles werden mit variable=load(filename) in den Workspace geladen
% Excel dateien können u.a. mit  mit variable=xlsread(filename) eingelesen
% werden

OCV_input=[]; 


% Lade Entladekennlinien in Workspace
C1=[]; 
C2=[];
C5=[];

OCV=[];

%% Plots Erstellen
% Plotten aller Spannungskurven
% plot(x,y) plottet y über x
% legend('string') erstellt eine Legende mit den Eintrag string
% title('string') erstellt einen Titel mit den Eintrag string
% xlabel('string') bezeichet die x-Achse mit string
% ylabel('string') bezeichet die x-Achse mit string
% subplot(x,y,z) teilst das aktive plotfenster in x Zeilen und y spalten
% und "aktiviert" das z-ste Teilfenster, sodass der nächste plot-Befehl in
% diesem Teilfenster ausgeführt wird
% saveas(figure,'Name','jpg') speichert die Figure mit dem handle "figure" als Name.jpg 

fig=figure; %erstellt ein neues Plotfenster und setzt dieses als "aktiv"
% plotten der Spannungsverläufe über der Zeit


% Normieren der Spannungsverlaeufe auf entnommene Ladungsmenge


% Plotten der Kurven in ein Fenster mit subplot


% Speichern der Ausgabe

