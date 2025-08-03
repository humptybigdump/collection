clear all
close all

%% Aufgabe 1

f=logspace(6,-3); %definieren eines logarithmischen Vektors 
EIS_test=zeros(length(f),3); %Vordefinieren einer aus Nullen bestehenden Matrix mit Zeilenzahl = Länge des Frequenzvektors und Spaltenzahl = 3;
EIS_test(:,1)=f; %befüllen der ersten Spalte mit dem Frequenzvektor

%% Parametrierung
R_R=5;
L=1e-7;
C=10;
R_RC=10;
tau_RC=1e-4;

%% Berechnung
%Ohmscher Widerstand:
EIS_R=EIS_test;
EIS_R(:,2)=R_R;

%Induktivität:
EIS_L=EIS_test;
EIS_L(:,3)=2.*pi.*L.*EIS_L(:,1);

%Kapazität:
EIS_C=EIS_test;
EIS_C(:,3)=-1./(2.*pi.*C.*EIS_C(:,1));

%RC-Element:
EIS_RC=EIS_test;
EIS_RC(:,2)=real(R_RC./(1+1j.*2.*pi.*tau_RC.*EIS_RC(:,1)));
EIS_RC(:,3)=imag(R_RC./(1+1j.*2.*pi.*tau_RC.*EIS_RC(:,1)));

%% Aufgabe 2
figure; %erzeugt eine neue figure
map=colormap(winter(5)); %erzeugt eine colormap für 5-Werte
m = ['.','p','d','*','o']; %erzeugt Marker
plot(EIS_R(:,2),EIS_R(:,3),[m(1)],'Color',map(1,:));
hold on
plot(EIS_L(:,2),EIS_L(:,3),[m(2)],'Color',map(2,:));

plot(EIS_C(:,2),EIS_C(:,3),[m(3)],'Color',map(3,:));

plot(EIS_RC(:,2),EIS_RC(:,3),[m(4)],'Color',map(4,:));

axis equal %gleich skaliert
set(gca,'YDir','reverse'); %Umkehren der Y-Achse
xlabel('Z` / \Omega'); %Beschriftung x-Achse
ylabel('Z`` / \Omega'); %Beschriftung y-Achse
legend('Z_R','Z_L','Z_C','Z_R_C');

%% Bonus Bode
%===============================================================================
%Bode-Plot
figure; %erzeugt eine neue figure
map=colormap(winter(5)); %erzeugt eine colormap für 5-Werte
subplot(2,1,1);
plot(EIS_R(:,1),sqrt(EIS_R(:,2).^2+EIS_R(:,3).^2),'Color',map(1,:));
hold on
plot(EIS_L(:,1),sqrt(EIS_L(:,2).^2+EIS_L(:,3).^2),'Color',map(2,:));

plot(EIS_C(:,1),sqrt(EIS_C(:,2).^2+EIS_C(:,3).^2),'Color',map(3,:));

plot(EIS_RC(:,1),sqrt(EIS_RC(:,2).^2+EIS_RC(:,3).^2),'Color',map(4,:));

set(gca,'xscale','log'); %logarithmische x-Achse
set(gca,'yscale','log'); %logarithmische y-Achse
xlabel('f / Hz'); %Beschriftung x-Achse
ylabel('|Z| / \Omega'); %Beschriftung y-Achse
legend('Z_R','Z_L','Z_C','Z_R_C');

subplot(2,1,2);
plot(EIS_R(:,1),atan(EIS_R(:,3)./EIS_R(:,2))./pi.*180,'Color',map(1,:));
hold on
plot(EIS_L(:,1),atan(EIS_L(:,3)./EIS_L(:,2))./pi.*180,'Color',map(2,:));

plot(EIS_C(:,1),atan(EIS_C(:,3)./EIS_C(:,2))./pi.*180,'Color',map(3,:));

plot(EIS_RC(:,1),atan(EIS_RC(:,3)./EIS_RC(:,2))./pi.*180,'Color',map(4,:));

set(gca,'xscale','log'); %logarithmische x-Achse
xlabel('f / Hz'); %Beschriftung x-Achse
ylabel('\Theta / °'); %Beschriftung y-Achse
legend('Z_R','Z_L','Z_C','Z_R_C');
%================================================================================
%% Aufgabe 3
%Berechnung R-C
EIS_R_C=EIS_test;
EIS_R_C(:,2:3)=EIS_R(:,2:3)+EIS_C(:,2:3);

%Berechnung L-R
EIS_L_R=EIS_test;
EIS_L_R(:,2:3)=EIS_L(:,2:3)+EIS_R(:,2:3);

%Berechnung R-RC
EIS_R_RC=EIS_test;
EIS_R_RC(:,2:3)=EIS_R(:,2:3)+EIS_RC(:,2:3);

%Berechnung L-R-RC-C
EIS_L_R_RC_C=EIS_test;
EIS_L_R_RC_C(:,2:3)=EIS_L(:,2:3)+EIS_R(:,2:3)+EIS_RC(:,2:3)+EIS_C(:,2:3);

%Plot R-C
figure; %erzeugt eine neue figure
map=colormap(winter(5)); %erzeugt eine colormap für 5-Werte
m = ['.','p','d','*','o']; %erzeugt Marker
plot(EIS_R(:,2),EIS_R(:,3),[m(1)],'Color',map(1,:));
hold on
plot(EIS_C(:,2),EIS_C(:,3),[m(3)],'Color',map(3,:));

plot(EIS_R_C(:,2),EIS_R_C(:,3),[m(5)],'Color',map(5,:));

axis equal %gleich skaliert
set(gca,'YDir','reverse'); %Umkehren der Y-Achse
xlabel('Z` / \Omega'); %Beschriftung x-Achse
ylabel('Z`` / \Omega'); %Beschriftung y-Achse
title('R-C');
legend('Z_R','Z_C','Z_R_-_C');

%Plot L-R
figure; %erzeugt eine neue figure
map=colormap(winter(5)); %erzeugt eine colormap für 5-Werte
m = ['.','p','d','*','o']; %erzeugt Marker
plot(EIS_R(:,2),EIS_R(:,3),[m(1)],'Color',map(1,:));
hold on
plot(EIS_L(:,2),EIS_L(:,3),[m(2)],'Color',map(2,:));

plot(EIS_L_R(:,2),EIS_L_R(:,3),[m(5)],'Color',map(5,:));

axis equal %gleich skaliert
set(gca,'YDir','reverse'); %Umkehren der Y-Achse
xlabel('Z` / \Omega'); %Beschriftung x-Achse
ylabel('Z`` / \Omega'); %Beschriftung y-Achse
title('R-L');
legend('Z_R','Z_L','Z_L_-_R');

%Plot R-RC
figure; %erzeugt eine neue figure
map=colormap(winter(5)); %erzeugt eine colormap für 5-Werte
m = ['.','p','d','*','o']; %erzeugt Marker
plot(EIS_R(:,2),EIS_R(:,3),[m(1)],'Color',map(1,:));
hold on
plot(EIS_RC(:,2),EIS_RC(:,3),[m(4)],'Color',map(4,:));

plot(EIS_R_RC(:,2),EIS_R_RC(:,3),[m(5)],'Color',map(5,:));

axis equal %gleich skaliert
set(gca,'YDir','reverse'); %Umkehren der Y-Achse
xlabel('Z` / \Omega'); %Beschriftung x-Achse
ylabel('Z`` / \Omega'); %Beschriftung y-Achse
title('R-RC');
legend('Z_R','Z_R_C','Z_R_-_R_C');

%Plot L-R-RC-C
figure; %erzeugt eine neue figure
map=colormap(winter(5)); %erzeugt eine colormap für 5-Werte
m = ['.','p','d','*','o']; %erzeugt Marker
plot(EIS_R(:,2),EIS_R(:,3),[m(1)],'Color',map(1,:));
hold on
plot(EIS_L(:,2),EIS_L(:,3),[m(2)],'Color',map(2,:));

plot(EIS_C(:,2),EIS_C(:,3),[m(3)],'Color',map(3,:));

plot(EIS_RC(:,2),EIS_RC(:,3),[m(4)],'Color',map(4,:));

plot(EIS_L_R_RC_C(:,2),EIS_L_R_RC_C(:,3),[m(5)],'Color',map(5,:));

axis equal %gleich skaliert
set(gca,'YDir','reverse'); %Umkehren der Y-Achse
xlabel('Z` / \Omega'); %Beschriftung x-Achse
ylabel('Z`` / \Omega'); %Beschriftung y-Achse
title('L-R-RC-C');
legend('Z_R','Z_L','Z_C','Z_R_C','Z_L_-_R_-_R_C_-_C');