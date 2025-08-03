function [U_Bat] = U_Batterie( t, I, R0, R1, C1, R2, C2, Cdiff )
%%Berechnung der Batteriespannung

%Finden des Stromsprungs
index=find(I, 1, 'first');
%Berechnung von t0 (Startzeitpunkt des Stromsprungs)in s
t0=t(index,1);

%Berechnung von I0 (Höhe des Stromsprungs) in A
I0=[];

%Berechnung von tau (Zeitkonstante RC Glied) in s
tau1=R1*C1;
tau2=R2*C2;

%Berechnung der Teilspannungen R0, R1/C1, R2/C2, Cdiff für t>t0
t_ab_Sprung=[]; %Zeitpunkte zur Berechnung von U_Batterie nach dem Stromsprung
U_R0(1:1:length(t_ab_Sprung),1)=I0.*R0; 
U_R1_C1=[];
U_R2_C2=[];
U_Cdiff=[];
U_OCV1(1:1:length(t_ab_Sprung),1)=3.9; % OCV Spannung nach dem Stromsprung

%Berechnung der Batteriespannung 
U_Bat(1:1:index-1,1)=[]; % Batteriespannung vor dem Stromsprung
U_Bat(index:1:length(t),1)=[]; % Batteriespannung nach dem Stromsprung

end

