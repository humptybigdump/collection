function [U_Bat] = U_Batterie( t, I, R0, R1, C1, R2, C2, Cdiff )
%%Berechnung der Batteriespannung

%Finden des Stromsprungs
index=find(I, 1, 'first');
%Berechnung von t0 (Startzeitpunkt des Stromsprungs)in s
t0=t(index,1);

%Berechnung von I0 (Höhe des Stromsprungs) in A
I0=I(index,1)-I((index)-1,1);

%Berechnung von tau (Zeitkonstante RC Glied) in s
tau1=R1*C1;
tau2=R2*C2;

%Berechnung der Teilspannungen R0, R1/C1, R2/C2, Cdiff für t>t0
t_ab_Sprung=t(index:1:end,1); %Zeitpunkte zur Berechnung von U_Batterie nach dem Stromsprung
U_R0(1:1:length(t_ab_Sprung),1)=I0.*R0; 
U_R1_C1=I0.*R1.*(1-exp(-(t_ab_Sprung-t0)./tau1));
U_R2_C2=I0.*R2.*(1-exp(-(t_ab_Sprung-t0)./tau2));
U_Cdiff=(I0/Cdiff).*(t_ab_Sprung-t0);
U_OCV1(1:1:length(t_ab_Sprung),1)=3.9; % OCV Spannung in V

%Berechnung der Batteriespannung 
U_Bat(1:1:index-1,1)=3.9; % OCV Spannung vor dem Stromsprung
U_Bat(index:1:length(t),1)=U_OCV1+U_R0+U_R1_C1+U_R2_C2+U_Cdiff;

end

