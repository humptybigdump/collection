clearvars;


%Daten zur Wärmeleitfähigkeit und Wärmeübertragungskoeffizienten
C=5000; %W/m
lambdaI=0.1; %W/(m K)
lambdaG=0.8; %W/(m K)
alphai=25; %W/(m^2 K)
alphaa=2;  %W/(m^2 K)


% Wärmeströme
Q1=-3129; %W
Q2=366.93; %W

%Abmessungen
L=1; %m
ri=0.1; %m
rS=ri+0.05; %m
rH=rS+0.01; %m
rI=rH+0.05; %m
rG=rI+0.02; %m

% Temperaturen
T_FL=363.15; %K
T_ri=-Q1/(pi*2*ri*L*alphai)+T_FL; % %K
T_rS=T_ri*(rS/ri)^(-Q1/(2*pi*L*C)); %K
T_rH=T_rS; %K
T_rI= T_rH+log(rH/rI)*(Q2/(2*pi*L*lambdaI)); %K;
T_rG= T_rI+log(rI/rG)*(Q2/(2*pi*L*lambdaG)); %K;
T_u=293.15; %K



%Darstellung des quantitativen Temperaturverlaufs
figure
hold on;
title('Quantitativer Verlauf der Temperatur')
xlabel('r / m') 
ylabel('T / K') 

%ACHTUNG; KONVEKTIVER VERLAUF NUR QUALITATIV
r01=0:0.01:ri/2;
T_reaktor1=T_FL + 0*r01;
plot(r01, T_reaktor1, '--k')
r02=ri/2:0.01:ri;
xxx=(T_ri-T_FL).^(1/(ri/2));
T_reaktor2=T_FL + xxx.^((r02-ri/2));
plot(r02, T_reaktor2, '--k')

%Temperaturverlauf durch den Stahlmantel
r1=ri:0.01:rS;
T_stahl=T_ri*(r1/ri).^(-Q1/(2*pi*L*C));
plot(r1, T_stahl, 'k')

%Temperatur der Heizung
r2=rS:0.01:rH;
T_Heiz=T_rS+0*r2;
plot(r2, T_Heiz, 'k')

%Temperaturverlauf in der Isolierung
r3=rH:0.01:rI;
T_iso=T_rH+(log(r3/rH))*(-Q2/(2*pi*L*lambdaI));
plot(r3, T_iso, 'k')

%Temperaturverlauf durch das Hartgummi
r4=rI:0.01:rG;
T_gummi=T_rI+(log(r4/rI))*(-Q2/(2*pi*L*lambdaG));
plot(r4, T_gummi, 'k')

%ACHTUNG; KONVEKTIVER VERLAUF NUR QUALITATIV
r51=rG:0.01:rG+0.05;
yyy=(T_rG-T_u).^(1/(0.05));
T_umgebung1=T_u + (yyy.^(rG+0.05-r51));
plot(r51, T_umgebung1, '--k')
r52=rG+0.05:0.01:rG+0.15;
T_umgebung2=T_u + 0*r52;
plot(r52, T_umgebung2, '--k')


%Abbildung zur Illustration der Krümmungen in den einzelnen Schichten
%Es ist zu sehen, dass der Gradient der Temperatur für größer werdende
%Radien abnimmt
figure
hold on;
title('Temperaturverläufe der einzelnen Schichten über größere Radien ')
xlabel('r / m') 
ylabel('T / K') 

txt = {'Es ist zu sehen, dass der Gradient ','der Temperatur für größer werdende ','Radien abnimmt. Dies liegt am ','Logarithmus für Isolierung und ','Hartgummi bzw. dem Term ','r_1/r_2 im Stahlmantel'};
text(45,-1000,txt)
%Temperaturverlauf durch den Stahlmantel
r1=ri:0.01:100;
T_stahl=T_ri*(r1/ri).^(-Q1/(2*pi*L*C));
plot(r1, T_stahl, 'k')

%Temperaturverlauf in der Isolierung
r3=rH:0.01:100;
T_iso=T_rH+(log(r3/rH))*(-Q2/(2*pi*L*lambdaI));
plot(r3, T_iso, 'b')

%Temperaturverlauf durch das Hartgummi
r4=rI:0.01:100;
T_gummi=T_rI+(log(r4/rI))*(-Q2/(2*pi*L*lambdaG));
plot(r4, T_gummi, 'r')
legend('Stahlmantel','Isolierung','Hartgummi')
