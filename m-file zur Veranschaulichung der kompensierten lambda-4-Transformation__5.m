% Breitbandkompensation GHF
clear all
close all
% Konstanten
c0=3e8; %m/s
ZL=50; %Ohm

% Einheiten
GHz=1e9;
MHz=1e6;
pF=1e-12;
nF=1e-9;
nH=1e-9;
myH=1e-6;

% Beipielwerte
R2=250; %Ohm
f0=1*GHz;
df=50*MHz*linspace(-1,1,100);
C=2*pF; %gewaehlt, guter Wert 4,5
L=1/((2*pi*f0)*(2*pi*f0)*C);

% abgeleitete Groessen
f=f0+df;

lambda0=c0/f0;
lambda=c0./f;

beta0=2*pi/lambda0;
beta=2*pi./lambda;

l0=1/4*lambda0; %physikalische Laenge der Leitung

% Impedanztransformation

Z2=R2.*(1+j*ZL/R2*tan(beta.*(-l0)))./(1+j*R2/ZL*tan(beta.*(-l0)));

%Sollwert
Zsoll=R2.*(1+j*ZL/R2*tan(beta0.*(-l0)))./(1+j*R2/ZL*tan(beta0.*(-l0)));


%Kompensationserienschwingkreis
ZS=j*2*pi.*f.*L+1./(j*2*pi.*f.*C);
figure 
plot(f./GHz,real(ZS),'--r','LineWidth',2); hold on
plot(f./GHz,imag(ZS),'-.r','LineWidth',2); hold on
plot(f./GHz,real(Z2),'--b','LineWidth',2); hold on
plot(f./GHz,imag(Z2),'-.b','LineWidth',2); hold on
grid on
xlabel('Frequenz in GHz')
ylabel('Real- und Imagin?rteil der Impedanz in Ohm')
% Reflexionsfaktorebene

r0=(R2-ZL)/(R2+ZL);

r2=(Z2./ZL-1)./(Z2./ZL+1);

rsoll=(Zsoll-ZL)/(Zsoll+ZL);

figure
% hold on
% GHF_SmithChart(1,0)
% hold on
plot(real(r2),imag(r2),'LineWidth',2);
hold on
plot(real(r0),imag(r0),'-.r*','MarkerSize',10);
hold on
plot(real(rsoll),imag(rsoll),'-.bo','MarkerSize',10);
hold on

%Anpasspunkt
plot(0,0,'bx','MarkerSize',10)


% Serienschaltung
Z1=Z2+ZS;

r1=(Z1./ZL-1)./(Z1./ZL+1);
plot(real(r1),imag(r1),'--g','LineWidth',2);

gcf
Xlim([-1 1])
Ylim([-1 1])
grid on
xlabel('Re(r)')
ylabel('Im(r)')

