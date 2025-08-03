% Beispiel Penicilin-Produktion

clear all; 
close all;

% 0. Parameter
para.mu_max = 0.12;
para.Ks     = 50;
para.V      = 150;
para.p1     = 0.00047;
para.p2     = 200000;
para.mS     = 23;  

%%% 1. Trophophase

%% 1.1 Linearisierung um Arbeitspunkt
syms mu_max Ks V p1 mS p2 p3 p4 u1
x        = sym('x',[2,1]); % Zustandsvektor
y        = sym('y',[2,1]); % Ausgangsvektor
mu       = mu_max*x(2)/(Ks*V+x(2));
f_tropho = [mu*x(1);-1/p1*mu*x(1)-mS*x(1)+p2*u1];
y(1)     = x(1);
y(2)     = mu/p3*x(1)+p4*mS*x(1);

% Arbeitspunkt
xR = solve(f_tropho==[0;0],x)
syms u1R
xR = subs(xR,u1,u1R);
xR = [xR.x1;xR.x2];

% Systemmatrizen
A = jacobian(f_tropho,x);
b = jacobian(f_tropho,u1);
c = jacobian(y(1),x); % Messung y(1)
C = jacobian(y,x);    % Messung [y(1),y(2)]

A = subs(A,x,xR); %an sich auf mit u=uR
b = subs(b,x,xR);
c = subs(c,x,xR);
C = subs(C,x,xR);

%% 1.2 Systemanalyse (symbolisch)

% Steuerbarkeit
S = [b,A*b];
rank(S)
simplify(det(S)) %-> u1R~=0 

% Beobachtbarkeit
% Fall 1 
O1 = [c;c*A]
det(O1)
% Fall 2
O2 = [C;C*A]
rank(O2) % Rang mit Vorsicht anzuwenden

%% 1.3 Systemanalyse (numerisch)
para.u1R = 1e-3; % Definition des Arbeitspunkts, muss physikalisch sinnvoll sein 
para_save = para; 
AN  = double(subs(A,para));
bN  = double(subs(b,para));
cN  = double(subs(c,para));
xRN = double(subs(xR,para));

% Stabilität
evN = eig(AN)
figure(1);
plot(real(evN),imag(evN),'x');
xlabel('Re'); ylabel('Im');
grid on; 

% Steuerbarkeit
SN = ctrb(AN,bN)
rank(SN) % 'rank' nur bei Matrizen mit niedriger Dimension verwenden
null(SN) % ermittelt Nullraum, also alle Vektoren v so, dass M*v = 0
det(SN)

% Beobachtbarkeit
ON = obsv(AN,cN)
rank(ON)
null(ON)
det(ON)

%% 1.4 Simulation des linearisierten Systems

dN = 0.0; % kein direkter Durchgriff von Eingang zu Ausgang
          %lsys_tropho = ss(AN,bN,cN,dN);
lsys_tropho = ss(AN,bN,eye(size(AN)),dN);
t1  = 0:0.05:150.0;
% Eingang und Anfangswert für NICHTLINEARES System
inp1 = @(t,x,para)para.u1R + 10*para.u1R*stepfun(t,10) - 5*para.u1R*stepfun(t,50);
u1   = inp1(t1,[],para); 
x01  = xRN; % AB gleich Ruhelage
% Eingang und Anfangswert für LINEARISIERTES System
delta_u1  = u1 - para.u1R;
delta_x01 = x01 - xRN;  
%
% Numerische Lösung
y1  = lsim(lsys_tropho,delta_u1,t1,delta_x01); 

%% 1.5 Simulation des nichtlinearen Systems

[t,x1] = ode45(@(t,x)ex1_ws2425_tropho_rhs(t,x,para,inp1),t1,x01);
%h = @(para,inp1)ode45(@(t,x)ex1_ws2425_tropho_rhs(t,x,para,inp1),t1,x01);
%para2=para;
%para2.V=2*para.V;
%g=h(para2,inp1);

figure(2);
subplot(2,1,1); 
plot(t1,y1(:,1),t1,y1(:,1)+xRN(1),t,x1(:,1));
xlabel('t');
legend('\Delta x_1','x_1^{lin}','x_1^{nlin}'); 
grid on; 
subplot(2,1,2);
plot(t1,y1(:,2),t1,y1(:,2)+xRN(2),t,x1(:,2));
xlabel('t');
legend('\Delta x_2','x_2^{lin}','x_2^{nlin}');  
grid on; 

%% 1.6 Regelungsentwurfs mittels Ackermann

eigDesired = eig(AN) - 2*abs(eig(AN));
kT = place(AN,bN,eigDesired);
eig(AN-bN*kT)-eigDesired

% Simulation des geregelten nichtlinearen Systems
para.xRN = xRN; 
acker1   = @(t,x,para)para.u1R-kT*(x-para.xRN); 

x01    = 0.1*xRN+[0;0]; %[5*xRN(1);100]
[t,x1] = ode45(@(t,x)ex1_ws2425_tropho_rhs(t,x,para,acker1),t1,x01);
u1     = acker1(t,x1',para); 
%
figure(3)
subplot(3,1,1); 
plot(t,x1(:,1));
xlabel('t');
ylabel('x_1^{nlin}'); 
grid on; 
subplot(3,1,2);
plot(t,x1(:,2));
xlabel('t');
ylabel('x_2^{nlin}');  
grid on;
subplot(3,1,3);
plot(t,u1);
xlabel('t');
ylabel('u');  
grid on;

%% 1.7 Regelungsentwurfs mittels Ackermann mit Vorfilter
%  Ziel y -> r

% Ackermann
eigDesired = eig(AN) - 4*abs(eig(AN));
kT = place(AN,bN,eigDesired);
eig(AN-bN*kT)-eigDesired

% Vorfilter
hv = 1.0/(-cN*inv(AN-bN*kT)*bN);
para.hv = hv;

% Referenztrajektorie
para.yRN = cN*xRN; 
ref = @(t)para.yRN + 2*para.yRN*stepfun(t,75);
para.ref = ref;

% Simulation des geregelten nichtlinearen Systems
para.xRN = xRN;
acker1   = @(t,x,para)para.u1R-kT*(x-para.xRN)+para.hv*(para.ref(t)-para.yRN); 

x01    = 0.1*xRN+[0;0]; %[5*xRN(1);100]
[t,x1] = ode45(@(t,x)ex1_ws2425_tropho_rhs(t,x,para,acker1),t1,x01);
for j=1:length(t)
    u1(j) = acker1(t(j),x1(j,:)',para);
end
%
figure(4)
subplot(3,1,1); 
plot(t,x1(:,1),t,ref(t));
xlabel('t');
ylabel('x_1^{nlin}'); 
grid on; 
subplot(3,1,2);
plot(t,x1(:,2));
xlabel('t');
ylabel('x_2^{nlin}');  
grid on;
subplot(3,1,3);
plot(t,u1);
xlabel('t');
ylabel('u');  
grid on;

%% 1.8 Beobachterbasierte Zustandsregelung mittels Ackermann inklusive Vorfilter

% Ackermann Zustandsregler
eigDesired = [-0.1,-0.4]; %eig(AN) - 4*abs(eig(AN));
kT = place(AN,bN,eigDesired);

% Ackermann Beobachter
eigBeobDesired = [-1,-1.2]; %eig(AN) - 8*abs(eig(AN));
L = place(AN',cN',eigBeobDesired)';

% Beobachter-Anteile
para.A  = AN;
para.b  = bN;
para.cT = cN;
para.L  = L;

% Vorfilter
hv = 1.0/(-cN*inv(AN-bN*kT)*bN);
para.hv = hv;

% Referenztrajektorie
para.yRN = cN*xRN; 
ref      = @(t)para.yRN + 2*para.yRN*stepfun(t,75);
para.ref = ref;

% Simulation des geregelten nichtlinearen Systems mit Beobachter für das linearisierte System
para.xRN = xRN; 
acker1   = @(t,x,para)para.u1R-kT*(x-para.xRN)+para.hv*(para.ref(t)-para.yRN); 

x01    = 0.1*xRN+[0;0]; % Anfangsbedingung der Regelstrecke
x02    = 2.0*xRN-xRN;   % Anfangsbedingung des linearen Beobachters (als \Delta\hat{x} = \hat{x}-xR anzugeben)
x0     = [x01;x02]; 
[t,x]  = ode45(@(t,x)ex1_ws2425_tropho_beob_rhs(t,x,para,acker1),t1,x0);
xsys   = x(:,1:2);
xbeo   = x(:,3:4)+para.xRN' ;
for j=1:length(t)
    u1(j) = acker1(t(j),xbeo(j,:)',para);
end
%
figure(5)
subplot(3,1,1); 
plot(t,xsys(:,1),t,xbeo(:,1),t,ref(t));
xlabel('t');
ylabel('x_1');
legend('$x_1^{nlin}$','$\Delta \hat{x}_1+x_{1,R}$','r','Interpreter','latex')
grid on; 
subplot(3,1,2);
plot(t,xsys(:,2),t,xbeo(:,2));
xlabel('t');
ylabel('x_2');
legend('$x_2^{nlin}$','$\Delta \hat{x}_2+x_{2,R}$','Interpreter','latex')
grid on;
subplot(3,1,3);
plot(t,u1);
xlabel('t');
ylabel('u');  
grid on;

%% 1.9 Folgeregelungsentwurf mittels Regelungsnormalform

wT = [0,1]*inv(S); % erfordert Berechnung von inv(S)
wT = [0,1]/S; %alternativ ohne Berechnung von inv(S)

pRNF = sym('pRNF',[2,1]); %Koeffizienten eines Hurwitz-Polynoms
kT_RNF = wT*(pRNF(1)*eye(2) + pRNF(2)*A  + A*A);

syms tz
zeta_star   = matlabFunction((10+5*sin(pi/40*tz))/100);
dzeta_star  = matlabFunction(diff(zeta_star(tz),tz)); 
ddzeta_star = matlabFunction(diff(dzeta_star(tz),tz));

% Symbolischer Folgeregler
x    = sym('x',[2,1]);
uRNF = simplify(-kT_RNF*x + pRNF'*[zeta_star(tz);dzeta_star(tz)] + ddzeta_star(tz));

% Numerische Auswertung und Simulation
eigDesired = [-0.4,-0.8];
pRNFN = fliplr(poly(eigDesired));
qara  = para_save; 
wTN   = double(subs(wT,para_save));
qara.pRNF1 = pRNFN(1);
qara.pRNF2 = pRNFN(2); 
kT_RNFN = double(subs(kT_RNF,qara)); 
p_RNFN  = pRNFN(1:2); % extrahiere p0 und p1

trackRNF = @(t,x,para)double(para.u1R-kT_RNFN*(x-para.xRN)+pRNFN*[zeta_star(t);dzeta_star(t);ddzeta_star(t)]); 
x01    = 0.1*xRN+[0;0]; %[5*xRN(1);100]
t1     = 0:0.05:300.0;
[t,x1] = ode45(@(t,x)ex1_ws2425_tropho_rhs(t,x,para,trackRNF),t1,x01);
u1     = trackRNF(t',x1',para); 

figure(6)
subplot(2,2,1); 
plot(t,x1(:,1));
xlabel('t');
ylabel('x_1^{nlin}'); 
grid on; 
subplot(2,2,2);
plot(t,x1(:,2));
xlabel('t');
ylabel('x_2^{nlin}');  
grid on;
subplot(2,2,3);
plot(t,u1);
xlabel('t');
ylabel('u');  
grid on;
subplot(2,2,4);
plot(t,wTN*(x1'-xRN),t,zeta_star(t));
xlabel('t');
ylabel('wT*x');  
grid on;

%% 1.10 Ausgangsregelung über Eingangs-Ausgangs-Normalform

% Mit y(1) entspricht der Entwurf dem von 1.9

c_mu = subs(simplify(jacobian(y(2),x)),x,xR); % Messung y(2) = mu
y_mu = c_mu*x;

% Bestimmung des relativen Grads
syms u
dy_mu = simplify(c_mu*A*x + c_mu*b*u);

simplify(c_mu*b) % von Null verschieden, somit ist r=1

% Für r=1 folgt der Ausgangsregler 
pEA   = sym('pEA',[1,1]); %Koeffizienten eines Hurwitz-Polynoms
kT_EA = c_mu*(pEA(1)*eye(2) + A)/(c_mu*b);
eigDesired = [-0.4];
pEAN  = fliplr(poly(eigDesired));
c_muN = double(subs(c_mu,para_save));
qara  = para_save;
qara.pEA1 = pEAN(1);
kT_EAN = double(subs(kT_EA,qara)); 

% Ab hier anpassen ...
trackRNF = @(t,x,para)double(para.u1R-kT_RNFN*(x-para.xRN)+pRNFN*[zeta_star(t);dzeta_star(t);ddzeta_star(t)]); 
x01    = 0.1*xRN+[0;0]; %[5*xRN(1);100]
t1     = 0:0.05:300.0;
[t,x1] = ode45(@(t,x)ex1_ws2425_tropho_rhs(t,x,para,trackRNF),t1,x01);
u1     = trackRNF(t',x1',para); 
