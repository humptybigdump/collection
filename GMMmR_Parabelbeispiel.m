%% PARABEL2 - Parabelschätzung mit Restriktion über Bedingungsgleichung
%
% H. Bähr, 8. Mai 2012
% angepasst von L. Dalheimer 20.05.2022

%%
clear variables    
close all     
format compact 


%% Daten

xi = (1:5)';

y  = [ 4.0
       1.1
       1.0
       3.7
       6.9 ];


%% Funktionales Modell

l = y;
A = [xi.^2 xi ones(5,1)];
R = [xi(3)^2 xi(3) 1];
g = y(3);


%% Stochastisches Modell

P = eye(5);        % Gewichtsmatrix = Einheitsmatrix


%% Ausgleichung im GMM

NGMM = A'*P*A;
QxxGMM = inv(NGMM);
xGMM = QxxGMM*A'*P*l;

%% Ausgleichung über geschlossene Lösung
xkGMMmR = inv([NGMM R'; R 0])*[A'*P*l;g]

%% Ausgleichung über stufenweise Lösung

Qkk = inv(R*QxxGMM*R');
k   = Qkk * (R*xGMM-g);

x   = xGMM - QxxGMM*R'*k

Qxx = QxxGMM-QxxGMM*R'*Qkk*R*QxxGMM

a = x(1);
b = x(2);
c = x(3);

%% Ergebnisse

fprintf(1,'Geschätzte Parameter der Parabel:\n');
fprintf(1,'  a = %8.4f\n  b = %8.4f\n  c = %8.4f\n',x);
fprintf(1,'Funktionswert bei x(3)=3:\n');
fprintf(1,'  f(3) = %8.4f\n',a*xi(3)^2+b*xi(3)+c);
fprintf(1,'Kofaktormatrix der Parameter:\n');
fprintf(1,'  %7.4f  %7.4f  %7.4f\n',Qxx);


%% Visualisierung

figure;
%title('Parabel mit Restriktion über Bedingungsgleichung');
hold on;
plot(xi,y,'*');
t = 0:.1:6;
plot(t,a*t.^2+b*t+c);
axis([0 6 0 8]);
box on
grid on
hold off;

