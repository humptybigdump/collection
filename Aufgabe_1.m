%%% Inversionsmethode %%%

% Quantilfunktion: F^(-1)(u)=-ln(1-u)/lambda
n=100;
lambda=3;

% simuliere n Zufallszahlen aus einer U(0,1)-Verteilung
u=rand(n,1);

% Transfomation der U(0,1)-verteilten ZV mit Quantilfunktion von X erzeugt
% Realisierungen von X
x_inv=-log(1-u)/lambda;

[y,x]=ksdensity(x_inv,'support','positive');

subplot(2,1,1)
histogram(x_inv);
title('Histogramm für X')
xlabel('x')
ylabel('h(x)')

subplot(2,1,2)
plot(x,y);
axis([0 max(x_inv) 0 max(y)]);
title('Kerndichteschätzung für X')
xlabel('x')
ylabel('f(x)')