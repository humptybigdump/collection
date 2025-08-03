% MATLAB-Übung zum Profilfach "Auslegung und Bilanzierung von Mikro-
% strukturreaktoren
% Übung 1 - Aufgabe 2

% 2a)
% 2b)
% Zuweisen von a1 und b1
a1 = -10;
b1 = 5;
% Generieren von Vektor x von (-5+a) bis (5+a) mit linspace und 100
% Elementen
x = linspace(-5+a1, 5+a1, 100);

% Berechnung von y (Beachte .^2)
y = (x-a1).^2 + b1;

% 2c)
% Generiere den Plot der ersten Parabel
plot(x,y)

% 2d)
% Halte den Plot in der aktuellen figure fest
hold on

% 2e)
% Zuweisen von a2 und b2
a2 = 10;
b2 = -5;
% Generieren von Vektor x von (-5+a) bis (5+a) mit linspace und 100
% Elementen
x = linspace(-5+a2, 5+a2, 100);
y = (x-a2).^2 + b2;
% Füge den Plot der zweiten Parabel der ersten hinzu
plot(x,y)

