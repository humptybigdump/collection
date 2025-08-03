% MATLAB-Übung zum Profilfach "Auslegung und Bilanzierung von Mikro-
% strukturreaktoren
% Übung 1 - Aufgabe 3

% Berechnung der x- und y-Vektoren der Parabeln
[y1, x1] = f_U1_a3_parabel(-10, 5);
[y2, x2] = f_U1_a3_parabel(10, -5);


% Plotten der beiden Parabeln in einem Diagramm
plot(x1,y1)
hold on
plot(x2, y2)



% Funktion f_U1_a3_parabel
function [y, x] = f_U1_a3_parabel(a,b)
    % Generieren von Vektor x von (-5+a) bis (5+a) mit linspace und 100
    % Elementen
    x = linspace(-5+a, 5+a, 100);
    
    y = (x-a).^2+b;
end