% 2D-Plot
clear all
close all
clc

figure(1)              % interner Name der Abbildung

% Liste der gewünschten x-Werte anlegen
x=-2*pi:4*pi/500:2*pi;
length(x)

y1=sin(x);             % Liste der Funktionswerte berechnen
y2=cos(x);      

length(y1)             % gibt die Array-Länge von y1 aus (Anzahl der Einträge)

% Beide Listen gegeneinander auftragen
plot(x,y1,'b-')
hold on                % beim nächsten plotten nicht löschen

plot(x,y2,'r-.')       % also ersten Plot beibehalten
hold off

% Achsenbeschriftung und Titel:
%xlable('x')
xlabel('x_{Werte} {\it\alpha} in rad')        % Hinweis: Matlab versteht Latex
ylabel('y ohne Einheiten')
title ('Übung zu Diagramm')

% Legenden:
L=legend('sin','cos')
get(L)
set(L,'FontSize',20,'FontName','Calibri','FontAngle','italic')
%legend('Sinus','Kosinus','Location','SouthWest', )

% Farben: r, g, b, y, m, c, w, k
% Linien: -, -., --, :

%Statt Linien auch unverbundene Markierung
% x, o, *, +, v, ^, <, >, s (square), p (pentagon), h (hexagon)
% d (diamond, . für Markierungspunkt

% Randfarben eines Markierungszeichen heißt: 'MarkerEdgeColor'
% Die Füllfarbe: 'MarkerFaceColor'


x=-2*pi:4*pi/10:2*pi;
y1=cos(x);
hold on

plotHandle = plot(x,y1);
get(plotHandle)
set(plotHandle,'Marker','p','Linestyle','none','Markersize',16,'MarkerFaceColor','y','MarkerEdgeColor','m')
hold off

% Neue Figure
fig_1 = figure(2)

x=-2:0.01:2;
y=exp(-x.^2);       % .vor ^, * und / löst "elementweise Berechnung" aus!
plot(x,y,'g-','LineWidth',3)
title('Gauß-Glocke')
xlabel('x')
ylabel('y')

% Abspeichern von Plot im aktuellen Verzeichnis
% Möglichkeit 1: savesas als png (nicht empfohlen!)
saveas(fig_1, 'Test.png')
%Möglichkeit 2: print als .png mit Auflösung 1000 dpi
print(fig_1, 'Gaussglocke', '-dpng', '-r1000')
