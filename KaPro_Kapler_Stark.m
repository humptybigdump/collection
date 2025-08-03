%% 
% Kartenprojektionen
% Übung 1
% Autoren: Theodor Kapler, Johannes Stark

%% Alles davor schließen
clear variables
close all
clc

%% Geographische Koordinaten der zu plottenden Städte festlegen
london = [52, 0];
dublin = [53, -6];
kopenhagen = [56, 13];
berlin = [52, 13];
warschau = [52, 21];
wien = [48, 16];
bologna = [44, 11];
marseille = [43, 5];
madrid = [40, -4];
santiago = [43, -8];

% Geographische Koordinaten
geo = [london; dublin; kopenhagen; berlin; warschau; wien; bologna; marseille; madrid; santiago] * (pi / 180);

%% Geographische Koordinaten von Karlsruhe als Hauptpunkt setzen
phi_H = 49 * (pi / 180);
lambda_H = 8 * (pi / 180);
phi_0 = geo(:,1);
lambda_0 = geo(:,2);
delta_lambda = lambda_0 - lambda_H;

% Delta berechnen
sin_delta = sin(phi_H) .* sin(phi_0) + cos(phi_H) .* cos(phi_0) .* cos(delta_lambda);
delta = asin(sin_delta);

% Fallunterscheidung: phi_0 < phi_H --> pi - delta
for i = 1 : length(delta)
    if phi_0(i) < phi_H
        delta(i) = pi - delta(i);
    end
end

% Alpha berechnen
sin_alpha = (sin(delta_lambda) ./ cos(delta)) .* cos(phi_0);
alpha = asin(sin_alpha);

% Karlsruhe hinzufügen
delta = [delta; pi/2];
alpha = [alpha; 0];

% Abbildungskoordinaten berechnen
[x, y] = geo2azi(delta, alpha);

%% Werte der Verzerrungsellipse berechnen
a = cos(delta / 2);
b = 1 ./ a;

%% Orthodrome berechnen
% Anfangs- und Endwerte setzen
delta_ka = delta(11);
alpha_ka = alpha(11);
delta_ko = delta(3);
alpha_ko = alpha(3);

% Festlegung der alpha-Schritte zum Einsetzen in die Formel für Orthodrome
alpha_ortho = (alpha_ka : 0.001 : alpha_ko);

% Deltas der Orthodrome berechnen
tmp_1 = tan(delta_ka) .* sin(alpha_ko - alpha_ortho); % Dieser Term wird unendlich --> siehe Absprache in der Mail
tmp_2 = tan(delta_ko) .* sin(alpha_ortho - alpha_ka);
tmp_3 = sin(alpha_ko - alpha_ka);

delta_ortho = atan((tmp_1 + tmp_2) ./ tmp_3);

% Abbildungskoordinaten berechnen
[x_ortho, y_ortho] = geo2azi(delta_ortho, alpha_ortho);

%% Loxodrome berechnen
% Festlegung der delta-Schritte zum Einsetzen in die Formel für Orthodrome
delta_loxo = (delta_ko : 0.001 : delta_ka);

% Alphas der Loxodrome berechnen
tmp_1 = alpha_ko - alpha_ka; 
tmp_2 = log(tan(pi/4 + delta_ko/2));
tmp_3 = log(tan(pi/4 + delta_ka/2));

tan_beta = tmp_1 ./ (tmp_2 - tmp_3);

tmp_4 = tan_beta .* log(tan(pi/4 + delta_loxo/2));
tmp_5 = tan_beta .* log(tan(pi/4 + delta_ka/2));

alpha_loxo = tmp_4 - tmp_5 + alpha_ka;

% Abbildungskoordinaten berechnen
[x_loxo, y_loxo] = geo2azi(delta_loxo, alpha_loxo);

%% Plotten
% Offset für Text festlegen
xoffset = 0.002;
yoffset = 0.008;

% Schriftgröße für Beschriftungen festlegen
fs = 10;

% Ausmaße festlegen
width = 0.297;
height = 0.210;
x_min = - height / 2;
x_max = height / 2;
y_min = - width / 2;
y_max = width / 2;

% Plot
figure
scatter(y, x)
xlim([y_min y_max]);
ylim([x_min x_max]);
axis equal

% Beschriftungen 
text(y(1) + xoffset, x(1) + yoffset, ['London', newline, 'a: ', num2str(a(1)), newline, 'b: ', num2str(b(1))], 'FontSize', fs)
text(y(2) + xoffset, x(2) + yoffset, ['Dublin', newline, 'a: ', num2str(a(2)), newline, 'b: ', num2str(b(2))], 'FontSize', fs)
text(y(3) + xoffset, x(3) + yoffset, ['Kopenhagen', newline, 'a: ', num2str(a(3)), newline, 'b: ', num2str(b(3))], 'FontSize', fs)
text(y(4) + xoffset, x(4) + yoffset, ['Berlin', newline, 'a: ', num2str(a(4)), newline, 'b: ', num2str(b(4))], 'FontSize', fs)
text(y(5) + xoffset, x(5) + yoffset, ['Warschau', newline, 'a: ', num2str(a(5)), newline, 'b: ', num2str(b(5))], 'FontSize', fs)
text(y(6) + xoffset, x(6) + yoffset, ['Wien', newline, 'a: ', num2str(a(6)), newline, 'b: ', num2str(b(6))], 'FontSize', fs)
text(y(7) + xoffset, x(7) + yoffset, ['Bologna', newline, 'a: ', num2str(a(7)), newline, 'b: ', num2str(b(7))], 'FontSize', fs)
text(y(8) + xoffset, x(8) + yoffset, ['Marseille', newline, 'a: ', num2str(a(8)), newline, 'b: ', num2str(b(8))], 'FontSize', fs)
text(y(9) + xoffset, x(9) + yoffset, ['Madrid', newline, 'a: ', num2str(a(9)), newline, 'b: ', num2str(b(9))], 'FontSize', fs)
text(y(10) + xoffset, x(10) + yoffset, ['Santiago de Compostela', newline, 'a: ', num2str(a(10)), newline, 'b: ', num2str(b(10))], 'FontSize', fs)
text(y(11) + xoffset, x(11) - 0.008, ['Karlsruhe', newline, 'a: ', num2str(a(11)), newline, 'b: ', num2str(b(11))], 'FontSize', fs)
hold on

% Orthodrome plotten --> Auskommentiert da keine sinnvollen Ergebnisse
%plot(y_ortho, x_ortho)
%hold on

% Loxodrome plotten 
plot(y_loxo, x_loxo)

% Achsen auf A4 begrenzen
axis([-width/2 width/2 -height/2 height/2])

% Achsbeschriftung manipulieren
xticks([-0.1 -0.05 0 0.05 0.1])
xticklabels({'-1000 km','-500 km', '0 km', '500 km', '1000 km'})
yticks([-0.1 -0.05 0 0.05 0.1])
yticklabels({'-1000 km','-500 km', '0 km', '500 km', '1000 km'})

% Beschriftungen nach innen setzen
ax = gca;
ax.XRuler.TickLabelGapOffset = -20;
ax.YRuler.TickLabelGapOffset = -40;

% Beschriftungen kleiner machen
ax.FontSize = 7;

% Raster an
grid on

% Alles außerhalb des Plots abschneiden
set(gca, 'LooseInset', [0,0,0,0]);

% Plot als PDF-Datei speichern
set(gcf, 'PaperOrientation', 'landscape');
set(gcf, 'PaperUnits', 'normalized');
set(gcf, 'PaperPosition', [0 0 1 1]);
print(gcf, 'Ergebnis_Kapler_Stark', '-dpdf')

%% Abbildungsfunktion für Azimutale mittabstandstr. Abbildung:
% Übergabeparameter delta und alpha in rad
% Rückgabeparameter x (Hochwert) und y (Rechtswert) in m
function [x, y] = geo2azi(delta, alpha)
    R = 6371000;
    M = 1 / 10000000;
    x = M * R * (pi/2 - delta) .* cos(alpha);
    y = M * R * (pi/2 - delta) .* sin(alpha);
end


