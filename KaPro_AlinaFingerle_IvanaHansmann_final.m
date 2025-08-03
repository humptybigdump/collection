% Kartenprojektionen Aufgabe 4 Alina Fingerle Ivana Hansmann
close all;
clear all;
clc;

rho=180/pi;
massstab = 50000000;
R = 6371000; % in Metern
coords = xlsread('Koordinaten_1.xlsx'); 
capitals = {'Hauptpunkt K';'Wien'; 'Kopenhagen'; 'Amsterdam'; 'Bern'; 'Budapest'; 'Rom'; 'Vaduz'; 'Prag'; 'Stockholm'; 'Luxemburg'; 'A'; 'B'};
num = length(capitals);

lambda = coords(1:num,1); % Länge in 
phi = coords(1:num,2);    % Breite in 

lambda_rad=lambda/rho; % geographische Länge in Radi
phi_rad=phi/rho; % geographische Breite in Rad

%%
% Hauptpunkt Koordinaten
phi_K = phi_rad(1);         % Breite Hauptpunkt in rad
lambda_K = lambda_rad(1);   % Länge Hauptpunkt in rad
%%
% Sphärischer Abstand δ und Azimut α
delta = acos(sin(phi_K) * sin(phi_rad) + cos(phi_K) * cos(phi_rad) .* cos(lambda_rad - lambda_K));
alpha = atan2(cos(phi_rad) .* sin(lambda_rad - lambda_K), cos(phi_K) * sin(phi_rad) - sin(phi_K) * cos(phi_rad) .* cos(lambda_rad - lambda_K));

% lokale Koordinatenberechnung und Skalierung
x = R * delta .* cos(alpha) / massstab; 
y = R * delta .* sin(alpha) / massstab; 

%% geographische koords
for i = 1:num
    delta_lambda = lambda_rad(i) - lambda_K;
    x_geograph(i) = R * cos(phi_rad(i)) * sin(delta_lambda) / massstab;
    y_geograph(i) = R * (cos(phi_K) * sin(phi_rad(i)) - sin(phi_K) * cos(phi_rad(i)) * cos(delta_lambda)) / massstab;    
end



%% Verzerrungsellipse
A_x = x(12);
A_y = y(12);
B_x = x(13);
B_y = y(13);

a = 1;
b_A = (pi/2-A_x)/cos(A_x);
b_B = (pi/2-B_x)/cos(B_x);

psi = 0:0.001:(2*pi);
rotA = rot(atan2(A_y,A_x));
rotB = rot(atan2(B_y,B_x));
ellipseA = rotA*(1/500)*[b_A*sin(psi); a * cos(psi)] + [A_x; A_y];
ellipseB = rotB*(1/500)*[b_B*sin(psi); a * cos(psi)] + [B_x; B_y];

%% Orthodrome
phi1= phi_rad(3);
phi2=phi_rad(5);
lambda1=lambda_rad(3);
lambda2=lambda_rad(5);

[Orthodrom_x1, Orthodrom_y1] = orth(phi1, phi2, lambda1, lambda2);
delta_orth = acos(sin(phi_K).* sin(Orthodrom_x1) + cos(phi_K) .* cos(Orthodrom_x1) .* cos(Orthodrom_y1 - lambda_K));
alpha_orth = atan2(cos(Orthodrom_x1) .* sin(Orthodrom_y1 - lambda_K), cos(phi_K) .* sin(Orthodrom_x1) - sin(phi_K).* cos(Orthodrom_x1) .* cos(Orthodrom_y1 - lambda_K));

Orthodrom_x = R * delta_orth .* cos(alpha_orth) / massstab; 
Orthodrom_y = R * delta_orth .* sin(alpha_orth) / massstab; 


%% Loxodrome
[Loxodrom_x1, Loxodrom_y1] = lox(phi1, phi2, lambda1, lambda2);
delta_lox = acos(sin(phi_K) * sin(Loxodrom_x1) + cos(phi_K) * cos(Loxodrom_x1) .* cos(Loxodrom_y1 - lambda_K));
alpha_lox = atan2(cos(Loxodrom_x1) .* sin(Loxodrom_y1 - lambda_K), cos(phi_K) * sin(Loxodrom_x1) - sin(phi_K) * cos(Loxodrom_x1) .* cos(Loxodrom_y1 - lambda_K));

Loxodrom_x = R * delta_lox .* cos(alpha_lox) / massstab; 
Loxodrom_y = R * delta_lox .* sin(alpha_lox) / massstab; 



%%  Geographische Koordinaten
figure;
% Plot geographische Koordinaten 
hGeographischeKoordinaten = plot(x_geograph(1:11), y_geograph(1:11), 'o', 'Color', 'green', 'DisplayName', 'Geographische Koordinaten');

% Text für geog Koordinaten (erste zwei Buchstaben)
for i = 1:num-2
    text(x_geograph(i), y_geograph(i), capitals{i}(1:2));
end

% geographisches Netz
hold on;
for phi_P = -90:10:90
    phi_P_rad = deg2rad(phi_P); 
    x_line = [];
    y_line = [];

    for lambda_P = -180:10:180
        lambda_P_rad = deg2rad(lambda_P);

        %  relativ zum Hauptpunkt
        delta_lambda = lambda_P_rad - lambda_K;
        cos_delta = sin(phi_K) * sin(phi_P_rad) + cos(phi_K) * cos(phi_P_rad) * cos(delta_lambda);
        x_geog = R * cos(phi_P_rad) * sin(delta_lambda) / massstab;
        y_geog = R * (cos(phi_K) * sin(phi_P_rad) - sin(phi_K) * cos(phi_P_rad) * cos(delta_lambda)) / massstab;

        if cos_delta > 0 % Nur Punkte auf der sichtbaren Halbkugel
            x_line = [x_line, x_geog];
            y_line = [y_line, y_geog];
        end
    end
    plot(x_line, y_line, 'g');
end

for lambda_P = -180:10:180
    lambda_P_rad = deg2rad(lambda_P); 
    x_line = [];
    y_line = [];

    for phi_P = -90:10:90
        phi_P_rad = deg2rad(phi_P); 
        
        % rel. zum Hauptpunkt
        delta_lambda = lambda_P_rad - lambda_K;
        cos_delta = sin(phi_K) * sin(phi_P_rad) + cos(phi_K) * cos(phi_P_rad) * cos(delta_lambda);
        x_geog = R * cos(phi_P_rad) * sin(delta_lambda) / massstab;
        y_geog = R * (cos(phi_K) * sin(phi_P_rad) - sin(phi_K) * cos(phi_P_rad) * cos(delta_lambda)) / massstab;

        if cos_delta > 0 % Nur Punkte auf der sichtbaren Halbkugel
            x_line = [x_line, x_geog];
            y_line = [y_line, y_geog];
        end
    end
    plot(x_line, y_line, 'g');
end

%% Gitterlinien erstellen für lokales Netz
lat_deg = -90:10:90; 
lon_deg = -180:10:180; 
[u, v] = meshgrid(deg2rad(lon_deg), deg2rad(lat_deg));

% Umrechnung in mittabstandstreue Koordinaten
gitter_x = (pi/2 - u) ;
gitter_y = v;

% Abbildungsgleichungen
x_prime = R* gitter_x .* cos(gitter_y)/ massstab;
y_prime = R* gitter_x .* sin(gitter_y)/ massstab;
% Plotten der Gitterlinien in blau für Parameternetz 
for i = 1:size(x_prime, 1)
    plot(x_prime(i, :), y_prime(i, :), 'blue'); % Breitengradlinien
end
for i = 1:size(y_prime, 2)
    plot(x_prime(:, i), y_prime(:, i), 'blue'); % Längengradlinien
end
%%
% Plot der Verzerrungsellipsen
plot(ellipseA(1,:), ellipseA(2,:), 'DisplayName', 'Ellipse A');
hold on;
plot(ellipseB(1,:), ellipseB(2,:), 'DisplayName', 'Ellipse B');
hold on;

% Plotten der Orthodrome
hOrthodrome = plot(Orthodrom_y, Orthodrom_x, 'LineWidth', 1.5, 'Color', [0.3, 0, 0.35]);
hold on;

% Plot der Loxodrome
hLoxodrome = plot(Loxodrom_y, Loxodrom_x, 'LineWidth', 1.5, 'Color', 'magenta');
hold on;

% Plot A und B
scatter([A_x; B_x], [A_y; B_y], 'DisplayName', 'Feste Punkte');
text([A_x; B_x], [A_y; B_y], ["A", "B"]);

% a und b für A
text(A_x, A_y, ['a = ', num2str(a)], 'HorizontalAlignment', 'left', 'VerticalAlignment', 'bottom');
text(A_x, A_y, ['b_A = ', num2str(b_A)], 'HorizontalAlignment', 'left', 'VerticalAlignment', 'top');

%  a und b für Punkt B
text(B_x, B_y, ['a = ', num2str(a)], 'HorizontalAlignment', 'left', 'VerticalAlignment', 'bottom');
text(B_x, B_y, ['b_B = ', num2str(b_B)], 'HorizontalAlignment', 'left', 'VerticalAlignment', 'top');

hold on;


% Plot-Einstellungen
%grid on;
xlabel('X-Koordinate');
ylabel('Y-Koordinate');
title('Azimutale Abbildung');
legend([hGeographischeKoordinaten, hOrthodrome, hLoxodrome], {'Geographische Koordinaten', 'Orthodrome', 'Loxodrome'});
xlim([-0.1 0.1]);
ylim([0 0.1]);
axis equal; 
hold off;
set(gca,'YTickLabel',[])
set(gca,'XTickLabel',[])
set(gcf,'Units','centimeters')
set(gcf,'InnerPosition',[0 0 23 17])
set(gcf,'OuterPosition',[0 0 29 21])
set(gcf,'PaperOrientation','landscape')

% Funktion zur Erstellung einer Rotationsmatrix
function R = rot(phi)
 R = [cos(phi) -sin(phi); sin(phi) cos(phi)];
end


% Loxodrome berechnen
function [phi_lo,lambda_lo] = lox(p1,p2,l1,l2)
         phi_lo=linspace(p1,p2,50);
         beta_tan=(l2-l1)./(log(tan(pi/4+p2/2))-log(tan(pi/4+p1/2)));
         C=l1-beta_tan.*log(tan(pi/4+p1/2));
         lambda_lo=zeros(1,numel(phi_lo));
         for i=1:numel(phi_lo)
             lambda_lo(i)=beta_tan.*log(tan(pi/4+phi_lo(i)/2))+C;
         end
end

% Orthodrome berechnen
function [phi_ox,lambda_ox] = orth(p1,p2,l1,l2)
         lambda_ox = linspace(l1,l2,50);
         phi_ox=zeros(1,numel(lambda_ox));
         for i=1:numel(lambda_ox)
             phi_ox(i)=atan((tan(p1).*sin(l2-lambda_ox(i))+tan(p2).*sin(lambda_ox(i)-l1))/sin(l2-l1));
         end
end