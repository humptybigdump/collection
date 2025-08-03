%% Kartenprojekton, Aufgabe 3
% Nils Schneider, Lazaro Bayer

clc
close all
clear variables

%% Konstanten
rho = 180/pi;
K = [49; 8];        % Hauptpunkt Karlsruhe
K_rad = K/rho;
M = 1/50000000;     % Maßstab
R = 6371000;        % Erdradius (m)

% Punkte
A = [80; -20];
B = [75; 30];
A_rad = A/rho;
B_rad = B/rho;

% Städte
% A, B, Wien, Kopenhagen, Amsterdam, Bern, Tallin, Reykjavik, Nordkap,
% Moskau, Warschau, Kiew
city_name = ["A", "B", "Wi", "Ko", "A", "Be", "T", "R", "N", "M", "Wa", "Ki"];
city_lat = [48 55 52 46 59 64 71 55 52 50];
city_lon = [16 12  4  7 24 -21 25 37 21 30];
city = [[A B], [city_lat; city_lon]];
city_rad = city/rho;


%% Projektion
% Maßstab
c = 2*R*M;

delta = f_abb_delta(city_rad(1,:),K_rad(1),city_rad(2,:),K_rad(2));
alpha = f_abb_alpha(city_rad(1,:),K_rad(1),city_rad(2,:),K_rad(2));

abb_city_x = c*f_delta(delta).*cos(alpha);
abb_city_y = c*f_delta(delta).*sin(alpha);


%% Tissot'sche Indikatrix
A_a = f_a(pi/2 - A_rad(1));
A_b = f_b(pi/2 - A_rad(1));
B_a = f_a(pi/2 - B_rad(1));
B_b = f_b(pi/2 - B_rad(1));
ell_A = f_ellipse(abb_city_x(1), abb_city_y(1), A_a, A_b, 1/100);
ell_B = f_ellipse(abb_city_x(2), abb_city_y(2), B_a, B_b, 1/100);


%% Orthodrome
% Kopenhagen --> Tallin
ortho_p1 = city_rad(1,9);
ortho_p2 = city_rad(1,12);
ortho_l1 = city_rad(2,9);
ortho_l2 = city_rad(2,12);

ortho_l = linspace(ortho_l1, ortho_l2, 100);
ortho_p = f_orthodrom(ortho_p1, ortho_p2, ortho_l1, ortho_l2, ortho_l);

ortho_delta = f_abb_delta(ortho_p,K_rad(1),ortho_l,K_rad(2));
ortho_alpha = f_abb_alpha(ortho_p,K_rad(1),ortho_l,K_rad(2));

abb_ortho_x = c*f_delta(ortho_delta).*cos(ortho_alpha);
abb_ortho_y = c*f_delta(ortho_delta).*sin(ortho_alpha);


%% Loxodrome
% Koppenhagen --> Tallin
loxo_p1 = city_rad(1,9);
loxo_p2 = city_rad(1,12);
loxo_l1 = city_rad(2,9);
loxo_l2 = city_rad(2,12);

loxo_p = linspace(loxo_p1, loxo_p2, 100);
loxo_l = f_loxodrom(loxo_p1, loxo_p2, loxo_p, loxo_l1, loxo_l2);

loxo_delta = f_abb_delta(loxo_p, K_rad(1), loxo_l, K_rad(2));
loxo_alpha = f_abb_alpha(loxo_p, K_rad(1), loxo_l, K_rad(2));

abb_loxo_x = c*f_delta(loxo_delta).*cos(loxo_alpha);
abb_loxo_y = c*f_delta(loxo_delta).*sin(loxo_alpha);


%% Geographisches Netz
figure
% Breitenkreise
for netz_p = -90:10:90
    netz_p = netz_p/rho;
    netz_B_x = [];
    netz_B_y = [];
    for netz_l = -180:10:180
        netz_l = netz_l/rho;

        netz_B_delta = f_abb_delta(netz_p,K_rad(1),netz_l,K_rad(2));
        netz_B_alpha = f_abb_alpha(netz_p,K_rad(1),netz_l,K_rad(2));

        abb_netz_B_x = c*f_delta(netz_B_delta).*cos(netz_B_alpha);
        abb_netz_B_y = c*f_delta(netz_B_delta).*sin(netz_B_alpha);

        if acos(netz_B_delta) > 0
            netz_B_x = [netz_B_x, abb_netz_B_x];
            netz_B_y = [netz_B_y, abb_netz_B_y];
        end       
    end
    hold on
    p1 = plot(netz_B_y,netz_B_x,'Color',"#77AC30");
end
% Meridiane
for netz_l = -180:10:180
    netz_l = netz_l/rho;
    netz_M_x = [];
    netz_M_y = [];
    for netz_p = -90:10:90
        netz_p = netz_p/rho;

        netz_M_delta = f_abb_delta(netz_p,K_rad(1),netz_l,K_rad(2));
        netz_M_alpha = f_abb_alpha(netz_p,K_rad(1),netz_l,K_rad(2));

        abb_netz_M_x = c*f_delta(netz_M_delta).*cos(netz_M_alpha);
        abb_netz_M_y = c*f_delta(netz_M_delta).*sin(netz_M_alpha);

        if acos(netz_M_delta) > 0
            netz_M_x = [netz_M_x, abb_netz_M_x];
            netz_M_y = [netz_M_y, abb_netz_M_y];
        end       
    end
    hold on
    p1 = plot(netz_M_y,netz_M_x,'Color',"#77AC30");
end


%% Parameternetz
hold on
pnetz_p = -90:10:90;%d
pnetz_l = -180:10:180;%a
pnetz_p = pnetz_p/rho;
pnetz_l = pnetz_l/rho;
% Breitenkreise
for i = 1:length(pnetz_p)
    abb_pnetz_B_x = zeros(length(pnetz_l));
    abb_pnetz_B_y = zeros(length(pnetz_l));
    for j = 1:length(pnetz_l)
        abb_pnetz_B_x(j) = c*sin(pi/4 - pnetz_p(i)).*cos(pnetz_l(j));
        abb_pnetz_B_y(j) = c*sin(pi/4 - pnetz_p(i)).*sin(pnetz_l(j));
    end
    hold on
    plot(abb_pnetz_B_y,abb_pnetz_B_x,'Color',"#0072BD");
end
% Meridiane
for i = 1:length(pnetz_l)
    abb_pnetz_M_x = zeros(length(pnetz_p));
    abb_pnetz_M_y = zeros(length(pnetz_p));
    for j = 1:length(pnetz_p)
        abb_pnetz_M_x(j) = c*sin(pi/4 - pnetz_p(j)).*cos(pnetz_l(i));
        abb_pnetz_M_y(j) = c*sin(pi/4 - pnetz_p(j)).*sin(pnetz_l(i));
    end
    hold on
    p2 = plot(abb_pnetz_M_y,abb_pnetz_M_x,'Color',"#0072BD");
end


%% Plot
hold on
axis equal

% Städte
p3 = plot(abb_city_y, abb_city_x,'o','Color', 'black');
text(abb_city_y+0.002, abb_city_x, cellstr(city_name), 'FontSize', 12, 'Color', 'black');

text(abb_city_y(1,1)+0.005, abb_city_x(1,1), ...
    "a = " + A_a + '\newline' + " b = " + A_b  , 'FontSize', 12, 'Color', 'black');
text(abb_city_y(1,2)+0.005, abb_city_x(1,2), ...
    "a = " + B_a + '\newline' + " b = " + B_b  , 'FontSize', 12, 'Color', 'black');
p4 = plot(ell_A(2,:),ell_A(1,:),'Color', "#D95319");
p4 = plot(ell_B(2,:),ell_B(1,:),'Color', "#D95319");

%figure
hold on
% Orthodrome
p5 = plot(abb_ortho_y,abb_ortho_x,'Color', "#EDB120");
% Loxodrome
p6 = plot(abb_loxo_y,abb_loxo_x,'Color', "#7E2F8E");

% Beschriftung
title('Aufgabe 3')
legende = legend([p1, p2(1), p3, p4, p5, p6], {'Geographisches Netz', 'Parameternetz', 'Städte', 'Tissotsche Indikatrix', 'Orthodrome', 'Loxodrome'}, 'Location', 'southwest');
title(legende,'Karte | Maßstab 1:50000000')
ylim([-0.01 0.09])
%xlim([-0.04 0.05])
set(gca,'YTickLabel',[])
set(gca,'XTickLabel',[])
set(gca,'XColor','none','YColor','none')
set(gca,'color','none')
set(gcf,'Units','centimeters')
set(gcf,'InnerPosition',[0 0 25 19])
set(gcf,'OuterPosition',[0 0 29 21])
set(gcf,'PaperOrientation','landscape')
saveas(gcf,'Kapro_3_SchneiderBayer.pdf')


%% Funktionen
function ell = f_ellipse(x, y, a, b, m)
    phi = atan2(x, y);
    psi = 0:0.001:2*pi;
    R = [cos(phi) -sin(phi); sin(phi) cos(phi)];
    ell = [x; y] + R*m*[b*sin(psi); a*cos(psi)];
end

function r = f_delta(delta)
    r = sin(delta/2);
end

function delta = f_abb_delta(ps, ph, ls, lh)
    delta = acos(sin(ph).*sin(ps) + cos(ph).*cos(ps).*cos(ls - lh));
end

function alpha = f_abb_alpha(ps, ph, ls, lh)
    alpha = atan2(cos(ps).*sin(ls - lh), cos(ph)*sin(ps) - sin(ph)*cos(ps).*cos(ls - lh));
end

function a = f_a(delta)
    a = cos(delta/2);
end

function b = f_b(delta)
    b = 2*sin(delta/2)/sin(delta);
end

function phi = f_orthodrom(phi1, phi2, lambda1, lambda2, d_lambda)
    phi = atan((tan(phi1)*sin(lambda2 - d_lambda) + tan(phi2)*sin(d_lambda - lambda1))/sin(lambda2 - lambda1));
end

function lambda = f_loxodrom(phi1, phi2, phi_d, lambda1, lambda2)
    tan_beta = (lambda2 - lambda1)./(log(tan(pi/4 + phi2/2)) - log(tan(pi/4 + phi1/2)));
    lambda = tan_beta.*log(tan(pi/4 + phi_d/2)) + lambda1 - tan_beta.*log(tan(pi/4 + phi1/2));
end