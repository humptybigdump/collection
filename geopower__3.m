%% function GP = geopower(Qvec, Tvec)
% Function to compute the geothermal power as a function of production flow
% rate and production temperature.
% Results are displayed in 2D and 3D
%
% INPUT:
%
% * Qvec: [1, n] vector of production flow rates in L/s
% * Tvec: [1, m] vector of production temperatures in degrees Celsius
%
% OUTPUT:
%
% * GP: [n, m] matrix containing the geothermal power
%
% EXAMPLE:
%
% GP = geopower([0:1:150], [70:1:190]);
%%

function GP = geopower(Qvec, Tvec)

%% define the internal variables
cpf = 4200; % J/kg/K
rhof = 1000; % kg/m^3
tinj = 70; % in degrees Celsius
% Qvec = 0:50:150; % from 0 to 150 L/s every 50 L/s
% Tvec = 70:20:190; % from 70°C to 190°C every 20°C

%% compute the GP matrix
GP = rhof * cpf * 1e-3 * transpose(Qvec) * (Tvec - tinj);
GPMW = GP*1e-6;

%% show results in MW

% pcolor
pcolor(Tvec, Qvec, GPMW);
shading interp;
colorbar;
colormap(parula(8));
xlabel('Production temperature [°C]');
ylabel('Production flow rate [L/s]');
title('Geothermal power [MW]');
hold on;
contour(Tvec, Qvec, GPMW, [0:10:max(max(GPMW))], 'k', 'ShowText', 'on');
hold off;
% save figure
saveas(gcf,'gpmw_pcolor','jpg');

% surf
figure;
surf(Tvec, Qvec, GPMW);
shading interp;
colorbar;
colormap(parula(8));
xlabel('Production temperature [°C]');
ylabel('Production flow rate [L/s]');
zlabel('Geothermal power [MW]');
title('Geothermal power [MW]');
hold on;
contour3(Tvec, Qvec, GPMW, [0:10:max(max(GPMW))], 'k', 'ShowText', 'on');
% save figure
saveas(gcf,'gpmw_surf','jpg');

