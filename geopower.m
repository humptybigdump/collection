% Compute the geothermal power as a function of flow rate and production
% temperature
% INPUT:
%   - Qvector: vector of the flow rates, in L/s
%   - Tvector: vector of the production temperature, in °C
% OUTPUT:
%   - GP: matrix of the geothermal power, in W, with nrow=numel(Qvector) and
%   ncol=numel(Tvector)
%
% The formula to compute the geothermal power is:
% GP = rho.cp.Q.(TPROD - TREINJ)
%
% GP:	Produced thermal power [W]
% Q:	Flow rate [L/s]
% cp:	Spec. heat capacity of water ~4200 [J kg 1K 1]
% rho: 	Density of water ~1000 [kg m 3]
% TPROD:	Production temperature
% TREINJ:	Re-injection temperature = 70°C
%
function GP = geopower(Qvector, Tvector)


%% Initialize parameters
% Qvector = 0:1:150;
% Tvector = 70:1:190;
% 0:50:150, 70:20:190
rho = 1000;
cp = 4200;
tinj = 70;

% with the formula below, Qvector and Tvector have to be row vectors. So, 
% check it and correct if necessary
% check that with have vectors and not matrices
if ((numel(Qvector)~=length(Qvector)) || (numel(Tvector)~=length(Tvector)))
    fprintf(1, 'Sorry, you must give vectors as input not matrices\nTry again!\n');
    return;
end
% check the vectors
if size(Qvector,1)~=1
    Qvector=Qvector';
end
if size(Tvector,1)~=1
    Tvector=Tvector';
end

% now we can compute the geothermal power
GP = rho*cp*1e-3*Qvector'*(Tvector-tinj); % [numel(Qvector), numel(Tvector)]

%% save matrix GP into a text file
dlmwrite('GP.txt', GP, 'precision', '%.2e', 'delimiter', ';', 'newline', 'pc');

%% make a 2D graph to show the geothermal power in color, as a function of
% the flow rate (y-axis) and the temperature (x-axis)
% use pcolor which also works with non-regular grids

% plot GP in mega Watt
figure('color','w','name','With pcolor');
pcolor(Tvector, Qvector, GP*1e-6);
shading interp; % to have all values taken into account
xlabel('Production temperature [°C]');
ylabel('Flow rate [L/s]');
title('Geothermal power [MW]');
% set only 8 colours in the colormap
colormap(hsv(8));
% show colorbar
cc=colorbar;
cc.Label.String = 'Geothermal power (in MW)';

% add contours
hold on;
contour(Tvector, Qvector, GP*1e-6,...
    0:10:max(max(GP*1e-6)),...
    'Showtext', 'on',...
    'Linestyle', '-', 'LineColor', [0 0 0]);
hold off;

% save figure
print(gcf, 'geopower_pcolor', '-djpeg');
%saveas(gcf, 'geopower_pcolor', 'jpeg');

%% make a 3D surface out of the GP matrix
figure('color','w','name','With surf');
surf(Tvector, Qvector, GP*1e-6);
xlabel('Production temperature [°C]');
ylabel('Flow rate [L/s]');
zlabel('Geothermal power [MW]');
shading interp;
colormap(hsv(8));
cc=colorbar;
cc.Label.String = 'Geothermal power (in MW)';

% 3D contours every 5 MW starting from 5 up to 80 MW
hold on;
contour3(Tvector,Qvector,GP*1e-6, 0:10:max(max(GP*1e-6)), '-k');

% save figure
print(gcf, 'geopower_surf', '-djpeg');

end
