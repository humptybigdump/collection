% USAGE: 
%   GP=geopower(Qvec, Tvec)
% 
% FUNCTION:
%   - Computes the geothermal power (GP, matrix) from a given range of
%   production temperatures (Tvec, vector) and given range of production
%   flow rates (Qvec, vector)
% 
% INPUT:
%   - Qvec: a vector of flow rate values in L/s
%   - Tvec: a vector of fluid temperature in degrees Celsius
% 
% OUTPUT:
%   - GP: a matrix [length(Qvec), length(Tvec)] of the geothermal power in
%   Watt
%   - A text file named 'GP.txt' is also created and contains the elements
%   of GP. It is created in the folder where the function is launched
%
% NOTE:
%   - In this function we will work with column vectors
%

function GP=geopower(Qvec, Tvec)

% Qvec = 0:50:150; % in L/s
% Tvec = 70:20:190; % in deg C

% define the constants of the problem
cpf = 4200; % J/kg/K
rho = 1000; % kg/m3
Tinj = 70; % deg C

% adapt Qvec and Tvec for proper vector multiplication
% we want column vectors
% WE ASSUME THAT WE HAVE VECTORS AS INPUT

[~, nc] = size(Qvec);
if nc~=1
    Qvec = Qvec';
    % now Qvec is a column vector
end

if size(Tvec,2)~=1
    Tvec = Tvec';
    % now Tvec is a column vector
end

% geothermal power, GP = rho * cpf * Q * (Tprod - Tinj)
% mind that we have to change units of the flow rate from L/s to m3/s
GP = rho*cpf*Qvec*1e-3*(Tvec'-Tinj); % GP [length(Qvec), length(Tvec)]

% save the result in a file
writematrix(GP, 'GP.txt');

% plot result as a 2D color plot
pcolor(Tvec, Qvec, GP*1e-6);
shading('interp');
colormap(jet(8));
cc = colorbar;
cc.Label.String = 'Power (in MW)';
xlabel('Production temperature (°C)');
ylabel('Production flow rate (L/s)');
title('Geothermal power (MW)');
hold on;
contour(Tvec, Qvec, GP*1e-6, (0:10:max(GP*1e-6)), 'LineColor','k', 'ShowText','on');
hold off;

% surface in 3D now (with contours in 3D too)
% create a new figure to prevent erasing the previous one
figure();

surf(Tvec, Qvec, GP*1e-6);
shading('interp');
colormap(hsv(8));
cc = colorbar;
cc.Label.String = 'Power (in MW)';
xlabel('Production temperature (°C)');
ylabel('Production flow rate (L/s)');
title('Geothermal power (MW)');
hold on;
contour3(Tvec, Qvec, GP*1e-6, (0:10:70), 'LineColor','k', 'ShowText','on');
hold off;

% keyboard;