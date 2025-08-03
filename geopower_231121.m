% Generate 2D and 3D figures showing in colour the geothermal power output
% from given production flow rate and temperature ranges.
% 
% INPUT:
%  Qvec: [1,n] row vector of the production flow rate range in L/s.
%        for example [0:50:150]
%  Tvec: [1,m] row vector of the production temperature in degC.
%        for example [70:20:190]
%
% OUTPUT:
%   GP: [n,m] matrix of the geothermal power for all flow rate-temperature
%   combinations
%
function GP = geopower_231121(Qvec, Tvec)

%% below: only uncomment if function line is commented and we want a script
%Qvec = 0:50:150; % L/s => WARNING, this is not SI units!
%Tvec = 70:20:190; % degC
%==========================================================================

%% check input parameters
% check that Qvec and Tvec exist!
if isempty(Qvec) || isempty(Tvec)
    disp('Please give 2 row vectors as input');
    return;
end
% check that Qvec and Tvec are not matrices and are row vectors
% if not row vectors, transpose them
[nr, nc] = size(Qvec);
if nr ~= 1
    if nc ~= 1
        % this is a matrix
        disp('Please give 2 row vectors as input');
        return;
    else
        Qvec = Qvec';
    end
end
[nr, nc] = size(Tvec);
if nr ~= 1
    if nc ~= 1
        % this is a matrix
        disp('Please give 2 row vectors as input');
        return;
    else
        Tvec = Tvec';
    end
end
%

%% variable assignment
rho = 1000; % kg/m3
cp = 4200; % J/kg/K
tinj = 70; % degC

%% apply the formula

GP = rho*cp*1e-3*Qvec'*(Tvec-tinj); % only valid if Qvec and Tvec are row vectors

%% 2D presentation of results
% use pcolor to show the result
% pcolor(GP);
pcolor(Tvec,Qvec,GP*1e-6); % Gpower is plotted in MW
shading interp;
xlabel('Production temperature (°C)');
ylabel('Production rate (l/s)');
c = colorbar;
c.Label.String = 'Geothermal power [MW]';
colormap(hsv(32));
% now add the contours
hold on;
% contour(Tvec,Qvec,GP*1e-6,10,'k', 'LineWidth',2,'ShowText','on');
contour(Tvec,Qvec,GP*1e-6, 0:10:max(max(GP))*1e-6,'k', 'LineWidth',2,'ShowText','on');

%% 3D presentation of results
% use surf to show the result
figure();
surf(Tvec,Qvec,GP*1e-6); % Gpower is plotted in MW
shading interp;
xlabel('Production temperature (°C)');
ylabel('Production rate (l/s)');
zlabel('Production power (MW))');
c = colorbar;
c.Label.String = 'Geothermal power [MW]';
colormap(turbo(8));
% now add the contours
hold on;
% contour(Tvec,Qvec,GP*1e-6,10,'k', 'LineWidth',2,'ShowText','on');
contour3(Tvec,Qvec,GP*1e-6, 0:10:max(max(GP))*1e-6,'k', 'LineWidth',2,'ShowText','on');
