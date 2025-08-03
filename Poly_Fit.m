% Skript zur Anpassung eine Polynoms 5. Grades an gegebene Stoffdaten
clear, clc;

% Stoffdaten aus: CRC handbook of chemistry and physics, 96.Aufl. 
% Boca Raton, CRC Press, 2015
% Gibbs Energie in kJ/mol, Referenzdruck: 1 bar
T = [298.15, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200, ...
         1300, 1400, 1500]';

G_H2  = [0,0,0,0,0,0,0,0,0,0,0,0,0,0]';
G_CH4 = [-50.530, -50.381, -41.827, -32.525, -22.690, -12.476, -1.993, ...
         8.677, 19.475, 30.358, 41.294, 52.258, 63.231, 74.200]';
G_H2O = [-228.582, -228.500, -223.900, -219.050, -214.008, -208.814, ...
         -203.501, -198.091, -192.603, -187.052, -181.450, -175.807, ...
         -170.132, -164.429]';         
G_CO  = [-137.168, -137.333, -146.341, -155.412, -164.480, -173.513, ...
        -182.494, -191.417, -200.281, -209.084, -217.829, -226.518, ...
        -235.155,-243.742]'; 
G_CO2 = [-394.373, -394.379, -394.656, -394.914, -395.152, -395.367, ...
         -395.558, -395.724, -395.865, -395.984, -396.081, -396.159, ...
         -396.219, -396.264]';

% Daten in ein Array zusammenfassen
G_data = [G_H2, G_CH4, G_H2O, G_CO, G_CO2];
temp = size(G_data);

% Daten mit polyfit anpassen
for i=1 : temp(2)
    Poly_Koeff(:,i) = polyfit(T,G_data(:,i),2);
end

save('Poly_Koeff.mat', 'Poly_Koeff')

