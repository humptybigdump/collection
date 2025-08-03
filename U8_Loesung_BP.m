clear all
close all
clc

%% Impedanzdaten einlesen

data_path = 'U:\Lehre\BMML\Übung\S13 Ü8 Elektrodenoptimierung\Lösungskripte und Daten\EIS-Daten';

% Speichern aller .z Dateien im Datenordner, dabei ist die Namensstruktur der Dateien bekannt
data_info = dir(fullfile(data_path, '*.z')); % output: struct array, also werden indize mit () und nicht mit {} wie bei struct eingesetzt
for i=1 : length(data_info)
    numPos = regexp(data_info(i).name, '\d'); % output: alle indize von natuerlichen Zahlen
    data_info(i).SOC = data_info(i).name(numPos(1): numPos(2)); 
    data_info(i).T = data_info(i).name(numPos(3): numPos(4));
    % Anlegen struct container fuer extrahierte Daten
    data{i} = readData(strcat(data_info(i).folder,'/',data_info(i).name));
end

keyboard

%% Impedanzen plotten

figure(1);
    hold all;
    for i=1:length(data)
        plot(data{i}.Z1_re, data{i}.Z1_im);
        iAmLegend{i} = ['SOC: ' data_info(i).SOC ' | T: ' data_info(i).T];
    end
axis equal;
set(gca, 'YDir', 'reverse');
grid on;
xlabel('Re(Z)');
ylabel('Im(Z)')
legend(iAmLegend,'Location','e');
title('Impedanzen aller Messreihen');

keyboard

%% ESB-Modell aufsetzen und parametrieren

f = data{1}.freq;
L = 1e-9;
R0 = 0.018;
RQ1_R = 0.015;
RQ1_n = 0.9;
RQ1_Q = 1.6e-4;
RQ2_R = 0.05;
RQ2_n = 0.9;
RQ2_Q = 0.016;
FLW_R = 0.08;
FLW_T = 200;
FLW_n = 0.5;
C = 700;

% alternative Werte (zum Ausprobieren)
% L = 1e-9;
% R0 = 0.018;
% RQ1_R = 0.055;
% RQ1_n = 0.9;
% RQ1_Q = 1.6e-4;
% RQ2_R = 0.05;
% RQ2_n = 0.9;
% RQ2_Q = 100;
% FLW_R = 0.8;
% FLW_T = 200;
% FLW_n = 0.5;
% C = 700;

modelValues = [L R0 RQ1_R RQ1_n RQ1_Q RQ2_R RQ2_n RQ2_Q FLW_R FLW_T FLW_n C];
Z = calc_Z(modelValues,f);

plot(Z, 'k');
axis equal;
set(gca, 'YDir', 'reverse');
grid on;
xlabel('Re(Z)');
ylabel('Im(Z)')
iAmLegend{end+1} = 'Startwerte';
legend(iAmLegend);

keyboard

%% Startparameter und Boundaries definieren
startparameter = [L R0 RQ1_R RQ1_n RQ1_Q RQ2_R RQ2_n RQ2_Q FLW_R FLW_T FLW_n C];
lb =             [0 0  0     0.5   1e-5  0     0.4   1e-4  0     0     0     0];
ub =             [1 0.02 0.1 1     1e-2  1     1     1     100   1000  0.5   1000];

%% Optionen setzen
options= optimset('MaxIter', 10e3 ,'TolX',1e-12, 'TolCon', 1e-20,...
                  'TolFun', 1e-12, 'MaxFunEvals', 1e12,...
                  'Algorithm', 'active-set', 'Display','final');

%% Optimierung
figure(2);
axis tight
for i = 1 : length(data)
    % Definiere eine zu minimierende Funktion 
    to_minimize =@(startparameter)guetemass(data{i},startparameter,f);
    % Optimierung der Modellparameter
    tic
        [resultParameters{i}, fval, exitflag, output] = fmincon(to_minimize, startparameter,[],[],[],[],lb,ub,[],options);
    toc
    Modelparameter = resultParameters{i};
    disp(Modelparameter);

    ZModel = calc_Z(Modelparameter,f);
    subplot(3,2,i)
    hold all;
    plot(data{i}.Z1_re, data{i}.Z1_im);
    plot(ZModel, 'r');
    axis equal;
    set(gca, 'YDir', 'reverse');
    grid on;
    name = ['Messung SOC:' data_info(i).SOC ' | T:' data_info(i).T];
    legend(name, 'Model','Location','e');
    grid on;
    xlabel('Re(Z)');
    ylabel('Im(Z)')
    title('Messung vs. Modell');
    disp([num2str(i) ' / ' num2str(length(data))])
end

keyboard

%% Plotten der SOC-abhaengigen Parameter

figure(3)
startparameter = [L R0 RQ1_R RQ1_n RQ1_Q RQ2_R RQ2_n RQ2_Q FLW_R FLW_T FLW_n C];
 
interestingParameters = [2 3 6 9];
interestingParamNames = {'R0' 'RQ1_R' 'RQ2_R' 'FLW_R'};
for i = 1:length(interestingParameters)
    subplot(2,2,i)
    hold all;
    for j = 1:length(data_info)
        plot(str2num(data_info(j).SOC), resultParameters{j}(interestingParameters(i)), 'o');
        iAmLegend{j} = ['SOC: ' data_info(j).SOC ' | T:' data_info(j).T];
    end
    grid on;
    xlabel('SOC [%]')
    ylabel('R [\Omega]')
    title(['Parameter: ', interestingParamNames{i}])
    legend(iAmLegend,'Location','sw')
end


