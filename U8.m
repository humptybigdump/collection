clear all
close all
clc

%% a)
% Impedanzdaten einlesen

data_path = ''; % Pfad der Massdaten

% Speichern der Matdaten der .z Dateien im Datenordner.
data_info = dir(fullfile(data_path, '*.z')); % output: struct array

for i=1 : length([])
    %%%%%%%%%%%%Beispiel für die Verwendung regexp:
    numPos = regexp('Input12Test58', '\d'); % output: alle Indizes von natuerlichen Zahlen, also hier: 6 7 12 13 
    %%%%%%%%%%%%
    
    data_info(i).SOC = []; % erweitern der data_info um extrahierten SOC
    data_info(i).T = []; % erweitern der data_info um extrahierte Temperatur
    % Anlegen struct container fuer extrahierte Daten
    data{i} = readData([]);
end

% Impedanzen plotten

% benutzen Sie hier den Code vergangener Übungen
% Benutzen Sie zur automatischen Erstellung einer Legende die Informationen
% über SOC und T in data_info


%% b)
% ESB-Modell aufsetzen und parametrieren

f = []; % Nutzen Sie den Frequenzvektor einer Messreihe 
L = 1e-9;
R0 = 0.02;
RQ1_R = 0.02;
RQ1_n = 0.9;
RQ1_Q = 1e-4;
RQ2_R = 0.05;
RQ2_n = 0.9;
RQ2_Q = 0.02;
FLW_R = 0.06;
FLW_T = 200;
FLW_n = 0.5;
C = 700;

modelValues = []; % alle Parameter in ein Container
Z = calc_Z(modelValues,f);

% ploten der Modellimpedanz
%...
%Beipiel für das Hinzufügen eines Legendeneintrags zur bisherigen Legende:
        Legende{end+1} = 'Zusatzeintrag';
        legend(Legende);

%% c)
% führen Sie Aufgabenteil b) mit unterschiedlichen Parameterwerden aus

%% c) Bonus
% Startparameter und Boundaries definieren
startparameter = [L R0 RQ1_R RQ1_n RQ1_Q RQ2_R RQ2_n RQ2_Q FLW_R FLW_T FLW_n C];
lb =             [0 0  0     0.5   1e-5  0     0.4   1e-4  0     0     0     0];
ub =             [1 0.02 0.1 1     1e-2  1     1     1     100   1000  0.5   1000];

% Optionen setzen
options= optimset('MaxIter', 10e3 ,'TolX',1e-12, 'TolCon', 1e-20,...
                  'TolFun', 1e-12, 'MaxFunEvals', 1e12,...
                  'Algorithm', 'active-set', 'Display','final');
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Optimierung

    % Definiere eine zu minimierende Funktion als function handle
    % Hinweis: fhandle_name = @(Optimierungsparameter)guetemass(Messdaten, Optimierungsparamter, Frequenzvektor); 
    to_minimize =[]; % 
    % Optimierung der Modellparameter
    [resultParameters, fval, exitflag, output] = fmincon(to_minimize, startparameter,[],[],[],[],lb,ub,[],options);
    Modelparameter = resultParameters;
    
    % Modellimpedanz berechnnen und plotten (vlg Aufgabenteil b)
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Plotten der SOC-abhaengigen Parameter
% ...




