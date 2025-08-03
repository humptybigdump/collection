%% KOSTRA-Daten
% J. Wienhöfer 2021

url = 'https://opendata.dwd.de/climate_environment/CDC/grids_germany/return_periods/precipitation/KOSTRA/KOSTRA_DWD_2010R/asc/';

% Inhalt des Verzeichnisses
webread(url, "ContentType", "table")     

% Datei für Dauerstufe 60 min laden und entpacken
zipfile = 'StatRR_KOSTRA-DWD-2010R_D0060.csv.zip';  
 unzip([url zipfile])  % current directory

% alle Rasterzellen für diese Dauerstufe einlesen
dat1 = readtable('StatRR_KOSTRA-DWD-2010R_D0060.csv');

% Rasterzelle für Weiherbach 49.1158°, 8.7405° -> aus KOSTRA-DWD-2010R_geog_Bezug.xlsx
  ID_WB = 80024;
  
% Datenreihe für WB extrahieren
wbstat =  dat1(find(dat1{:,1}==ID_WB),2:end);       
    % Zeilen -> Spalten
     wbstat = table(wbstat{:,:}.', 'RowNames',wbstat.Properties.VariableNames);
     wbstat.Properties.VariableNames = "D60min";
dauerstufen = [1, 2, 3, 5, 10, 20, 30, 50, 100];         

%% plotten
  bar( categorical(dauerstufen), wbstat.D60min, 'BarWidth', 0.3)
title("Jährlichkeiten für Niederschlag (1 h) Weiherbach" )
xlabel('Jährlichkeit {\itT_n}'); ylabel('{\itN}_{1 h}');


