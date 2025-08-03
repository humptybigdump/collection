%% Übung 1.5 Wasserbilanz und Doppelsummenkurven

%% Daten einlesen und vorbereiten
% Daten liegen im aktuellen Arbeitsverzeichnis von Matlab
% mit eigener Funktion (springt über Header, formatiert Datum)
obst = readWasserbilanzdat('1.5_Wasserbilanz_OBST.csv');
bech = readWasserbilanzdat('1.5_Wasserbilanz_BECH.csv');

% EZG-Flaechen (stehen als km² im Header)
A_obst = 81 * 1e6 ;       % m²
A_bech = 93.4 * 1e6  ;   % m²

% Abfluss von m³/s in mm/h umrechnen, neue Spalte in Tabelle
obst.Q_mm = obst.Q*3600*1000/A_obst ;
bech.Q_mm = bech.Q*3600*1000/A_bech ;


%% Kumulierte Wasserbilanz über ganzen Zeitraum (6 Jahre) zeigen
 subplot(1,2,1);
    plot(obst.Datum, cumsum(obst.N))
    hold on
    plot(obst.Datum, cumsum(obst.Q_mm))
    hold off
    title('Oberstdorf/Stillach')
    xlabel('Datum') 
    ylabel('mm') 
    legend('Niederschlag', 'Abfluss', 'Location','northwest')

  subplot(1,2,2);
    plot(bech.Datum, cumsum(bech.N))
    hold on
    plot(bech.Datum, cumsum(bech.Q_mm))
    hold off
    title('Bechhofen/Wieseth')
    xlabel('Datum') 
    ylabel('mm') 
    legend('Niederschlag', 'Abfluss', 'Location','northwest')
    
%% Doppelsummenkurven für einzelne Jahre bestimmen

    % Wasserwirtschaftsjahre
    obst.wwj = year(obst.Datum);         % Jahr als Zahl
    bech.wwj = year(bech.Datum);         
    % November und Dezember gehören zum nächsten Jahr
    %       -> das Jahr für diese Einträge wird eins weitergezählt
    %       der Index ist für beide Stationen gleich
    novdez_ind = ismember(month(obst.Datum),[11 12]);
    obst.wwj( novdez_ind ) = obst.wwj( novdez_ind )+1;
    bech.wwj( novdez_ind ) = bech.wwj( novdez_ind )+1;
    
    %% Doppelsummenkurve
    % Kumulieren der Niederschläge und Abflüsse
    % normieren mit Jahresniederschlag
    % jeweils pro WWJ - hier wieder für beide gleich   
    
    % Gruppieren der Daten nach WWJ
    [G, wwjs] = findgroups(obst.wwj);

    % Überprüfen, ob beide Reihen gleich lang sind
          if(G ~= findgroups(bech.wwj)) 
              warning("Data not of same length");
          end
      
    % Jahresniederschläge je WWJ
    Ntot_obst = splitapply(@sum, obst.N, G); 
     Ntot_bech = splitapply(@sum, bech.N, G);
    
    % kumulierte Reihen je WWJ
    Nnorm_obst = splitapply(@(x1) {cumsum(x1)}, obst.N, G);
     Qnorm_obst = splitapply(@(x1) {cumsum(x1)}, obst.Q_mm, G);
    Nnorm_bech = splitapply(@(x1) {cumsum(x1)}, bech.N, G);
     Qnorm_bech = splitapply(@(x1) {cumsum(x1)}, bech.Q_mm, G);    

    % normieren: jede Reihe durch jeweiligen Jahresniederschlag teilen
    for jj = 1:length( wwjs)
      Nnorm_obst{jj} = Nnorm_obst{jj}/Ntot_obst(jj);
        Qnorm_obst{jj} = Qnorm_obst{jj}/Ntot_obst(jj);
      Nnorm_bech{jj} = Nnorm_bech{jj}/Ntot_bech(jj);
        Qnorm_bech{jj} = Qnorm_bech{jj}/Ntot_bech(jj);
    end
    
   %% Grafiken
  
  figure       %% macht ggf ein neues Grafikfenster auf
  subplot(1,2,1);
    hold on
    for jj = 1:length( Ntot_obst)
        plot(Nnorm_obst{jj},Qnorm_obst{jj})
    end
    hold off
    
    ylim([0 1]);
    title('Oberstdorf/Stillach')
    legend(string(wwjs),'Location','northwest')
    xlabel('N_{kumuliert} / N_{tot}') 
    ylabel('Q_{kumuliert} / N_{tot}') 
 
  
  subplot(1,2,2);
    hold on
    for jj = 1:length( Ntot_bech)
        plot(Nnorm_bech{jj},Qnorm_bech{jj})
    end
    hold off
    
    ylim([0 1]);
    title('Bechhofen/Wieseth') 
    legend(string(wwjs),'Location','northwest')
    xlabel('N_{kumuliert} / N_{tot}') 
    ylabel('Q_{kumuliert} / N_{tot}') 
 
  % Ende 
    
    