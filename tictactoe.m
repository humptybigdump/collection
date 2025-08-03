% Programm für das TicTacToe Spiel
% Dies ist nur eine! Lösungsmöglichkeit
% Jerono, Mai 2024
clear
close all

% Initialisieren des 3x3 Spielfeldes
row = 3;
col = 3;
% Ausgabe des Spielfeldes
Spielfeld = zeros(row,col)

% Maximale Anzahl an Spielzügen (3x3 grid mit 2 Eingaben pro Iteration)
n_max = 5;

% Schleife über die Spielzüge
for i = 1:1:n_max

    % Eingabe von Spieler 1
    sp = input(['Spieler 1: Geben Sie ein Element '...
        'zwischen [1,1] und [3,3] ein\n']);

    % Überprüfen der Gültigkeit der Eingabe von Spieler 1
    sp1_valid = is_valid(Spielfeld,1,sp);

    % Ausgabe des Spielfeldes nach gültiger Eingabe von Spieler 1
    Spielfeld(sp1_valid(1),sp1_valid(2)) = 1
    
    % Überprüfen ob Sieg für Spieler 1 vorliegt
    win1 = check_win(Spielfeld,1,sp1_valid);
    if win1 == 1
        fprintf('Spieler 1 hat gewonnen!\n')
        break;
    % Überprüfen ob Spielfeld ausgefüllt ist
    elseif i == 5
        fprintf('Unentschieden!\n')
        break;
    end

    % Eingabe von Spieler 2
    sp = input(['Spieler 2: Geben Sie ein Element '...
        'zwischen [1,1] und [3,3] ein\n']);
     % Überprüfen der Gültigkeit der Eingabe von Spieler 2
    sp2_valid = is_valid(Spielfeld,2,sp);

    % Ausgabe des Spielfeldes nach gültiger Eingabe von Spieler 2
    Spielfeld(sp2_valid(1),sp2_valid(2)) = 2
    
    % Überprüfen ob Sieg für Spieler 2 vorliegt
    win2 = check_win(Spielfeld,2,sp2_valid);
    if win2 == 1
        fprintf('Spieler 2 hat gewonnen!\n')
        break;
    end
end

function win = check_win(Spielfeld,Spieler,sp)
% Input:
% - Spielfeld : aktuelle Spielfeld
% - Spieler   : aktueller Spieler
% - sp        : letzte Spielereingabe 
% Output:
% win = true oder win = false

% Überprüfen ob Sieg für Spieler vorliegt
% Überprüfen der Zeilen
win_row = find(Spielfeld(sp(1),:)==Spieler);
% Überprüfen der Spalten
win_col = find(Spielfeld(:,sp(2))==Spieler);
% Überprüfen der Diagonale
d1_col = find(diag(Spielfeld)==Spieler);
% Überprüfen der Gegendiagonale
d2_col = find(diag(flipud(Spielfeld))==Spieler);

% Abfragen der Gewinnerbedingung
  if length(win_row) == 3 || length(win_col) == 3 ||...
     length(d1_col) == 3 || length(d2_col) == 3
     % Setzen der Ausgabe
     win = true;
  else
     win = false;
  end
end

function sp_valid = is_valid(Spielfeld,Spieler,sp)
% Input:
% - Spielfeld : aktuelle Spielfeld
% - Spieler   : aktueller Spieler
% - sp        : letzte Spielereingabe 
% Output:
% sp_valid    : gültige Eingabe des aktuellen Spielers
    
% Überprüfen der Gültigkeit der Eingabe des Spielers
while length(sp) ~= 2 || sp(2) > 3 || sp(2) < 1 || ...
            sp(1) > 3 || sp(1) < 1 || Spielfeld(sp(1),sp(2)) ~= 0
   
   % Fehlermeldungen für die entsprechenden Fälle
   % Überprüfen der Länge des Arrays (Eingabesyntax)
   if length(sp) ~= 2
       % Fehlermeldung
       fprintf(['Die Eingabe von Spieler %d ist kein Array'...
       ' in eckigen Klammern mit zwei Argumenten\n'],Spieler)
       % Fordern einer neuen Eingabe
       sp = input(['Spieler ',num2str(Spieler),': Geben Sie ein' ...
       ' Element zwischen [1,1] und [3,3] ein\n']); 
   else 
       % Eingabe kein Element zwischen [1,1] und [3,3]
       if sp(2) > 3 || sp(2) < 1 || sp(1) > 3 || sp(1) < 1
          % Fehlermeldung
          fprintf(['Die Eingabe [%d,%d] von Spieler %d ist keine'...
          ' gültige Eingabe\n'],sp(1),sp(2),Spieler)
          % Fordern einer neuen Eingabe
          sp = input(['Spieler ',num2str(Spieler),': Geben Sie ein' ...
          ' Element zwischen [1,1] und [3,3] ein\n']); 
       % Feld ist bereits belegt
       elseif Spielfeld(sp(1),sp(2)) ~= 0
          % Fehlermeldung
          fprintf(['Das gewählte Feld [%d,%d] ist bereits'...
          ' belegt\n'],sp(1),sp(2))
          % Fordern einer neuen Eingabe
          sp = input(['Spieler ',num2str(Spieler),': Geben Sie ein' ...
          ' Element zwischen [1,1] und [3,3] ein\n']); 
       % Unbekannter Fehlerfall
       else 
          % Fehlermeldung
          fprintf(['Die Eingabe von Spieler %d führt auf einen'...
          ' unbekannten Fehler\n'],Spieler)            
       end
   end
end
% Gültige Eingabe des Spielers erhalten
sp_valid = [sp(1),sp(2)];
end