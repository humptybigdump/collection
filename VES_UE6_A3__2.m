% VES - Uebung 6 Aufgabe 3

clc
clear variables

%% Adjazenzmatrix anlegen
adj_org = zeros(29,29);
adj_org(4,1) = 2;
adj_org(18,2) = 1;
adj_org(1,3) = 3;
adj_org(21,4) = 2;
adj_org(22,4) = 2;
adj_org(26,6) = 1;
adj_org(29,6) = 1;
adj_org(29,7) = 3;
adj_org(19,8) = 2;
adj_org(16,9) = 3;
adj_org(22,9) = 1;
adj_org(23,11) = 2;
adj_org(11,12) = 2;
adj_org(18,12) = 3;
adj_org(26,12) = 3;
adj_org(14,13) = 2;
adj_org(17,13) = 3;
adj_org(16,15) = 1;
adj_org(1,16) = 1;
adj_org(11,17) = 2;
adj_org(25,17) = 1;
adj_org(14,22) = 2;
adj_org(16,23) = 2;
adj_org(20,28) = 3;
adj_org(13,29) = 1;
adj = adj_org';
adj(adj==0) = -inf;


%% Adjazenzmatrix als Graph plotten
names = {'1' '2' '3' '4' '5' '6' '7' '8' '9' '10' '11' '12' '13' '14' '15'...
    '16' '17' '18' '19' '20' '21' '22' '23' '24' '25' '26' '27' '28' '29'};
G = digraph(adj_org,names);
plot(G,'Layout','circle');
axis square;


%% maxplusmatrix-Objekt erstellen
adj_maxplus = maxplusmatrix(adj);


%% Initialisierung

% A^0 berechnen und speichern
A_new = mpower(adj_maxplus,0);
A_sum = cell(size(adj,1),1);
A_sum{1} = A_new;

% Hilfsvariablen
break_condition = false;
index = 0;


%% Schleifenfreiheit -> maximal 28 Schritte erforderlich
while ~break_condition && index <= size(adj,1)

    % A^index berechnen
    index = index + 1;
    A_old = A_new;
    A_new = mtimes(A_old,adj_maxplus);

    % wenn A^index gleich A^(index-1): Schleifenfreiheit, max. Anzahl
    % Schritte durch den Graph ist erreicht
    if isequal(A_new,A_old)
        break_condition = true;
        disp(['Schritt: ', num2str(index)]);
        break;
    end

    % A^* bis A^index berechnen und speichern
    A_sum{index+1} = plus(A_new,A_sum{index});
end


%% Laufzeit zwischen Knoten 14 und 7
disp(['Laufzeit Knoten 14 zu Knoten 7: ', num2str(A_sum{index}(7,14))])

%% Maximale Laufzeit von Knoten 14
disp(['Maximale Laufzeit von Knoten 14: ', num2str(max(double(A_sum{index}(:,14))))]);

%% plot Erreichbarkeit
adj_new_plot = A_sum{index}';
adj_new_plot(adj_new_plot == -inf) = 0;
G = digraph(double(adj_new_plot),names);
figure;
plot(G,'Layout','circle');
axis square;
