function x = cramerRegel(A, b, method)

% Funktion:         cramerRegel
% Syntax:           cramerRegel(A,b,method)
% A:                Matrix der Widerstände/Leitwerte
% b:                je nach Methode Spannungen oder Ströme
% x:                Lösung des LGS, je nach Methode Maschenströme oder Knotenpotentiale
% method:           gibt Verfahren an 
% method = 1:       Maschenstromverfahren
% method = 2:       Knotenpunktpotentialverfahren
% method nicht 1,2: Falsche Eingabe
% Autor:            Fabian Köninger (LEN Tutor)


% Beispiel:
% Sei A =[ 5.4  3   0;
%          3    10  5;
%          0    5   6] in Ohm
%
% Sei b =[ -9;
%           0;
%          12] in Volt
%
% Lösung durch Maschenstromverfahren und Cramersche Regel: 
% 
% I = cramerRegel(A, b, 1);
%
% In I werden die einzelnen Maschenströme zurüchgegeben.



% Falls method einen Wert ungleich 1 und 2 hat, wird hier ein Fehler
% ausgegeben
if method ~= 1 && method ~= 2
    msg_err_1 = 'Falsche Eingabe. 1 = Maschenstromverfahren. 2 = Knotenpunktpotentialverfahren';
    error(msg_err_1)
end


% Initalisierung der relevanten Variablen wie: Spalten-/Zeilenanzahl,
% Determinate der Matrix A und die Determinaten, die für die Cramersche
% Regel wichtig sind
[m_A, n_A] = size(A);

A = single(A);

det_A = det(A);

x = zeros(m_A,1);
det_cram = zeros(m_A,1);


% Jeder Durchlauf dieser for-Schleife iteriert die einzelnen Schritte,
% spaltenweise durch

for i = 1:n_A
    
    % matrix_helper dient als Hilfsmatrix die jeden Schritt wie gewünscht,
    % verändert wird. Zu Beginn wird sie immer gleich A gesetzt
    matrix_helper = A;
    
    % Um die i-te Variable zu bestimmen wird in matrix_helper die i-te Spalte
    % durch den Vektor b ersetzt
    matrix_helper(1:end, i) = b;
    
    % Durch single() wird die Art der Variable auf single gestellt. Spart
    % gegenüber double Variablen Platz
    matrix_helper = single(matrix_helper);
    
    % Die Determinate der neuentstandenen Matrix wird in die i-te Stelle
    % des Vektors det_cram geschrieben
    % Dadurch ist es einfach die Determinaten für die veränderten Matrizen
    % abzurufen
    det_cram(i) = det(matrix_helper);
    
    % Hier wird die Cramersche Regel angewand. Die i-te Variable ist gleich
    % dem Quotienten aus der Determinante der Matrix, deren i-te Spalte
    % durch b ersetzt wurde und der Determinate der Ursprungsmatrix A.
    x(i) = det_cram(i)/det_A;
    
    
    % Hier werden die Ausgaben im Command Window ausgeführt. 
    % Es wird empfohlen, nach jedem Durchführen der Funktion, das Command
    % Window mit dem Befehl 'clc' zu leeren. Ansonsten könnte es zu
    % Problemen hinsichtlich der Übersichtlichkeit geben.
    disp('---------------------------------------------------------------------');
    msg_1 = 'Ursprungsmatrix A:';
    disp(msg_1);
    disp(num2str(A));
    msg_2 = ['Neue Matrix A_neu_',num2str(i),':'];
    disp(msg_2);
    disp(num2str(matrix_helper));
    msg_3 = ['Determinante von A = ', num2str(det_A)];
    msg_4 = ['Determinate von neuer Matrix A_neu_', num2str(i), '= ', num2str(det_cram(i))];
    disp(msg_3);
    disp(msg_4);
    
    % Je nach gewählter Methode werden hier die Ergebnisse bezüglich ihrer
    % korrekten Einheiten und Formelzeichen dargestellt
    % Sollte method nicht entweder 1 oder 2 sein, wird weiter oben schon
    % ein Fehler ausgegeben
    if method == 1
        msg_5 = ['Berechnung: Strom',num2str(i), ' = det_A_neu_',num2str(i),' / det_A = ', num2str(det_cram(i)), '/', num2str(det_A), ' A'];
        msg_6 = ['Strom', num2str(i), ' = ', num2str(x(i)),'A'];
    elseif method == 2
        msg_5 = ['Berechnung: Potential',num2str(i), ' = det A_neu_',num2str(i),' / det_A = ', num2str(det_cram(i)), '/', num2str(det_A), ' V'];
        msg_6 = ['Potential', num2str(i), ' = ', num2str(x(i)),'V'];
    end
    
    disp(msg_5);
    disp(msg_6);
    
    % Durch den Befehl 'pause' wird sichergestellt, dass jede Itteration
    % einzeln im Command Window nacheinander ausgegben wird. Nach jeder
    % Itteration ist ein Tastendruck notwendig um die Ergebnisse der
    % nächsten Itteration darzustellen. Dies dient der Übersichtlichkeit.
    % Außerdem wird im letzten Schritt der for-Schleife nicht noch einmal
    % ein Tastendruck abgefragt, da dieser zu diesem Zeitpunkt nichtmehr notwendig ist.
    % Alle Ergebnisse wurden schon dargestellt
    if i ~= n_A
        pause
    end
    
end

end

