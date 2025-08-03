%% Matlab script zu Geometrische Modelle Übungsblatt 4 Matlab-Aufgabe

clear variables;
close all;
clc;

%% Matrizen und Vektoren erstellen

% Matrix A
A = randi([1 20], 20, 20);
disp("Rang(A) =  " + rank(A));

% Vektor b
b = randi([1 20],20,1);

%% QR-Zerlegung
epsilon = 0.0001;
[Q,R] = qr(A);

% Q*R = A ?
equal = 1;
for i = 1:A(:,1)
    for j = 1:A(1,:)
      if(A < Q*R-epsilon || A > Q*R +epsilon)
          equal = 0;
      end
    end
end

if equal==1
    disp('A == Q*R. Die QR-Zerlegung hat funktioniert');
else
    disp('A =/= Q*R. QR-Zerlegung hat nicht funktioniert');
end

%% LGS mithilfe QR-Zerlegung lösen

x_qr = linsolve(R,Q'*b);
y_qr = linsolve(Q,x_qr);


% Test
x_normal = linsolve(A,b);

if(y_qr < x_normal-epsilon || y_qr > x_normal+epsilon)
    disp('QR-Zerlegung liefert korrektes Ergebnis');
else
    disp('QR-Zerlegung liefert nicht das Ergebnis von A x = b');
end
