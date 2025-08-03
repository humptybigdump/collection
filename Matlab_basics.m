%%%% MATLAB - kurze Einführung %%%%

% Auskommentieren von Codes mit "%"
% Erstellen von individuell kompilierbaren Sektionen mit "%%" (Befehl: Run Section)

% clear % löscht alle Variablen aus dem Workspace
% clc   % löscht alle Einträge im Command Window (Historie bleibt jedoch erhalten)

%% Erzeugung von Vektoren und Matrizen (allgmein)

a=[1;2;3] % erzeugt einen Spaltenvektor
b=[1 2 3] % erzeugt einen Zeilenvektor
A=[1 2 3;4 5 6] % erzeugt 2x3-Matrix
B=[7 8;9 10;11 12] % erzeugt 3x2-Matrix

%% Erzeugung spezieller Vektoren und Matrizen

C=ones(2, 4)  % erzeugt 2x4-Matrix mit 1en
D=zeros(2, 4) % erzeugt 2x4-Matrix mit 0en
E=rand(2, 4)  % erzeugt 2x4-Matrix gleichverteilter Zufallszahlen aus dem Intervall (0,1)
              % (hilfreich für die Simulation verschiedener Verteilungen)
F=randi(11, 2, 4) % erzeugt 2x4-Matrix ganzzahliger Zufallszahlen aus dem Intervall (0,11)

% MATLAB bietet noch weitere Befehle für die Generierung von Zufallszahlen
% aus speziellen Verteilungen: normrnd(mu,sigma), binornd(N,P), exprnd(mu),
% poissrnd(lambda), trnd(nu)...

%% Rechnen mit MATLAB

G=A' % transponiert Matrix A
J=inv(randi(11,3)) % berechnet Inverse einer 3x3-Matrix
I=A'*A 
scalar=a'*a        % Skalarprodukt eines Vektor
norm_a=norm(a)     % Norm eines Vektors
H=A*B
K=A-B'
L=randi(100,3)*rand(3)
M=inv(L)*a

R_had=randi(100,4).*randi(5,4) % elementweise Multiplikation (Hadamard-Produkt)
R=L.^2             % quadriert jedes einzelne Element der Matrix
S=a.\2             % dividiert jedes einzelne Element der Matrix durch 2

det_L=det(L)       % Determinante einer Matrix
trace_L=trace(L)   % Spur einer Matrix
diag_L=diag(L)     % gibt Vektor mit Diagonalelementen der Matrix L
elements=numel(L)  % gibt die Anzahl von Elementen in der Matrix L
[V e]=eig(L)       % Spalten von V sind die Eigenvektoren zu den Eigenwerten gegeben in Matrix e
%% Zerlegung von Matrizen

O=L(1, 3)      % wählt das Element aus Zeile 1, Spalte 3
P=L(2:3, 1:2)  % wählt die Teilmatrix Zeile 2 und 3, Spalten 1 und 2
Q=L(1:end, 1)  % wählt komplette 1. Spalte der Matrix
R=L(:, 1)      % wählt komplette 1. Spalte der Matrix
U=L([1 3], :)  % wählt 1. und 3. Zeile der gesamten Matrix 

%% Beispiele für einfache Schleifen

% Simples Beispiel: Addition einer Zahl zu einem Vektor
g=(1:10)'  % generiert Spaltenvektor mit Werten von 1 bis 10 (geordnet)
% Einfache Berechnung
t=g+1
% Umständliche Berechnung mit Hilfe einer for-Schleife
t=zeros(10, 1) % Initialisierung des Ergebnisvektors
for i=1:10;
    t(i,1)=g(i,1)+1;
end
t

% Erzeugung einer n-dimensionalen Einheitsmatrix mit Hilfe einer for-Schleife
n=5;
T=zeros(n)
for j=1:n;
    T(j,j)=T(j,j)+1;
end
T

% Aufaddieren aller ganzen Zahlen bis 30 mit einer while-Schleife
f=0;
k=1;
while k <= 30;
    f=f+k;
    k=k+1;
end
f