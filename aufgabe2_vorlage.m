clear all; close all; clc; 
%%--------------------------------------------------------
% Lösung zur Studienarbeit Teil 2 - Aufgabe 2)
%
% Vorlage zur Entwicklung der Biegefläche und Rückrechnung
% der Schnittgrößenverläufe
%
% Ergänzt werden müssen die Koeffizientematrix K und der 
% Vektor der rechten Seite F des LGS K*A = F, siehe unten
% 
% Für Konvergenzuntersuchungen kann die Anzahl der Reihen-
% glieder noch varriiert werden
%
% Im einfachsten Fall können durch Anklicken der Knoten der
% Verläufe die Min/Max Werte rausgelesen werden
%
% Starten des Scripts durch F5 oder oben grüner Knopf "Run"
% im Editor
%
% Für Profis: Rückrechnung der Querkräfte fehlt noch und 
% kann bei Interesse ergänzt werden. 
%
%%--------------------------------------------------------
%% Eingabe der Konstanten
E = 100000;
q = 1;
nu= 0;
a = 10;
b = 5;
d = 0.2;

%---------------------------------------------------------------------------------------------------
%---------------------------------- Anpassungsbereich ----------------------------------------------
%---------------------------------------------------------------------------------------------------
% Hier kann man sich durch Anpassen der Anzahl der Reihenglieder die Konvergenzeigenschaften der
% Lösung anschauen

N = 5; % Anzahl Reihenglieder
%---------------------------------------------------------------------------------------------------
%---------------------------------- Anpassungsbereich ----------------------------------------------
%---------------------------------------------------------------------------------------------------

% Berechnung Konstanten 
B = E*d^3/(12*(1-nu^2));

%% Reihenentwicklung
% Vorgabe Variablen
x = 0:0.5:a;
y = 0:0.5:b;

% Initialisierung Biegefläche & Ableitungen
wf = zeros(size(x,2),size(y,2));
wfyy = zeros(size(x,2),size(y,2));
wfxx = zeros(size(x,2),size(y,2));
wfxy = zeros(size(x,2),size(y,2));

% Summe über Reihenglieder
for n = 1:N
    %-----------------------------------------------------------------------------------------------
    %---------------------------------- Anpassungsbereich ------------------------------------------
    %-----------------------------------------------------------------------------------------------
    % Konstanten für LGS -> Sollte so übernommen werden können!
    alpha = pi*n/a;
    cab   = cosh(alpha*b);
    sab   = sinh(alpha*b);
    pn    = 2*q/(pi*n)*(1-cos(pi*n));
    
    % Rechte Seite LGS
%     F = [ F1 ; 
%           F2 ; 
%           F3 ;
%           F4 ];
    
    % Koeffizientenmatrix
%     K = [ k11        k12          k13         k14;
%           k21        k22          k23         k24;
%           k31        k32          k33         k34;
%           k41        k42          k43         k44];

    %-----------------------------------------------------------------------------------------------
    %---------------------------------- Anpassungsbereich ------------------------------------------
    %----------------------------------------------------------------------------------------------
    % Lösen des LGS
    A = K\F;

    % Partikulärlösung wpn Auswerten
    wnp = pn/(alpha^4*B);

    % Homogene Lösung Auswerten
    cay = cosh(alpha*y);
    say = sinh(alpha*y);
    wnh = A(1,1)*cay + A(2,1)*alpha*y.*say + A(3,1)*say + A(4,1)*alpha*y.*cay;
    wny = wnh + wnp;

    % Gesamte Lösung Biegefläche
    wf   = wf + sin(alpha*x)'*wny;

    % Berechnung der Ableitungen von wny
    wny_y  = A(1,1)*alpha*say + A(2,1)*alpha*(say+alpha*y.*cay) + A(3,1)*alpha*cay + A(4,1)*alpha*...
             (cay+alpha*y.*say);
    wny_yy = A(1,1)*alpha^2*cay + A(2,1)*alpha.^2*(2*cay+alpha*y.*say) + A(3,1)*alpha^2*say + A(4,1)...
           *alpha^2*(2*say+alpha*y.*cay);

    % Ableitungen Biegefläche gesamt
    wfyy = wfyy + sin(alpha*x)'*wny_yy;
    wfxx = wfxx + -alpha^2*sin(alpha*x)'*wny;
    wfxy = wfxy + alpha*cos(alpha*x)'*wny_y;
end

% Berechnug Momente
mx  = -B*(wfxx + nu*wfyy);
my  = -B*(wfyy + nu*wfxx);
mxy = -B*(wfxy*(1-nu));

% Plot
[ypl,xpl] = meshgrid(y,x);

f=figure;
f.Position = [100 100 1000 800];
movegui(f,'center');

% Biegefläche
subplot(2,2,1)
surf(ypl,xpl,-wf)
xlabel('y')
ylabel('x')
zlabel('w')
title('Biegefläche w')
axis equal

% Moment mx
subplot(2,2,2)
surf(ypl,xpl,mx)
xlabel('y')
ylabel('x')
zlabel('mx')
title('Biegemoment mx')

% Moment my
subplot(2,2,3)
surf(ypl,xpl,my)
xlabel('y')
ylabel('x')
zlabel('my')
title('Biegemoment my')

% Moment mxy
subplot(2,2,4)
surf(ypl,xpl,mxy)
xlabel('y')
ylabel('x')
zlabel('mxy')
title('Drillmoment mxy')





























