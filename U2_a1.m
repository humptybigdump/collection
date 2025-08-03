% MATLAB-Übung zum Profilfach "Auslegung und Bilanzierung von Mikro-
% strukturreaktoren
% Übung 2 - Aufgabe 1 

%a)
% Deklaration der Konstanten
k_0 = 1;
mu_0 = 10;
k_N1 = 2;
k_P1 = 3;

%c)
% Deklaration der Vektoren von N und P
N = 0:0.01:10;
P = 0:0.05:0.2;

% for-Schleife für alle Werte von P
for i=1:length(P)
    
    %Berechnung der Wachstumskoeffizienten für P(i)
    mu = f_U2_a1(N, P(i), k_0, mu_0, k_N1, k_P1);
    
    %Generiere Sublots
    hold on
    subplot(1,3,1)
    plot(N, mu)
    
    
    %d)
    % x-Achsenbeschriftung
    xlabel('Nährstoffkonz. N')
    % y-Achsenbeschriftung
    ylabel('Wachstumskoeff. \mu')
    % Titel
    title('Wachstumgesetz \mu(N)');
    % Beschriftung der Kurve
    text(N(round(length(N)/2)), mu(round(length(N)/2)), ['P= ', num2str(P(i))])
    
    %e)
    %Erzeuge function hadle von -1 * f_U2_a1
    f = @(N)f_U2_a1(N,P(i),k_0, mu_0, k_N1, k_P1)*(-1);
    %Finde Maximum für mu(N,P)
    x = fminbnd(f,N(1), N(end));
    nu_max = f_U2_a1(x,P(i),k_0, mu_0, k_N1, k_P1);
    
    %f)
    % Erstelle Diagramm für mu_max(P)
    hold on
    subplot(1,3,2)
    plot(P(i), nu_max, 'k*')
    xlabel('Produktkonz. P')
    ylabel('Max. Wachstumskoeffizient \mu_{max}')
    title('\mu_{max}(P)')
    
    %g)
    % Berechne Integral
    I = integral(@(N)f_U2_a1(N,P(i),k_0, mu_0, k_N1, k_P1),N(1), N(end));
    
    % Erstelle Diagramm für I(P)
    hold on
    subplot(1,3,3)
    plot(P(i), I, 'k*')
    xlabel('Produktkonz. P')
    ylabel('Integral I')
    title('I(P)')
end
    



