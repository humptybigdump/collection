% MATLAB-Übung zum Profilfach "Auslegung und Bilanzierung von Mikro-
% strukturreaktoren
% Übung 2 - Aufgabe 1b)
% Funktion zur Berechnung des Wachstumskoeffizienten nu von Zellen nach Han
% und Levenspiel
% Eingangsparameter:
% N: Nährstoffkomzentration als Vektor [-]
% P: Produktkonzentration [-]
% Konstanten: k_0, nu_0, k_N1, k_P1 [-]

function mu = f_U2_a1(N, P, k_0, mu_0, k_N1, k_P1)

    mu = mu_0*(N./(k_0+N+k_N1*N.^2)) * (1-k_P1* P)^2;
    
end