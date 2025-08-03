function dn = DGL(x, n, T, p, R, k)
% Partialdrücke berechnen (bar!)
p_i = p * n / sum(n);

% Ausdruck für Gleichgewichtskonstante
Keq=exp(4577.8/T - 4.33);
Beta=p_i(3)*p_i(4)/(Keq*p_i(1)*p_i(2));

% Berechnung der Reaktionsgeschwindigkeit
r= k(5)*exp(-k(6)/(R*T)) * ...
    p_i(1)^k(1)*p_i(2)^k(2)*p_i(3)^k(3)*p_i(4)^k(4) * ...
    (1-Beta);

% Änderungsraten berechnen
d_H2O = -r;
d_CO  = -r;
d_H2  =  r;
d_CO2 =  r;

% Rückgabevektor Zp
dn=[d_CO; d_H2O; d_H2;  d_CO2];

end

