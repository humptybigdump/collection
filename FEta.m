
% Funktion Berechnung von Eta
function y = FEta(P,T,R)

y = P(1)*exp(P(2)./(R*(T+273.15)));

end


