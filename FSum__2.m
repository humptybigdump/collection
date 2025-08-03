
% Funktion zur Bestimmung der Fehler zwischen Modell und Messdaten
function S = FSum(P,T_mess, Eta_mess, R)

% Fehler berechnen
S = (FEta(P,T_mess,R) - Eta_mess)./Eta_mess;

% Messdaten und Optimierungsfortschritt plotten
plot(T_mess, Eta_mess, 'ro')
hold on
TempBer = 0:0.1:100;
EtaBer = FEta(P, TempBer, R);
plot(TempBer,EtaBer,'b-')
xlabel('Temperatur T in °C')
ylabel('Viskosität \it{\eta} in mPa*s')
title('Viskosität von Wasser in Abhängigkeit der Temperatur')
hold off
drawnow

end

