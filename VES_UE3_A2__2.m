% VES - Uebung 3 Aufgabe 4

% Zeitvektor
time = linspace(0.003,0.005,1000);

% Gleichung für alle Zeitpunkte lösen
P = exp(-2000*time).*(1+ 2000*time + (2000*time).^2/2);

% Schnitt der Funktion mit dem Wert 0.01
[~, index_min] = min(abs(P'-0.01));
time(index_min)

% Ergebnisse plotten
figure('Position',[200 200 600 350], 'WindowStyle','normal');
hold on;
plot([time(1),time(end)],[0.01,0.01],'r-');
plot(time,P,'b-');
plot(time(index_min),P(index_min),'ko');
xlabel('t in s')
ylabel('1 - P')

