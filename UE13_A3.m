%%% UE VII Aufgabe 6 %%%

% Bootstrap + Jackknife

% 6a) 
% Varianz des empirischen Medians

%% Lösung für Übungsblatt

% BOOTSTRAP
% Matrix der Bootstrap-Stichproben
A=[1 1 1; 2 1 1; 5 1 2; 1 1 1; 2 1 1; 5 2 2; 2 2 5; 1 5 2; 5 5 1; 1 5 1];

% empirische Mediane pro Bootstrap-Stichprobe
M=median(A,2);

% Varianz des Median-Schätzers
v_hat1=var(M) %(korrigierte Varianz)

% JACKKNIFE
%jackstat=jackknife(@median,[1 2 5])

%% Lösung für Bootstrap allgemein

% Matrix der Mediane von 10 möglichen Bootstrap-Stichproben:
bootstat=bootstrp(1000,@median,[1 2 5]);
% Bemerkung: Beim Bootstrapping ist das Ziehen jedes Stichprobenelements gleichwahscheinlich,
% daher können auch Bootstrap-Stichproben gezogen werden, die gleich sind.

% Schätzung der Varianz des Medianschätzers
v_hat=var(bootstat)

% Je nach zufällig gezogenem Bootstrap-Sample variiert die Varianzschätzung
% für den Medianschätzer. Eine Erhöhung des Umfangs des Bootstrap-Samples
% liefert robustere Ergebnisse.

% 6b)
% empirische Verteilung und Perzentile des Medians (Matlab-Daten)
% für n=1000 gilt
boot_sort=sort(bootstat);
T_1=boot_sort(25,:)  % untere Grenze des KI
T_2=boot_sort(975,:) % obere Grenze des KI