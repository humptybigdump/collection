%%% UE VII Aufgabe 6 %%%

% Bootstrap + Jackknife

% 6a) 
% Varianz des empirischen Medians

%% L�sung f�r �bungsblatt

% BOOTSTRAP
% Matrix der Bootstrap-Stichproben
A=[1 1 1; 2 1 1; 5 1 2; 1 1 1; 2 1 1; 5 2 2; 2 2 5; 1 5 2; 5 5 1; 1 5 1];

% empirische Mediane pro Bootstrap-Stichprobe
M=median(A,2);

% Varianz des Median-Sch�tzers
v_hat1=var(M) %(korrigierte Varianz)

% JACKKNIFE
%jackstat=jackknife(@median,[1 2 5])

%% L�sung f�r Bootstrap allgemein

% Matrix der Mediane von 10 m�glichen Bootstrap-Stichproben:
bootstat=bootstrp(1000,@median,[1 2 5]);
% Bemerkung: Beim Bootstrapping ist das Ziehen jedes Stichprobenelements gleichwahscheinlich,
% daher k�nnen auch Bootstrap-Stichproben gezogen werden, die gleich sind.

% Sch�tzung der Varianz des Mediansch�tzers
v_hat=var(bootstat)

% Je nach zuf�llig gezogenem Bootstrap-Sample variiert die Varianzsch�tzung
% f�r den Mediansch�tzer. Eine Erh�hung des Umfangs des Bootstrap-Samples
% liefert robustere Ergebnisse.

% 6b)
% empirische Verteilung und Perzentile des Medians (Matlab-Daten)
% f�r n=1000 gilt
boot_sort=sort(bootstat);
T_1=boot_sort(25,:)  % untere Grenze des KI
T_2=boot_sort(975,:) % obere Grenze des KI