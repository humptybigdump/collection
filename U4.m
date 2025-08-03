%% BMML Übung 4 
% Ein wichtiger Punkt zur Beschreibung einer Batterie sind die Lade- und
% Entlade-Kurven der Anode und Kathode. Hierzu werden die Anode/Kathode der
% Batterie entnommen und gegen ein Referenzpotentiel (z.B. Li) vermessen.
% Zur Vermeidung von Ventrikeln wird die Kathode z.B. dicker gewÃ¤hlt als
% die Anode damit das Graphit nicht Li-frei und dadurch instabil wird.

%% Rohdaten einlesen und DVA berechnen

cd 'Daten'
load('Anode_gefiltert.mat');
Anode.Q = Q; % Ah
Anode.I = I; % A
Anode.t = t; % s
Anode.U = U; % V
clear Q I t U
 
%...

cd .. % Gehe zurück zum übergeordneten Verzeichnis

% ploten der drei Kennlinien in ein gemeinsames Diagramm. Sie können
% möglichweise teile des Codes vergangener Übungen wiederverwenden

% Anpassung der Kurven aufeinander
Skalierungsfaktor_Anode = [];
Skalierungsfaktor_Kathode = [];


% Berechne Differenz der Spannung der Anode, Kathode
U_diff = []; % achten Sie darauf, dass Sie nur gleichlange Vektoren vonanander abziehen können. Sie können also nur den Bereich der Kennlinien voneinander abziehen, für denn alle Spannungsvektoren Werte besitzen.

% PLOT Rohdaten
% plotten Sie hier die Spannungsverläufe über der skalierten Kapazität
%% DVA

dvadata_anode = DVA_curve(Anode,Skalierungsfaktor_Anode);
dvadata_kathode = [];
dvadata_Vollzelle = [];

% PLOT DVA


%% Streckungs und Stauchungsfaktoren

Anode.Streckung = 1;
Anode.Verschiebung = 1;

Kathode.Streckung = 1;
Kathode.Verschiebung = 1;

% Stauchen/Strecken und Verschieben der Kennlinien
dvadata_anode.DVA_neu = [];
dvadata_kathode.DVA_neu = [];
Anode.Q_neu = [];
Kathode.Q_neu = [];

Kathode.U_neu = interp1(Kathode.Q_neu, Kathode.U, Vollzelle.Q,'linear',0); % Interpolation der veränderten Kennlinie auf eine gemeinsame Basis (Vollzelle.Q)
Anode.U_neu = [];
dvadata_kathode.DVA_interp = [];
dvadata_anode.DVA_interp = [];

% PLOT DVA aller Kurven
U_diff_neu = [];
dva_gesamt = [];
figure(4)
% plotten aller DVA-Kurven
subplot(2,1,1)

% Plotten aller Spannungskurven
subplot(2,1,2)

