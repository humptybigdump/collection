%% BMML Übung 4 Lösungsskript
%
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

load('Kathode_gefiltert.mat');
Kathode.I = I; % Ah
Kathode.t = t; % A
Kathode.U = U; % s
Kathode.Q = Q; % V
clear Q I t U

load('Vollzelle_gefiltert.mat');
Vollzelle.Q = Q; % Ah
Vollzelle.I = I; % A
Vollzelle.t = t; % s
Vollzelle.U = U; % V
clear Q I t U

cd .. % Gehe zurÃ¼ck zum Ã¼ber geordneten Verzeichnis
figure(1)
plot(Vollzelle.t, Vollzelle.U);
hold on
plot(Kathode.t, Kathode.U, '--');
plot(Anode.t, Anode.U, '-.');
hold off
title('OCV Rohdaten OHNE Skalierung')
xlabel('Zeit t / s')
ylabel('Spannung U / V')
legend('Vollzelle','Kathode','Anode')

% Anpassung der Kurven aufeinander
%Skalierungsfaktor_Anode = max(Vollzelle.Q) ./ max(Anode.Q);
%Skalierungsfaktor_Kathode = max(Vollzelle.Q) ./ max(Kathode.Q);
%Alternativ über die Fläche
Skalierungsfaktor_Anode = 890/2.54;
Skalierungsfaktor_Kathode = 890/2.54;


% Berechne Differenz der Spannung der Anode, Kathode
max_length = min(length(Anode.U), length(Kathode.U));
U_diff = Kathode.U(1:max_length) - Anode.U(1:max_length);

% PLOT Rohdaten
figure(2)
plot(Vollzelle.Q, Vollzelle.U); % As auf mAh
hold on
plot(Kathode.Q.*Skalierungsfaktor_Kathode, Kathode.U, '--');
plot(Anode.Q.*Skalierungsfaktor_Anode, Anode.U, '-.');
plot(Anode.Q(1:max_length).*Skalierungsfaktor_Anode, U_diff,'-');
title('OCV Rohdaten MIT Skalierung')
xlabel('Ladung Q / Ah')
ylabel('Spannung U / V')
legend('Vollzelle','Kathode','Anode','Differenz')
%% DVA

dvadata_anode = DVA_curve(Anode,Skalierungsfaktor_Anode);
dvadata_kathode = DVA_curve(Kathode,Skalierungsfaktor_Kathode);
dvadata_Vollzelle = DVA_curve(Vollzelle,1);

% PLOT DVA
figure(3)
plot(dvadata_anode.Q, -dvadata_anode.DVA);
hold on
plot(dvadata_kathode.Q, dvadata_kathode.DVA);
plot(dvadata_Vollzelle.Q, dvadata_Vollzelle.DVA);
hold off
ylim([-2 1]);
title('DVA')
xlabel('Ladung Q  /  Ah')
ylabel('DVA  /  Ah / V')
legend('Anode','Kathode','Vollzelle');
%% Streckungs und Stauchungsfaktoren

Anode.Streckung = 1.1;
Anode.Verschiebung = 0.2;

Kathode.Streckung = 1.015;
Kathode.Verschiebung = .05;

dvadata_anode.DVA_neu = dvadata_anode.DVA./Anode.Streckung;
dvadata_kathode.DVA_neu = dvadata_kathode.DVA./Kathode.Streckung;
Anode.Q_neu = Anode.Q .* Anode.Streckung .* Skalierungsfaktor_Anode - Anode.Verschiebung;
Kathode.Q_neu = Kathode.Q .* Kathode.Streckung .* Skalierungsfaktor_Kathode - Kathode.Verschiebung;

Kathode.U_neu = interp1(Kathode.Q_neu, Kathode.U, Vollzelle.Q,'linear',0);
Anode.U_neu = interp1(Anode.Q_neu, Anode.U, Vollzelle.Q,'linear',0);
dvadata_kathode.DVA_interp = interp1(Kathode.Q_neu, dvadata_kathode.DVA_neu, Vollzelle.Q);
dvadata_anode.DVA_interp = interp1(Anode.Q_neu, -dvadata_anode.DVA_neu, Vollzelle.Q);

% PLOT DVA aller Kurven
U_diff_neu = Kathode.U_neu - Anode.U_neu;
dva_gesamt = dvadata_anode.DVA_interp + dvadata_kathode.DVA_interp;
figure(4)
subplot(2,1,1)
    plot(Anode.Q_neu, -dvadata_anode.DVA_neu);
    hold on
    plot(Kathode.Q_neu, dvadata_kathode.DVA_neu);
    plot(Vollzelle.Q, dvadata_Vollzelle.DVA);
    plot(Vollzelle.Q, dva_gesamt);
    hold off
ylim([-2 1]);
title('DVA aller Kurven')
xlabel('Ladung Q  /  Ah')
ylabel('DVA  /  Ah / V')
legend('DVA Anode','DVA Kathode','DVA Vollzelle', 'DVA Summe', 'location','northwest', 'orientation', 'horizontal');

%U_diff = Kathode.U(1:max_length) - Anode.U(1:max_length);

subplot(2,1,2)
    plot(Vollzelle.Q, Vollzelle.U);
    hold on
    plot(Kathode.Q_neu, Kathode.U, '--');
    plot(Anode.Q_neu, Anode.U, '-.');
    plot(Vollzelle.Q, U_diff_neu);
    hold off
title('OCV aller Kurven')
xlabel('Ladung Q  /  Ah')
ylabel('Spannung U / V')
legend('Vollzelle', 'Kathode', 'Anode', 'Differenzspannung','Location','W');

% Die differenz der Spannung der Anode, Kathode ergibt ziehmlich genau den
% Spannungsverlauf der Vollzelle. Der Ãœberhang der Anode links und rechts 
% wird nicht genutzt da die Anode Ã¼berdimensioniert ist.
