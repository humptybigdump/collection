clear all
close all
%% Input
% Lade Gleichgewichtsspannung aus Matlab-File in "OCV"
OCV_input=load('C40_Pouchzelle_AG02-1944.mat'); 
OCV=[(OCV_input.vollzelle.t)' (OCV_input.vollzelle.I)' (OCV_input.vollzelle.U)'];

% Lade Entladekennlinien in Workspace
C1=xlsread('1C_Entladekennlinie.xls',1); 
C2=xlsread('2C_Entladekennlinie.xls',1);
C5=xlsread('5C_Entladekennlinie.xls',1);

%% Plots Erstellen

% Plotten aller Spannungskurven
fig=figure;
subplot(2,1,1)
plot(OCV(:,1),OCV(:,3))
hold on
plot(C1(:,1),C1(:,3))
plot(C2(:,1),C2(:,3))
plot(C5(:,1),C5(:,3))
legend('OCV', '1C-Entladung', '2C-Entladung', '5C-Entladung','Location','north','Orientation','horizontal')
title('Spannung über die Zeit')
xlabel('Zeit / s')
ylabel('Spannung / V')


% Normieren auf entnommene Ladungsmenge
OCV(:,4)=OCV(:,1).*-OCV(:,2)/3.6;
C1(:,4)=C1(:,1).*-C1(:,2)/3.6;
C2(:,4)=C2(:,1).*-C2(:,2)/3.6;
C5(:,4)=C5(:,1).*-C5(:,2)/3.6;

% Plotten aller Spnnungskurven über die entnommene Kapazität
subplot(2,1,2)
plot(OCV(:,4),OCV(:,3))
hold on
plot(C1(:,4),C1(:,3))
plot(C2(:,4),C2(:,3))
plot(C5(:,4),C5(:,3))
legend({'OCV', '1C-Entladung', '2C-Entladung', '5C-Entladung'},'Location','north','Orientation','horizontal')
title('Spannung über Ladungsmenge')
xlabel('Entnommene Ladung / mAh')
ylabel('Spannung / V')
% Speichern der Ausgabe
saveas(fig,'Spannungsverläufe','jpg')
