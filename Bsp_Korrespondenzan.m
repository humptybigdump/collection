
% Beispiel aus Kap 3 corrAnal
% Entnommen dem Buch von Greenacre

% Originaldatenmatrix
Data=[4 2 3 2
4 3 7 4
25 10 12 4
18 24 33 13
10 6 7 2];

% eine potentielle Schleife, um den Effekt der Massen auf das
% Durchschnittsprofil zu verdeutlichen
for faktor=[1:.5:2.5,5],
Data(1,:)=faktor*Data(1,:);

Zeilennamen={'Seniormanager','Juniormanager', 'Seniorangest.',...
    'Juniorangest.','Sekretariat'};
Spaltennamen={'Nicht','Leicht','Mittel','Schwer'};

% Baue eine Tabelle, in der die Spalten/Zeilensummen stehen
OrTab=[Data, sum(Data,2)];
OrTab=[OrTab;sum(OrTab,1)];

% Standardisierte Tabellen
Data_std=Data/sum(sum(Data));
StdTab=[Data_std, sum(Data_std,2)];
StdTab=[StdTab;sum(StdTab,1)];
Spaltenmassen=StdTab(6,1:4)';
Zeilenmassen=StdTab(1:5,5);

for i=1:length(Data_std(:,1)),
   for j=1:length(Data_std(1,:)),
      
       eij=sum(Data_std(i,:))*sum(Data_std(:,j))
       Data_stdtmp(i,j)=(Data_std(i,j) -eij)/sqrt(eij);
       
   end
end
% Das ist jetzt die Standardisierte Matrix (ohne Ränder)
Data_std=Data_stdtmp;

% jetzt standard svd ohne Normierung
[U,D,V]=svd(Data_std,'econ');

% Koordinaten in jeweils des anderen Basissystems
ZProf=U*D;
SProf=V*D;

% erzeuge eine Figure und plotte Zeilen/Spaltenprofile
figure;
% Zeilenprofil, symmetrische Normierung

ZProfNorm=U*sqrt(D).*(   (1./sqrt(StdTab(1:5,5)))*ones(1,4));
%ZProfNorm=ZProf;
plot(ZProfNorm(:,1),ZProfNorm(:,2),'x')
set(gca, 'xlim',[-1 1],'ylim',[-1 1])
grid on
text(ZProfNorm(:,1),ZProfNorm(:,2), Zeilennamen, 'VerticalAlignment','bottom', ...
                             'HorizontalAlignment','center', ...
                             'FontSize', 8)
hold on;

% Spaltenprofil, symmetrische Normierung
SProfNorm=V*sqrt(D).*(   (1./sqrt(StdTab(6,1:4)'))*ones(1,4));
%SProfNorm=SProf;
plot(SProfNorm(:,1),SProfNorm(:,2),'xr')
set(gca, 'xlim',[-1 1],'ylim',[-1 1])
grid on
text(SProfNorm(:,1),SProfNorm(:,2), Spaltennamen, 'VerticalAlignment','bottom', ...
                             'HorizontalAlignment','center', ...
                             'FontSize', 8)
hold on;

% kosmetische Anpassungen 
% GET TICKS
X=get(gca,'Xtick');
Y=get(gca,'Ytick');

% GET LABELS
XL=get(gca,'XtickLabel');
YL=get(gca,'YtickLabel');

% GET OFFSETS
Xoff=diff(get(gca,'XLim'))./40;
Yoff=diff(get(gca,'YLim'))./40;

% DRAW AXIS LINEs
plot(get(gca,'XLim'),[0 0],'k');
plot([0 0],get(gca,'YLim'),'k');

% Plot new ticks  
for i=1:length(X)
    plot([X(i) X(i)],[0 Yoff],'-k');
end;
for i=1:length(Y)
   plot([Xoff, 0],[Y(i) Y(i)],'-k');
end;

% Füge Beschriftungen hinzu

%Inertias der Achsen
text(0.8,-1.*Yoff,['Achse ',num2str(100*D(1,1)^2/sum(sum(D.^2))),'% Inertia'], 'FontSize', 6);
text(1*Xoff,0.9,['Achse ',num2str(100*D(2,2)^2/sum(sum(D.^2))),'% Inertia'], 'FontSize', 6);

% Skala
text(0.8,-0.9,'Skala 0.2', 'FontSize', 6);
plot([0.8, 1],[-0.96,-0.96],'k')
plot([0.8 0.8],[-0.96-Yoff/2,-0.96+Yoff/2],'k')
plot([1 1],[-0.96-Yoff/2,-0.96+Yoff/2],'k')

box off;
axis square;
axis off;
set(gcf,'color','w');

% Abspeichern
saveas(gcf,'Bsp_CorrAn.eps', 'psc2')
saveas(gcf,'Bsp_CorrAn.pdf', 'pdf')

%Inertias einzelner Koordinaten
%Zeilen
Ti=sum(Data_std.^2,2)

%Spalten
Tj=sum(Data_std.^2)'


%Inertia Rekonstruktion basierend auf der ersten Koordinate
%Zeilen
Ti1=sum((V(:,1)*ZProf(:,1)').^2)'

%Spalten
Tj1=sum((U(:,1)*SProf(:,1)').^2)'

%Inertia Rekonstruktion basierend auf der zweiten Koordinate
%Zeilen
Ti2=sum((V(:,2)*ZProf(:,2)').^2)'

%Spalten
Tj2=sum((U(:,2)*SProf(:,2)').^2)'

%Inertia Rekonstruktion basierend auf der zweiten Koordinate
%Zeilen
Ti3=sum((V(:,3)*ZProf(:,3)').^2)'

%Spalten
Tj3=sum((U(:,3)*SProf(:,3)').^2)'


%Inertia Rekonstruktion basierend auf den ersten beiden Koordinaten
%Zeilen
Ti12=sum((V(:,1:2)*ZProf(:,1:2)').^2)'

%Spalten
Tj12=sum((U(:,1:2)*SProf(:,1:2)').^2)'

%Anteil an den Zeilen
round(1000*[Ti1./Ti Ti2./Ti Ti3./Ti])/1000

% Anteil an den Spalten
round(1000*[Tj1./Tj Tj2./Tj Tj3./Tj])/1000


%Wieviel fällt jeweils auf die erste Koordinate

%Zeilen
round(1000*Ti1/sum(Ti1))/1000

%Spalten
round(1000*Tj1/sum(Tj1))/1000



end