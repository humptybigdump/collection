%% Parametrierung (hier Frequenzbereich, Plot-Typ, gew�hltes Modell und zugeh�rige Parameter eingeben)
f=logspace(6,-3); %definieren eines logarithmischen Vektors 
Plot_Name = 'ReIm'; %Angabe in welchem Format das Spektrum geplottet werden soll: 'Nyquist' (f�r Real-Imagin�r-Plot in komplexer Ebene) oder 'Bode' (f�r Plot von Betrag und Phase �ber Frequenz)
ESB_Modell={'L';'R';'RC';'C'}; %hier das Ersatzschaltbild in der Reihenfolge wie die Parameter �bergeben werden eintragen;
Color='k'; %Angabe des f�r den Plot gew�hlten Farbe gem�� Matlab-Bezeichnungen
Marker='>'; %Angabe des f�r den Plot gew�hlten Markers gem�� den Matlab-Bezeichnungen
Parameter=zeros(length(ESB_Modell),2); %Erzeugt eine Matrix f�r die Parameter der einzelnen ESB-Elemente (zeilenweise), je zwei, da f�r die hinterlegten ESB-Elemente maximal zwei Parameter notwendig sind.
% Definition der ESB-Element-Parameter zeilenweise (Spalte 1 ist f�r 
% Induktivit�t, Kapazit�t, Widerstand; Spalte 2 f�r Zeitkonstanten)
Parameter(1,1)=1e-5;
Parameter(2,1)=5;
Parameter(3,1)=10;
Parameter(3,2)=1e-4;
Parameter(4,1)=10;

%% Berechnung (ab hier muss nichts mehr ver�ndert werden!)
Legende=[ESB_Modell(1)];
for index_i=2:length(ESB_Modell);
Legende=[strcat(Legende,'-',ESB_Modell(index_i))]; %automatisierte Legendeneintragserzeugung, die durch "-" getrennt wird
end

[EIS_ges] = Calc_EIS(ESB_Modell(1),Parameter(1,:),f);
for index_i=2:length(ESB_Modell); %ab zwei weil die Berechnung des ersten Elements zur Erzeugung der Matrix vor die Schleife gezogen wurde.
   
    [EIS_Element] = Calc_EIS(ESB_Modell(index_i),Parameter(index_i,:),f); %Berechnung des jeweiligen Impedanzelements
    
    EIS_ges(:,2:3) = EIS_ges(:,2:3) + EIS_Element(:,2:3); %Berechnung der Summe aller bisherigen Impedanzelemente bis index_i (Spalte 1 ist ausgeklammert, da der Frequenzvektor f�r alle gleich)
end

figure;
Disp_EIS(Plot_Name,EIS_ges,Color,Marker,Legende);
