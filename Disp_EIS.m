function [] = Disp_EIS(Plot_Name,EIS,Farbe,Marker,Legende)
%Erklärung des Funktionsaufbaus folgt hier:
%   Die Funktion Disp_EIS plottet ausgehend von der Angabe welches
%   Plotformat verwendet werden soll (Plot_Name = 'Nyquist' oder 'Bode')
%   das entsprechende Impedanzspektrum (EIS = 1.Spalte Frequenz, 2.Spalte Real- 3.Spalte Imaginärteil) 
%   im gewünschten Format unter Verwendung der gewählten Farbe und Marker
%   und der gewählten Legende,
%  in ein bereits bestehendes figure!!! (ist dies nicht erwünscht, so muss
%  vor dem Funktionsaufruf der Befehl figure gegeben werden).

if strcmp(Plot_Name, 'Nyquist') == 1
    plot(EIS(:,2),EIS(:,3),Marker,'Color',Farbe);
    hold on
    axis equal %gleich skaliert
    set(gca,'YDir','reverse'); %Umkehren der Y-Achse
    xlabel('Z` / \Omega'); %Beschriftung x-Achse Könnte natürlich auch mit übergeben werden, aber der Einfachheit halber festgelegt!
    ylabel('Z`` / \Omega'); %Beschriftung y-Achse
    legend(Legende); %fügt Legende ein
    
elseif strcmp(Plot_Name, 'Bode') == 1
    subplot(2,1,1);
    plot(EIS(:,1),sqrt(EIS(:,2).^2+EIS(:,3).^2),'Color',Farbe);
    hold on
    set(gca,'xscale','log'); %logarithmische x-Achse
    set(gca,'yscale','log'); %logarithmische y-Achse
    xlabel('f / Hz'); %Beschriftung x-Achse
    ylabel('|Z| / \Omega'); %Beschriftung y-Achse

    subplot(2,1,2);
    plot(EIS(:,1),atan(EIS(:,3)./EIS(:,2))./pi.*180,'Color',Farbe);
    hold on
    set(gca,'xscale','log'); %logarithmische x-Achse
    xlabel('f / Hz'); %Beschriftung x-Achse
    ylabel('\Theta / °'); %Beschriftung y-Achse
    legend(Legende); %fügt Legende ein

elseif strcmp(Plot_Name, 'ReIm') == 1
    subplot(2,1,1);
    plot(EIS(:,1),EIS(:,2),'Color',Farbe);
    hold on
    set(gca,'xscale','log'); %logarithmische x-Achse
    xlabel('f / Hz'); %Beschriftung x-Achse
    ylabel('Re(Z) / \Omega'); %Beschriftung y-Achse

    subplot(2,1,2);
    plot(EIS(:,1),EIS(:,3),'Color',Farbe);
    hold on
    set(gca,'xscale','log'); %logarithmische x-Achse
    xlabel('f / Hz'); %Beschriftung x-Achse
    ylabel('Im(Z) / \Omega'); %Beschriftung y-Achse
    legend(Legende); %fügt Legende ein    
    
else disp('Diese Plot-Art ist nicht definiert!');
end


end

