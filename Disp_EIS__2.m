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
    % Nutzen Sie hier ihre Ergebnisse aus Aufgabe 2
    
elseif [] %Bodeplot
    
else disp('Diese Plot-Art ist nicht definiert!');
end


end

