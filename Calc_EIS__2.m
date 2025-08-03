function [EIS_Werte] = Calc_EIS(Element_Name,Parameter,Frequenzvektor)
%Erkl�rung des Funktionsaufbaus folgt hier:
%   Die Funktion Calc_EIS berechnet ausgehend von der Angabe welches
%   ESB-Element berechnet werden soll (Element_Name) = 'R','L','C','RC',
%   dessen Impedanz, mit Hilfe der gegebenen Parameter, dabei ist f�r R,L,C
%   Parameter ein Skalar mit dem Widerstand, der Induktivit�t oder der
%   Kapazit�t. Und f�r RC ist Parameter(1) der Widerstand des RCs und
%   Parameter(2) dessen Zeitkonstante. Als letzter Parameter muss der
%   Frequenzvektor �bergeben werden.

EIS_Werte=zeros(length(Frequenzvektor),3); %Vordefinieren einer aus Nullen bestehenden Matrix mit Zeilenzahl = L�nge des Frequenzvektors und Spaltenzahl = 3;
EIS_Werte(:,1)=Frequenzvektor; %bef�llen der ersten Spalte mit dem Frequenzvektor

if strcmp(Element_Name,'R') == 1
    EIS_Werte(:,2)=Parameter(1);
elseif []
    EIS_Werte(:,3)=[];
elseif []
%...
else disp('Bitte geben Sie die korrekte Bezeichnung eines definierten Ersatzschaltbildelements ein!');
end


end

