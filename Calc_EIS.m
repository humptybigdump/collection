function [EIS_Werte] = Calc_EIS(Element_Name,Parameter,Frequenzvektor)
%Erklärung des Funktionsaufbaus folgt hier:
%   Die Funktion Calc_EIS berechnet ausgehend von der Angabe welches
%   ESB-Element berechnet werden soll (Element_Name) = 'R','L','C','RC',
%   dessen Impedanz, mit Hilfe der gegebenen Parameter, dabei ist für R,L,C
%   Parameter ein Skalar mit dem Widerstand, der Induktivität oder der
%   Kapazität. Und für RC ist Parameter(1) der Widerstand des RCs und
%   Parameter(2) dessen Zeitkonstante. Als letzter Parameter muss der
%   Frequenzvektor übergeben werden.

EIS_Werte=zeros(length(Frequenzvektor),3); %Vordefinieren einer aus Nullen bestehenden Matrix mit Zeilenzahl = Länge des Frequenzvektors und Spaltenzahl = 3;
EIS_Werte(:,1)=Frequenzvektor; %befüllen der ersten Spalte mit dem Frequenzvektor

if strcmp(Element_Name,'R') == 1
    EIS_Werte(:,2)=Parameter(1);
elseif strcmp(Element_Name,'L') == 1
    EIS_Werte(:,3)=2.*pi.*Parameter(1).*EIS_Werte(:,1);
elseif strcmp(Element_Name,'C') == 1
    EIS_Werte(:,3)=-1./(2.*pi.*Parameter(1).*EIS_Werte(:,1));
elseif strcmp(Element_Name,'RC') == 1
    EIS_Werte(:,2)=real(Parameter(1)./(1+1j.*2.*pi.*Parameter(2).*EIS_Werte(:,1)));
    EIS_Werte(:,3)=imag(Parameter(1)./(1+1j.*2.*pi.*Parameter(2).*EIS_Werte(:,1)));
else disp('Bitte geben Sie die korrekte Bezeichnung eines definierten Ersatzschaltbildelements ein!');
end


end

