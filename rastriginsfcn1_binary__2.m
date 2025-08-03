function scores = rastriginsfcn1_binary(pop_binary)
%function scores = rastriginsfcn1_binary(pop_binary)
%Binäre Variante der um eins verschobenen Rastrigin-Funktion
%Material zur Vorlesung Computational Intelligence
%
%Copyright 2003-2004 The MathWorks, Inc
%Modifiziert: [Ralf Mikut, KIT, 2020]

%bestimmt die Dimensionalität bei einer 8-Bit-Kodierung
mydimension = length(pop_binary)/8;
for i=1:mydimension
    %Berechnung Schrittweite
    temp = bin2dec(sprintf('%d ',pop_binary(8*(i-1)+[1:8])));
    %Abbildung auf Intervall -5:5
    pop(i) = -5 + temp/255*10;    
end;
%Ruf der reellwertigen, um Eins verschobenen Rastrigin-Funktion
scores = rastriginsfcn1(pop);



