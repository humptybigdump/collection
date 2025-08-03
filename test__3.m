%Beispiel Maschenstromverfahren (siehe Aufgabe T4a)
A_1 = [5.4 3 0; 3 10 5; 0 5 6];
U_1 = [-9; 0; 12];

I = cramerRegel(A_1,U_1,1); 


%Beispiel Knotenpunktpotentialverfahren (siehe Aufgabe T4b)
A_2 = [1.25 -0.5; -0.5 1.7];
I_1 = [3.75; 12];

I = cramerRegel(A_2,I_1,2);







