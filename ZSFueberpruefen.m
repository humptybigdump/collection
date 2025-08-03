function c=ZSFueberpruefen(a)
 [m,n]=size(a); f=1;
for i=1:m,   %%%%%% Fuer alle Zeilen der Matrix 
    j=1;  while and(a(i,j)==0, j<n) j=j+1; end; k(i)=j+(a(i,j)==0); %%%%%% Finde die Spalte des Ersten nicht Null Elements k(i) sind gleich wie in Definition
end;                                            %%%%% wenn das letzte Element auch Null ist dann wird k(i)=n+1, 
  r=m; while (k(r)==n+1), r=r-1; end;  %%%%%%% k(i)=n+1 bedeutet die Zeile i hat nur Nullen. r ist gleich wie in der Definition.
  for i=1:r-1, f=f*(k(i)<k(i+1)); d(i)=a(i,k(i)); end; d(r)=a(r,k(r)); c=f;  %%%%%%%% c=1 oder 0 bedeutet Aussage richtig oder falsch
  if (c==1) k=k(1:r), r,d, end;   %%%%%%%%%% Wenn die Aussage stimmt dann das Programm gibt die Zahlen r und k1,k2,...,kr.
  %%%%%%%%%%%%%%%%%%%   Mehr Erklärungen in der Zeile 4 des Programms die
  %%%%%%%%%%%%%%%%%%%   wird die Zeile i überprüft. Das Programm dann läuft
  %%%%%%%%%%%%%%%%%%%   entlang der Spalte j und wenn a(i,j) Null ist
  %%%%%%%%%%%%%%%%%%%   steigt j nach 1. Am Ende können wir finden die
  %%%%%%%%%%%%%%%%%%%   erste Spalte j, so dass a(i,j) nicht Null ist. Dann
  %%%%%%%%%%%%%%%%%%%   wird k(i)=j, außer wenn alle a(i,j) Null ist fuer
  %%%%%%%%%%%%%%%%%%%   alle j und dann wird k(i)=n+1 (in der Definition ist es unmöglich, dass ki=n+1, weil nur k1 bis kr existieren,
  %%%% aber diese zusätzliche Definition ist für das Programm hilfreich).
  %%%%%%%%%%%%%%%%%%%%%% In der Zeile 6 r wird als n definiert, wenn
  %%%%%%%%%%%%%%%%%%%%  k(n)=n+1 dann ist die n-te Zeile Null und wird r
  %%%%%%%%%%%%%%%%%%%%%% n-1. Wenn k(n-1)=n+1 dann ist die (n-1)-te Zeile
  %%%%%%%%%%%%%%%%%%%%%% Null und wird r n-2. So geht es weiter bis wir die
  %%%%%%%%%%%%%%%%%%%%%% richtige Zahl r gereicht wird
  %%%%%%%%%%%%%%%%%%%%%% In der Zeile 7 wird immer überprüft ob
  %%%%%%%%%%%%%%%%%%%%%% k1<k2<...<kr. f ist am Anfang 1. Wenn alle
  %%%%%%%%%%%%%%%%%%%%%% Ungleichungen stimmen dann sind alle Aussagen im
  %%%%%%%%%%%%%%%%%%%%%% forloop richtig und f bleibt 1. Anstonsten wird
  %%%%%%%%%%%%%%%%%%%%%% f=0. 
  %%%%%%%%%%%%%%%%%%%%%% In der Zeile 8 wenn c=1 (also wenn es wahr ist,
  %%%%%%%%%%%%%%%%%%%%%% dass die Matrix in Zeilenstufenform ist) dann gibt
  %%%%%%%%%%%%%%%%%%%%%% uns das Programm die Zahl r und die Zahlen
  %%%%%%%%%%%%%%%%%%%%%% k1,...,kr in der Form eines Zeilenvektors k.