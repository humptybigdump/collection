function a=Zeilenstufenform(d)
a=d; [m,n]=size(a); i=1;
for j=1:n,   %%%%%% Fuer alle Spalten der Matrix 
       l=i;  while and(a(l,j)==0, l<m) l=l+1; end; %%%%%%% Suchen wir das erste Element das nicht Null ist
       %%%%%% Die Zeilen die wir weggestrichen haben beruecksichtigen wir nicht
           if not(a(l,j)==0),  %%%%%%% Die Matrix wird geaendert nur wenn wir ein Element finden das nicht 
               %%%%%% Null ist. Sei l die Zeile mit diesem Element das
               %%%%%% nicht Null ist
             for k=(l+1):m, e=a(k,j); a(k,j:n)=a(k,j:n)-(a(k,j)/a(l,j))*a(l,j:n); if not(e==0), b=a , end;  end; 
             %%%%%%% Wir subktraieren die Merfaches der Zeile l von den
             %%%%%%% naechsten, damit die anderen Elemente der Spalte Null
             %%%%%%% werden.
              if not(i==l), b=a(i,j:n); a(i,j:n)=a(l,j:n); a(l,j:n)=b;  b=a;  end;
              %%%%%%% Die Zeilen werden vertauscht, wenn das erste Element der Spalte Null ist, so dass es nicht
              %%%% Null wird.
              i=i+1;  %%%%%%%%%% Wir Streichen die Zeile i weg
           end;  %%%% Ansonsten keine Aenderung (wenn alle Elemente Null sind). Wir suchen
           %%%%% Die naechste Spalte j+1, aber wir streichen die Zeile i nicht
    end;