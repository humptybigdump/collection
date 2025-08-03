function y=Entdet(a)
[m,n]=size(a);  
if or((m~=n),(n==0)), error('input must be a square matrix'); %%% Der Computer schreibt Fehler, wenn die Matrix keine Quadratische Matrix ist
elseif (n==1), y=a(1,1);  %%% Wenn die Matrix 1x1 ist dann ist die Determinante gleich a(1,1)
else  y=a(1,1)*Entdet(a(2:n,2:n)); for i=2:n,  y=y+(-1)^(i+1)*a(1,i)*Entdet(a(2:n,[1:i-1, i+1:n])); end; end;
 %%% Ansonsten Determinantenentwicklungssatz!!