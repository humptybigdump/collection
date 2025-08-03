    function a=Zeilennormalform(d)
a=d; [m,n]=size(a); i=1;
for j=1:n, 
    if i<=m,
       l=i;  while and(a(l,j)==0, l<m), l=l+1; end; 
           if not(a(l,j)==0),  
             for k=(l+1):m, e=a(k,j); a(k,j:n)=a(k,j:n)-(a(k,j)/a(l,j))*a(l,j:n); if not(e==0), b=a , end;  end; 
              if not(i==l), b=a(i,j:n); a(i,j:n)=a(l,j:n); a(l,j:n)=b;  b=a , end;
              i=i+1; 
           end; 
   end;  %%%%%% Bis jetzt war die Zeilenstufenform. Dieser Teil wurde aus dem Programm zeilenstufenform.m koppiert.
end;
   for i=1:m,   
    j=1;  while and(a(i,j)==0, j<n) j=j+1; end; k(i)=j+(a(i,j)==0); 
   end;                                            
  r=m; while (k(r)==n+1), r=r-1; end;  %%%%% k(i),r der Definition werden bestimmt (dieser Teil wurde von ZSFueberpruefen kopiert) 
  for i=1:r, a(i,1:n)=a(i,1:n)/a(i,k(i)), end;   %%%% Das erste Element jeder Zeile wird eins
  for i=1:r, for j=1:i-1, a(j,1:n)=a(j,1:n)-a(j,k(i))*a(i,1:n), end; end; %%%%% Die Elemente daoben werden Null