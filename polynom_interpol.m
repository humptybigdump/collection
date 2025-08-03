function p = polynom_interpol(x,y,n)

exponents =  ones(length(x),1) * (n:-1:0);

A = ( x.' * ones(1,n+1) ).^exponents;

p = A \ y.';
