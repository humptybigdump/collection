%% Gruppe 1 
%% a)
syms x a b n;

% f=(x^3);
fun = @(x) x.^3;

n=3;

a=0;
b=1;
h=(b-a)/n;

n1=(b-a)/2* (fun(a)+fun(b));
n2=(b-a)/6* (fun(a)+4*fun((a+b)/2) + fun(b) );
n3= (b-a)/8* (fun(a)+3*fun(a+b)+3*fun(b-h)+fun(b));

e1 =abs( n1- integral(fun,a,b));
e2 =abs( n2- integral(fun,a,b));
e3 =abs( n3- integral(fun,a,b));

%% b) Quadrature error

funl = @(x) x.^1;

% Ungleichung berechnen Fehlt hier noch

Qe3 = abs( integral(fun,a,h) - integral(funl,a,h) );
n=2;
Qe2 = abs( integral(fun,a,h) - integral(funl,a,h) );
n=1;
Qe1 = abs( integral(fun,a,h) - integral(funl,a,h) );





