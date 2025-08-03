%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Geometrische Modelle der Geodaesie - Uebung 2
% Autor: Leon Klonowski
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

a = (1:20)';
f = grad2Rad(a);
g = absolute(a);

plot=sum(f,g);


figure
scatter(plot,(1:20));

%% Funktionen
function y = grad2Rad(x)
y = x*pi()/200;
end
function y = absolute(x) 
if x<0
    y=x*-1;
else
    y=x;
end
end
function y = sum(x,z)
y = x+z;
end