%% Matlab Abgabe 3 
% Programm zum lösen eines beliebigen inhomogenen überbestimmten LGS
% Sala, Stark, Freund
clc
close all
clear variables

A = zeros(20,10);
b = zeros(20,1);
for i=1:20
    for j=1:10
        A(i,j) = randi(10); 
    end
    b(i,1) = randi(10);
end

if rank(A) == 10
    if (A'*A) == (A'*A)'
    figure
    scatter(1:10 , eig(A' * A))
    ylabel('Eigenwert')
    xlabel('Eigenwertnummer')
    y = linsolve(A, b);
    x = linsolve(A', y);
    z = linsolve(A' * A, A' * b);
    end
end



