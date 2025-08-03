% -----
% Exercise 1.5a: a basic matlab code to compute 
%                the trace of a matrix 
% -----
clear all
close all
clc 
%Initialize matrix A
A=[3 1 2; 8 6 5; 0 4 1]

%find the dimension of matrix A
[m,n]=size(A); 

%check square 
if (m==n)
    disp('Matrix is a square matrix.')
else
    disp('Error: matrix is not square!!')
    return
end    

%Compute the trace
sum=0;
for i=1:m
    for j=1:n
        if (i==j)
          sum = sum + A(i,j);
        end
    end
end
my_trace=sum

% the matlab built-in function : 
matlab_trace =trace(A)

