% -----
% Exercise 1.5: a basic matlab code that 
%               performs matrix addition 
% -----
clear all;
close all
clc 
%Initialize matrices A and B 
A=[3 1 2; 8 6 5; 0 4 1]
B=[2 5 9; 7 2 3; 1 1 -1]
%B=[ 5 9;  2 3;  1 -1];

%find the dimension of matrix A and B
[m1,n1]=size(A); 
[m2,n2]=size(A); 

%check matrix dimensions
%(size must be the same)
if (m1==m2) & (n1==n2)
	disp('Matrix dimensions are the same.')
else
   disp('Error : Matrix dimensions are not the same!!!')
   return
end

%summation of A + B =C
for i=1:m1
    for j=1:n1
        C(i,j)=A(i,j)+B(i,j);
    end
end
%display result of A+B 
disp('Result of A+B is')
my_sum=C

%check your result using the built-in matlab function: 
matlab_sum =A+B

