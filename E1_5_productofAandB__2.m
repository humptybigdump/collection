% -----
% Exercise 1.5b: a basic matlab code to compute 
%                the prooduct of two matrices 
% -----
clear all
close all
clc 
%Initialize matrices A and B 
%interior size must be the same
%A=[3 1 2; 8 6 5; 0 4 1];
%B=[2 5 9; 7 2 3; 1 1 -1];

A=[3 1; 8 6; 0 4];
B=[5 9; 7 2];


%find the dimension of matrix A
[m1,n1]=size(A); 
[m2,n2]=size(B);

%check interior size
if(n1==m2) 
    disp('A and B can be multiplied!')
else
    disp('Error: Inner dimensions do not match, A and B can not be multiplied!!!')
    return
end    

%Multiply A and B
for i=1:m1
    for j=1:n1
        sum=0;
        for k=1:n2
          sum = sum + A(i,k).*B(k,j);
        end
    C(i,j)=sum;
    end
end
my_product = C

%in matlab you can also simply use : 
matlab_product = A*B

