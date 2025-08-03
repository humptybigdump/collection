clear all;
close all;
clc;

% Computing inverse of A using
% Gauss-Jordan elimination
% Solve [A][x]=[b]
%a = [-5   2   1; 1   6   2; 3  -3   8];
disp('Given matrix A')
A = [1 1 1; 1 2 2;1 1 2]
disp('Generate identity matrix which has the same size as A')
I = eye(length(A))
disp('concate matrix A and I')
c = [A I]                 

disp('Perform the Gauss-Jordan elimination')
[nrow,ncol]=size(c);
% loop through all rows
% loop through all rows
for i=1:nrow 
    disp(['Loop for pivot #' num2str(i)])
    disp(['Normalizing the row of pivot #' num2str(i)])
    c(i,:)=c(i,:)./c(i,i)
    % loop through all other rows
    disp(['Zeroing all entries below and above pivot c(' num2str(i) ',' num2str(i) ')'])
    for j=1:nrow
        if j~=i
            disp(['Row ' num2str(j) ', in loop for pivot #' num2str(i) ])
            c(j,:)=c(j,:)-c(j,i)*c(i,:)
        end
    end
end
disp('Extract the inverse of A from the resulting matrix c')
Ainv_gaussjordan = c(:,nrow+1:end)
Ainv_matlab = A^-1

