clear all;
close all;
clc;

% Computing inverse of A using
% Gauss-Jordan elimination
% Solve [A][x]=[b]
%a = [-5   2   1; 1   6   2; 3  -3   8];
a = [1 1 1; 1 2 2;1 1 2];   % matrix A
b = eye(length(a));         % identity matrix
ab = [a b];                 % concate AI

[nrow,ncol]=size(ab);

% loop through all rows
% normalize and zero out
for i=1:nrow %each pivot
    % normalize
    ab(i,:)=ab(i,:)./ab(i,i)
    % loop through all other rows
    % to zero out
    for j=1:nrow
        if j~=i
            ab(j,:)=ab(j,:)-ab(j,i)*ab(i,:)
        end
    end
end
Ainv_gaussjordan = ab(:,nrow+1:end)
Ainv_matlab = a^-1

