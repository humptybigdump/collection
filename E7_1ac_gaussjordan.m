clear all;
close all;
clc;

%Gauss-Jordan elimination without pivoting
% example no problem
A = [1 1 1; 1 2 2;1 1 2];
b = [1 1 2]';

% next try the example below
% example hit zero problem
%A = [1 1 1; 1 1 2;1 2 2];
%b = [1 2 1]';

% concatenate A and b
c =[A b];

[nrow,ncol]=size(c);

% loop through all rows
for i=1:nrow 
    % normalize i-th row with its pivot
    c(i,:)=c(i,:)./c(i,i);
    % loop through all other rows
    % make all entries below and above pivot zero
    for j=1:nrow
        if j~=i
            c(j,:)=c(j,:)-c(j,i)*c(i,:);
        end
    end
end
% finally, extract solution x from c
x_gaussjordan=c(:,nrow+1)
x_backslash=A\b