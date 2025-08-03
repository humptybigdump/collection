clear all;
close all;
clc;

%Gauss-Jordan elimination without pivoting
% example no hit zero problem
A = [1 1 1; 1 2 2;1 1 2];
b = [1 1 2]';

% next try the example below
% example hit zero problem
%A = [1 1 1; 1 1 2;1 2 2];
%b = [1 2 1]';

disp('concatenate A and b, c=[A b]')
c =[A b]

[nrow,ncol]=size(c);

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
% finally, extract solution x from c
x_gaussjordan=c(:,nrow+1)
%compare result with matlab built-in function " \ " 
x_backslash=A\b