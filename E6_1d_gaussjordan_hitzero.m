% Demo to show the problem of division by zero
% when using Gauss-Jordan method.
% Fixed by partial pivoting.
% Note !! The code is for demo purpose.
% It is not written in the most efficient way.

clear all;
close all;
clc;

%Solve [A][x]=[b]

% example no problem
%A = [1 1 1; 1 2 2;1 1 2];
%b=[1 1 2]';

% example hit zero problem
A = [1 1 1; 1 1 2;1 2 2];
b=[1 2 1]';

% Set partialpivot = true or 1 to activate partial pivoting.
% Set partialpivot = false or 0 to deactivate partial pivoting.
%partialpivot=true
partialpivot = true

% construct the augmented matrix
c=[A b]
[nrow,ncol]=size(c);

% Gauss-Jordan method :
% loop forward through all rows
% first normalize and then zero out
for i=1:nrow % each pivot
    disp(['Loop for pivot #' num2str(i)])
    %
    if partialpivot
        disp('Check : partial pivoting required?')
        for j=i+1:nrow % j is row under pivot
            if abs(c(j,i)) > abs(c(i,i))
                disp(['Yes, we need to swap rows ' num2str(j) ' and ' num2str(i)])
                c_larger = c(j,:)
                c(j,:)=c(i,:)
                c(i,:)=c_larger
            else
                disp('No need to swap rows')
            end %endif
        end %endfor
    end %endif
    %
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
x_backslash=A\b