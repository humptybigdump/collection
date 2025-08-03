% Demo to show the problem of division by zero
% when using Gauss-elimination method.
% Fixed by partial pivoting.
% Note !! The code is for demo purpose.
% It is not the most efficient code for
% performing Gauss-elimination.

clear all;
close all;
clc;

% Solve [A][x]=[b]
A = [1 1 1
    1 1 2
    1 2 2]
b = [1 2 1]'

%A=[4 1 2 -1; 3 6 -1 2;2 -1 5 -3;4 1 -3 -8];
%b=[2 -1 3 2]';

%cond(A)

% The code can perform Gauss-elimination with
% and without partial pivoting.
% Set partialpivot = true to activate partial pivoting.
% Set partialpivot = false to deactivate partial pivoting.
%partialpivot=true
partialpivot = false

% Construct the augmented matrix c = [A|b]
c=[A b];
[nrow,ncol]=size(c);

% first phase : forward elimination sweep
% notice computational effort = O(n^3)
for i=1:nrow-1 %i is row of pivot
    disp(['Forward elimination sweep, stage: ' num2str(i)])
    %
    % when partialpivot is true, do partial pivoting first.
    if partialpivot == true
        disp('in: partial pivoting')
        for j=i+1:nrow % j is row under pivot
            if abs(c(j,i)) > abs(c(i,i))
                disp('We need to swap the rows')
                i
                j
                c_keep = c(j,:)
                c(j,:)=c(i,:)
                c(i,:)=c_keep
            end %endif
        end %endfor
    end %endif
    %
    % forward elimination
    % make entries below pivot zero
    for j=i+1:nrow %row under pivot
        disp('in: make entries below pivot zero')
        i
        j
        c(j,:)=c(j,:)-c(i,:)*(c(j,i)/c(i,i))
    end
    
end

%second phase : backward substitution
%notice comp effort = O(n^2)
%Solve x
x=zeros(nrow,1);
for i=nrow:-1:1
    disp('in: backward substitution')
    sumax=0;
    for j=i+1:nrow
        sumax=sumax+c(i,j)*x(j);
    end
    x(i)=(c(i,ncol)-sumax)/c(i,i);
end

my_x = x
matlab_x = A\b
