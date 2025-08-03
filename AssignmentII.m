clear all; clc;

% Create a matrix with random numbers and defined size.
A = randi([1, 8], 30, 20);

% Define a variable n, that counts up for each row with a sum over 100
% and indexes the matrix containing the result (rows_larger_100).
n = 1;
% For loop to calculate which row has a sum of entries over 100. The
% number of rows is determined by size(A, 1) where the second argument (=
% 1) defines that the size function outputs the number of rows. ii results
% in a vector of numbers from one to number of rows in A with an increment
% of 1. 
for ii = 1:1:size(A, 1)
    % The function sum outputs the sum of all values of A in the current
    % row (:) of the iteration (ii). rows_larger_100 has as many entries as
    % rows with a sum over 100, therefore the separate counter and index
    % variable n is required.
    if sum(A(ii,:)) > 100
        rows_larger_100(n) = ii;
        n = n + 1;
    end        
end
% The problem is solved in the same manner for colum sums over 100.
m = 1;
for jj = 1:1:size(A, 2)
    if sum(A(:,jj)) > 100
        cols_larger_100(m) = jj;
        m = m + 1;
    end        
end