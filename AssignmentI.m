clear all; clc;
 
% The matrix A, filled with random numbers and the size 19x7, is created
% with the function "randi". The first argrument is a 1x2 matrix containing
% the minimum of 1 and the maximum of 100 for the range of random numbers.
A = randi([1, 100], 19, 7);

% For loop to find the maximum of each row. The number of rows is
% determined by size(A, 1) where the second argument (= 1) defines that the
% size function outputs the number of rows. ii results in a vector of
% numbers from one to number of rows in A with an increment of 1.
for ii = 1:1:size(A, 1)
    % The function max outputs the maximum of all values of A in the current
    % row (:) of the iteration (ii). maxRow has as many entries as rows in A
    % which results from indexing it with ii.
    max_Row(ii) = max(A(ii,:));
end

% Extracting the maximum for each column is performed in the same manner.
for jj = 1:1:size(A, 2)
    max_Col(jj) = max(A(:,jj));
end