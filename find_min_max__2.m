function [i_max_col, i_min_col,...
            i_max_row, i_min_row] = find_min_max(X)

% EVERYTHING IN GREEN WILL NOT BE COMPUTED, THIS IS COMMENT TEXT, JUST LIKE
% WRITTEN TEXT. IT IS INDICATED BY % IN THE BEGINNING OF THE TEXT.
% Just type a % and then all following text will be green. 

% GOAL
% We want the indices (the row and column number) of the minimum and 
% maximum value in the input matrix.

% ---------------------- INFORMATION FROM DOCUMENTATION ----------------- %
% 1. ROW VERSUS COLUMN
% M = max(A,[],dim) returns the largest elements along dimension dim. 
% For example, if A is a matrix, then max(A,[],2) is a column vector 
% containing the maximum value of each row.

% 2. INDEXING
% [M,I] = max(___) finds the indices of the maximum values of A and 
% returns them in output vector I, using any of the input arguments in the 
% previous syntaxes. If the maximum value occurs more than once, then max 
% returns the index corresponding to the first occurrence.


% ---------------- EXTRACT THE MINIMUM AND MAXIMUM VALU------------------ %
% Find the maximum of the entire matrix (--> X(:))
max_X = max(X(:));
% Find the minimumof the entire matrix (--> X(:))
min_X = min(X(:));

% --------------------------- FINDING A VALUE --------------------------- %
% [row,col] = find(___) returns the row and column subscripts of each 
% nonzero element in array X using any of the input arguments in previous 
% syntaxes.
% Find the corresponding matrix index for each value, using the function
% find
[i_max_row, i_max_col] = find(X == max_X);
[i_min_row, i_min_col] = find(X == min_X);

% OR Combine the index in a matrix for output
% If you use this, then the OUTPUT variables of the function should be
% changed to i_max and i_min. This means you will have only TWO output
% variables, which each contain 2 values (the row and column number). 
i_max = [i_max_row i_max_col];
i_min = [i_min_row i_min_col];


end