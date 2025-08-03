% Function to calculate the mean of each row of a given matrix
%
% USAGE: Mr = meanrow(MAT)
%
% INPUT:
%	- MAT: 2D matrix of real numbers, size: [nr,nc]
%
% OUTPUT:
%	- Mr: column vector with the average value of each row of MAT, 
%         size: [nr,1]
%
function Mr = meanrow(MAT)

% get the number of rows and columns of the input matrix MAT
[nr, nc]=size(MAT); % nr = nb of rows, nc = nb of columns

% initialize the output column vector Mr to zeros
% this is necessary because below we sum up the column values for a given
% row by looping
Mr = zeros(nr,1);

% first, we loop over the rows
for ir=1:nr
    % now we loop over the columns (for the row "ir") and sum each element
    for ic=1:nc
        Mr(ir)=Mr(ir)+MAT(ir,ic); % the sum here justifies the initializing of Mr
    end
    % to get the average we divide by the number of columns
    Mr(ir) = Mr(ir)/nc;
    % we print the result on the screen for each row
    fprintf(1,'The average of the matrix MAT for row %d is: %f\n', ir, Mr(ir));
end

% well done!
