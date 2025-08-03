% calculates the product of the elements of each column of the matrix MAT
function Pc=productcolumn(MAT)

% preallocate the vector in which the product will be stored
% this will be an output row vector
[nr,nc]=size(MAT); % get number of rows and columns of the matrix
Pc= ones(1,nc); % initialize to 1 because of coming product

% loop over the columns first (outer loop)
for ic=1:nc
    % loop now over the rows
    for ir=1:nr
        Pc(ic)=Pc(ic)*MAT(ir,ic); % make the product and update the output vector element accordingly
    end
    fprintf(1,'The product of all elements of column %d of MAT is %f\n',ic,Pc(ic));
end
