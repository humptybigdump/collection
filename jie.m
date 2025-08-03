% The product of all elements of the matrix MAT

MAT = rand(8,6);

prod = 1; % initialize the product to 1

% loop over the number of rows
for row = 1:size(MAT,1)
    % loop over the number of columns
    for col = 1:size(MAT,2)
        prod = MAT(row, col)*prod; % update the product with the new matrix element
    end
end    

% show result
disp(prod)