% the function below is one solution proposal of the "Vector, matrix and
% loop" exercise, slide 66 of lecture (E. Gaucher WS2022-2023)

function [meanvec, sum2vec, maxvec, prodmat, Meanrow, Prodcol] = forloops(Vec, MAT)

% Vec = rand(1,2500);
% MAT = rand(8,6);

%% The mean of the vector Vec
% initialise the mean to zero
meanvec=0;
% sum each element of the vector together
for ii=1:numel(Vec)
    meanvec = Vec(ii) + meanvec;
end
% now get the average
meanvec = meanvec/numel(Vec);
% print result on screen
fprintf(1, 'The mean of vector Vec is: %f\n', meanvec);

%% The sum of the squares of all components of the vector Vec

% initialize the sum of the squares to zero
sum2vec = 0;
% sum the square of each element together
for ii=1:numel(Vec)
    sum2vec = Vec(ii)*Vec(ii) + sum2vec;
end
% show result on screen
fprintf(1, 'The sum of the squares of all components of Vec is: %f\n', sum2vec);

%% The largest value in the vector Vec
% initialize the maximum value to the first value in Vec
maxvec = Vec(1);
% for each following index (for loop) check if this maxvec is still larger
% than the other ones. If not, update maxvec accordingly
for ii=2:numel(Vec)
    if maxvec<Vec(ii)
        maxvec=Vec(ii);
    end
end
% show result on screen
fprintf(1, 'The largest value in the vector Vec is: %f\n', maxvec);

%% The product of all elements of the matrix MAT

% initialize the product to 1
prodmat = 1; 

% loop over the number of rows
for row = 1:size(MAT,1)
    % loop over the number of columns
    for col = 1:size(MAT,2)
        % update the product with the new matrix element
        prodmat = MAT(row, col)*prodmat; 
    end
end    
% show result on screen
fprintf(1, 'The product of all elements of the matrix MAT is: %f\n', prodmat);

%% The mean of all elements of each row of the matrix MAT
% get size of the matrix
[nrow, ncol]=size(MAT);
% initialise the mean of each row to 0
% so it is a column vector with nrow elements
Meanrow = zeros(nrow, 1);
% now add each column
for icol=1:ncol
    Meanrow = Meanrow + MAT(:,icol);
end
% divide the result by the number of column
Meanrow = Meanrow/ncol;
% print the result on screen
fprintf(1, 'The mean of all elements of each row of the matrix MAT is:\n');
fprintf(1, '%f\n', Meanrow);

%% The product of all elements of each column of the matrix MAT
% get size of the matrix
[nrow, ncol]=size(MAT);
% initialise the product for each column to 1
% so it is a row vector with ncol elements
Prodcol = ones(1, ncol);
% now multiply each column
for irow=1:nrow
    Prodcol = Prodcol .* MAT(irow,:);
end
% print the result on screen
fprintf(1, 'The product of all elements of each column of the matrix MAT is:\n');
fprintf(1, '%f ', Prodcol);
fprintf(1, '\n');
