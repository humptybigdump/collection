%The mean of all elements of each row of the matrix MAT
%data input and parameter
function meanvalues=robin(MAT)

% Vec = rand(1,2500);
% MAT = 3*ones(8,6);
% meanvalues = [];

%calculation of the mean of every element in each row
for e = 1:size(MAT,1)
    sum = 0;
%     meanvalue = 0;
    for i = 1:size(MAT,2)
        sum = MAT(e,i) + sum;
    end
%         meanvalue = sum/size(MAT,2);
        meanvalues(e,1) = sum/size(MAT,2);
    %end
end    

%data output
disp(meanvalues)