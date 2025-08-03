%% Exercise 1
% This script proposes solutions of Exercise 1

%% Internal parameter definition

% we work with a row vector
Vec = rand(1,5000001);
% Vec = rand(1,20);
% Vec = 1:21;

%% 1. How to compute the mean of a vector?

% for loop method
sumval = 0;
for ii=1:numel(Vec)
    sumval = Vec(ii) + sumval;
end
ave = sumval/numel(Vec);

disp(['Mean: for loop gives: ' num2str(ave)]);
fprintf(1, 'Mean: for loop gives: %f\n', ave);

% sum function method
ave = sum(Vec)/numel(Vec);
fprintf(1, 'Mean: sum gives: %f\n', ave);

% mean function method
fprintf(1, 'Mean: mean gives: %f\n', mean(Vec));

%% 2. How to compute sum of the squares of all components?

% for loop method
sumsq = 0;
for ii=1:numel(Vec)
    sumsq = Vec(ii)*Vec(ii) + sumsq;
end
fprintf(1,'\n');
fprintf(1, 'Sum of squares: for loop gives: %f\n', sumsq);

% sum with dot product method
sumsq = sum(dot(Vec,Vec));
fprintf(1, 'Sum of squares: sum with dot gives: %f\n', sumsq);

% vector product method
sumsq = Vec*Vec';
fprintf(1, 'Sum of squares: vector product gives: %f\n', sumsq);

%% 3. How to compute the std of a vector?

% for loop method
ave = 0;
for ii=1:numel(Vec)
    ave = Vec(ii) + ave;
end
ave = ave/numel(Vec);
sumsq = 0;
for ii=1:numel(Vec)
    sumsq = (Vec(ii)-ave)^2 + sumsq;
end
stdev = sqrt(sumsq/(numel(Vec)-1));

fprintf(1,'\n');
fprintf(1, 'Std: for loop gives: %f\n', stdev);

% mean function method
ave = mean(Vec);
stdev = sqrt((Vec-ave)*(Vec-ave)'/(numel(Vec)-1));
fprintf(1, 'Std: vector product with mean gives: %f\n', stdev);

% std function method
fprintf(1, 'Std: std gives: %f\n', std(Vec));

%% 4. How to compute the median of a vector?

% first we sort the vector values in increasing order
V2 = sort(Vec); % by default from small to high values

% then we check if we have odd or even number of elements
nelt = numel(Vec);
if mod(nelt,2)==0 % even number of elements
    % in this case, the median is the mean value of the 2 middle values
    med = (V2(nelt/2)+V2(nelt/2+1))/2;
else
    % in this case, the median is the middle value
    med = V2(ceil(nelt/2));
end
fprintf(1,'\n');
fprintf(1, 'Median: sort with if gives: %f\n', med);

med = median(Vec);
fprintf(1, 'Median: median gives: %f\n', median(Vec));
