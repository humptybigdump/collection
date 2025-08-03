% The sum of the squares of all components of the vector Vec

Vec = rand(1,2500);

sum=0; % initialize the sum of the squares to zero 

% loop over all elements of the vector
for ii=1:numel(Vec)
    sum=Vec(ii)*Vec(ii)+sum; % update the sum from new vector element value
end

% show result on screen
disp(sum);