function s2 = sumsq(Vec)

s2=0;
for ii=1:numel(Vec)
    s2=s2+Vec(ii)^2;
end
fprintf(1,'Sum of the squares of each element of Vec is: %f\n', s2);