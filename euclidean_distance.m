function [d] = euclidean_distance(u, v)
if (size(u) == size(v))
    d = sum((u-v).^2);    
    d = sqrt(d);
else
    error('The vector dimensions do not match!');
end