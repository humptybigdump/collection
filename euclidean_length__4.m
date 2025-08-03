function [ l ] = euclidean_length( x )
    sum = 0;
    for i=1:length(x)        
        squared = x(i)^2;
        sum = sum + squared;
    end
    
    l = sqrt(sum);
end

