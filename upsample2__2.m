function y = upsample2(x, n, phase)
%upsample Increase sample rate by integer factor
% y = upsample(x,n) increases the sample rate of the vector x by inserting
% n – 1 zeros between consecutive samples. If desired, apply an additional
% phase offset. If x is a matrix, treat the columns as independent signal
% vectors and upsample columnwise.
% 
% Karlsruhe Institute of Technology
% ETIT Faculty
% Lecture: Digital Signal Processing in Optical Communications (DSPO)
% Written by: Jonas Krimmer
% Institute of Photonics and Quantum Electronics
% Last edit 25.05.2020 by Jonas Krimmer
% 

if nargin < 3
    phase = 0;
end

if isscalar(x)
    
    y = zeros(n, 1);
    y(1:n:end) = x;
    y = circshift(y, phase);

elseif isvector(x)

    if isrow(x)
        y = zeros(1, n*length(x));
    else % iscolumn(x)
        y = zeros(n*length(x), 1);
    end

    y(1:n:end) = x;
    y = circshift(y, phase);
    
elseif ismatrix(x)
    
    y = zeros(n*size(x,1), size(x,2));
    y(1:n:end,:) = x;
    
    y = circshift(y, phase, 1);
else
    
    error("x has to be either a matrix or a vector.")
    
end
    
    

end