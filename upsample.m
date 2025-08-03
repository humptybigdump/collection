function y = upsample(x, n)
%upsample Increase sample rate by integer factor
% y = upsample(x,n) increases the sample rate of the vector x by inserting
% n – 1 zeros between consecutive samples.
% 
% Karlsruhe Institute of Technology
% ETIT Faculty
% Lecture: Digital Signal Processing in Optical Communications (DSPO)
% Written by: Jonas Krimmer
% Institute of Photonics and Quantum Electronics
% Last edit 27.04.2020 by Jonas Krimmer
% 
assert(isvector(x));

if isrow(x)
    y = zeros(1, n*length(x));
else % iscolumn(x)
    y = zeros(n*length(x), 1);
end

y(1:n:end) = x;

end