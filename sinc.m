function y = sinc(x)
%sinc Sinc function
% y = sinc(x) returns an array, y, whose elements are the sinc of the
% elements of the input, x. The output y is the same size as x.
% 
% Karlsruhe Institute of Technology
% ETIT Faculty
% Lecture: Digital Signal Processing in Optical Communications (DSPO)
% Written by: Jonas Krimmer
% Institute of Photonics and Quantum Electronics
% Last edit 27.04.2020 by Jonas Krimmer
%

xp = pi*x;
mask = xp == 0;

y = sin(xp) ./ (xp .* ~mask + mask) + mask;

end