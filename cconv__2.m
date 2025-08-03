function y = cconv(x,c,dim,N)
%cconv Circular convolution
% y = cconv(x,c[,dim,N]) returns an array, y, whose elements are the
% circular convolution of the input arrays x and c along the dimensions
% dim. If dim is not provided, the circular convolution is
% computed over all occurring dimensions. The output y has size N (if
% provided) or the dimensionwise maximum of x and c.
%
% Karlsruhe Institute of Technology
% ETIT Faculty
% Lecture: Digital Signal Processing in Optical Communications (DSPO)
% Written by: Jonas Krimmer
% Institute of Photonics and Quantum Electronics
% Last edit 08.05.2020 by Jonas Krimmer
%

if nargin < 3
    maxdim = max(ndims(c), ndims(x));
    dim = 1:maxdim;
    if verLessThan('matlab','9.7')
        N = zeros(1, maxdim);
        for k = 1:maxdim
            N(k) = max(size(x, k), size(c, k));
        end
    else
        N = max(size(x, dim), size(c, dim));
    end
elseif nargin < 4
    if verLessThan('matlab','9.7')
        N = ones(1, length(dim));
        for k = 1:length(dim)
            N(k) = max(size(x, dim(k)), size(c, dim(k)));
        end
    else
        N = max(size(x, dim), size(c, dim));
    end
else
    if isscalar(N)
        if isrow(x) && isrow(c)
            N = [1 N];
        elseif iscolumn(x) && iscolumn(c)
            N = [N 1];
        end
    end
end


X = fft(x,N(1),dim(1));
C = fft(c,N(1),dim(1));
for k = 2:length(dim)
    X = fft(X,N(k),dim(k));
    C = fft(C,N(k),dim(k));
end
Y = X.*C;
y = ifft(Y,[],dim(1));
for k = 2:length(dim)
    y = ifft(y,[],dim(k));
end

end