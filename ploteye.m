function ploteye(y, F, dly, varargin)
%ploteye Creates an eye diagram of the signal vector/matrix y. 
% If y is a matrix, assume that each column of y contains one distinct
% signal.
% 
% INPUT:
%   y       : Signal vector/matrix.
%   F       : Oversampling rate of the signal, i.e., no. samples / symbol
%   dly     : Optional additional delay
%   varargin: Arbitrary argument list, directly passed to plot
% 
%
% Karlsruhe Institute of Technology
% ETIT Faculty
% Lecture: Digital Signal Processing in Optical Communications (DSPOC)
% Written by: Juned Kemal, Adib Hossaine, Sebastian Randel
% Institute of Photonics and Quantum Electronics
% Last edit 28.04.2020 by Jonas Krimmer (IPQ)
%

if nargin < 3
    dly = 0;
end

[M,N] = size(y);

% make sure that input vectors are divisible by F
no_missing_samples = F - rem(M, F);
y = [y; nan(no_missing_samples, N)];


y = circshift(y, dly);
% generate the eye for one symbol, third dimension contains different
% signals
eye = reshape(y, F, [], N);
% concatenate such that first symbol is followed by second symbol. Add
% first value of third symbol and finish with a nan to ensure there is a
% gap before the ensuing symbol.
eye = [eye; circshift(eye,-1,2); circshift(eye(1,:,:),-2,2); nan(1, size(eye, 2), N)];


x = repmat([-F:F NaN], 1, size(eye, 2));


plot(x, reshape(eye, [], N), varargin{:})
grid on;
box on;
xlabel('Time [Samples]')
ylabel('Amplitude [a.u.]')
title('Eye Diagram')

end