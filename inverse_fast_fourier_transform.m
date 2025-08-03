function [signal]=inverse_fast_fourier_transform(SIGNAL,dt);

% Notice: 
% We set the Fourier coefficient at 0 Hz to zero to avoid NaN.
% 
%
% Input:
%	SIGNAL	:	Vector containing the Fourier coefficients
%	dt	:	Sampling rate in s
%
% Output:
%	signal	:	Vector after inverse Fourier transformation
%
% -----
%
% Lisa Groos, 2013
%

SIGNAL_shifted=(1/dt)*ifftshift(SIGNAL);
SIGNAL_shifted(1)=0;

signal=ifft(SIGNAL_shifted);
	
