function [h, H] = rct (t,f,beta)
% In optical communication systems, different linear pulse-shaping filters 
% are used to generate continuous-time transmit signals. We can define 
% these filters either through their impulse response hp(t) or their 
% frequency response Hp( f )=F{hp(t)}, where F{ } stands for the Fourier
% transform.

% rct: Raised-cosine time domain (RCT-Filter)
% Time Domain
h = (1 + cos((pi./beta).*(abs(t)-(1-beta)/2)))./2;
h(abs(t)>(1+beta)/2) = 0;
h(abs(t)<(1-beta)/2) = 1;
h(abs(t)==1/2) = 1/2;

% Frequency Domain
% H = fftshift(fft(h),1);
H = sinc(f).*cos(pi.*beta.*f)./(1-(2*beta.*f).^2);
H(f == -0.5/beta) = sinc(1/(2*beta))*pi/4 ;
H(f == +0.5/beta) = sinc(1/(2*beta))*pi/4;
end