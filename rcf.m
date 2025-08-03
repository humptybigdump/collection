function [h, H] = rcf (t,f,beta)
% In optical communication systems, different linear pulse-shaping filters 
% are used to generate continuous-time transmit signals. We can define 
% these filters either through their impulse response hp(t) or their 
% frequency response Hp( f )=F{hp(t)}, where F{ } stands for the Fourier
% transform.

% rcf: Raised cosine Filter (RCF)
% Time Domain
h = sinc(t).*cos(pi*beta*t)./(1-(2*beta*t).^2);
h (t == -0.5/beta) = pi/4 * sinc(1/(2*beta)) ;
h (t == +0.5/beta) = pi/4 * sinc(1/(2*beta)) ;

% Frequency Domain
H = (1 + cos((pi./beta).*(abs(f)-(1-beta)/2)))./2;
H(abs(f)>(1+beta)/2) = 0;
H(abs(f)<(1-beta)/2) = 1;
H(abs(f)==1/2)=1/2;

end