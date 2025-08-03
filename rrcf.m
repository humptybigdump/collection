function [h, H] = rrcf(t,f,beta)
%%rrcf Root Raised cosine frequency response (RRCF-Filter) 
% In optical communication systems, different linear pulse-shaping filters 
% are used to generate continuous-time transmit signals. We can define 
% these filters either through their impulse response hp(t) or their 
% frequency response Hp( f )=F{hp(t)}, where F{ } stands for the Fourier
% transform.
% 
% Karlsruhe Institute of Technology
% ETIT Faculty
% Lecture: Digital Signal Processing in Optical Communications (DSPO)
% Written by: ?
% Institute of Photonics and Quantum Electronics
% Last edit 13.05.2020 by Jonas Krimmer
% 
% 

% Time Domain
beta = beta.*ones(size(t));
case0       = (t == 0*beta);
case14b     = (abs(t) == 1./(4*beta));
h           = (sin(pi*t.*(1-beta))+4*beta.*t.*cos(pi*t.*(1+beta)))./(pi*t.*(1-(4*beta.*t).^2));
h(case0)    = 1 - beta(case0)*(1 - 4/pi);
h(case14b)  = beta(case14b)/sqrt(2).*((1+2/pi)*sin(pi./(4*beta(case14b)))+(1-2/pi)*cos(pi./(4*beta(case14b))));

% Frequency Domain
H                     = sqrt(abs(((1 + cos((pi./beta).*(abs(f)-(1-beta)/2)))./2)));
H(abs(f)>(1+beta)/2)  = 0;
H(abs(f)<(1-beta)/2)  = 1;
H(abs(f)==1/2)        = 1/sqrt(2);

end